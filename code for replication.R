# Install necessary packages if not already installed
if (!require("quantregForest")) install.packages("quantregForest")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("randomForest")) install.packages("randomForest")
if (!require("RColorBrewer")) install.packages("RColorBrewer")

library(randomForest)
library(RColorBrewer)
library(quantregForest)
library(ggplot2)

# Load the data
data(BHousing)

# Remove rows with missing values
BHousing <- BHousing[complete.cases(BHousing), ]

# Set seed for reproducibility
set.seed(1)

# Create an index for the training set
n <- nrow(BHousing)
indextrain <- sample(1:n, size = round(0.6 * n), replace = FALSE)

# Define training and testing sets
Xtrain <- as.matrix(BHousing[indextrain, 2:14])
Xtest <- as.matrix(BHousing[-indextrain, 2:14])
Ytrain <- as.matrix(BHousing[indextrain, 1])
Ytest <- as.matrix(BHousing[-indextrain, 1])

# Train quantile regression forest
qrf <- quantregForest(x = Xtrain, y = Ytrain)

# Define quantiles
quantiles = c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995)

# Predict conditional quantiles
conditionalQuantiles <- predict(qrf, Xtest, what = quantiles)

# Define the weighted absolute deviation loss function
weighted_abs_loss <- function(y, q, alpha) {
  ifelse(y > q, alpha * abs(y - q), (1 - alpha) * abs(y - q))
}

# Calculate average losses for QRF
qrf_losses <- sapply(1:length(quantiles), function(i) {
  alpha <- quantiles[i]
  mean(weighted_abs_loss(Ytest, conditionalQuantiles[, i], alpha))
})

# Create a data frame for plotting
plot_data <- data.frame(
  Quantile = quantiles,
  Loss = qrf_losses
)

# Plot using ggplot2
ggplot(plot_data, aes(x = as.factor(Quantile), y = Loss)) +
  geom_point(color = "blue") +
  geom_line(color = "blue", group = 1) +
  labs(
    x = "Quantiles",
    y = "Average Loss",
    title = "Average Loss for Quantile Regression Forest"
  ) +
  theme_minimal()
