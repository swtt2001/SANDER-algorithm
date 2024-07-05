install.packages("ggplot2")
library(ggplot2)
install.packages("Matrix")
library(Matrix)
install.packages("arules")
library(arules)
install.packages("openxlsx")
library(openxlsx)

install.packages("quantregForest")
install.packages("ggplot2")
library(randomForest)
library(RColorBrewer)
library(quantregForest)
library(ggplot2)


df <- ClusterFinalDataset

# Print summary statistics
#summary_stats <- summary(df)
#print(summary_stats)

# Print correlation matrix
# Select only numeric columns for correlation matrix
#numeric_columns <- df[sapply(df, is.numeric)]
#correlation_matrix <- cor(numeric_columns, use = "complete.obs")
#print(correlation_matrix)

# Create an index for the training set
n <- nrow(df)
indextrain <- sample(1:n, size = round(0.8 * n), replace = FALSE)

# List of columns to exclude for QRF-FD
columns_to_exclude <- c('id', 'Cluster', 'two_year_recid', 'decile_score', 'decile_score.1')

#For SANDER
#columns_to_exclude <- c('id', 'two_year_recid', 'decile_score', 'decile_score.1')

# Create Xtrain with all columns except those in columns_to_exclude
Xtrain <- as.matrix(df[indextrain, !names(df) %in% columns_to_exclude])
Ytrain <- as.matrix(df[indextrain, "decile_score.1"])
Xtest <- as.matrix(df[-indextrain, !names(df) %in% columns_to_exclude])
Ytest <- as.matrix(df[-indextrain, "decile_score.1"] )


#quant rf
qrf <- quantregForest(x= Xtrain, y = Ytrain)

test <- IF_test
Xtest <- as.matrix(test[, !names(test) %in% columns_to_exclude])


conditionalQuantiles <- predict(qrf, Xtest, what=c(0.05, 0.5, 0.95))



Xtest_full <- df[-indextrain,]

# Create a dataframe combining Xtest, conditionalQuantiles, and possibly Ytest
# (if Ytest is available and you want to include it)
combined_df3 <- cbind(Xtest_full, conditionalQuantiles)

# Write the combined dataframe to an Excel file
write.xlsx(combined_df3, file = "QRF_final.xlsx")















