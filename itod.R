require("XLConnect")
require("data.table")
require("caret")
lab1 <- function(path) {
  #"/home/tanya/Downloads/BDA/itod/Classification_test.xls"
  wb <- loadWorkbook(path, create = FALSE)
  df1 <- readWorksheet(wb, sheet = "Sheet1", header = FALSE)
  df1 <- df1[, -c(2)]
  
  df1$Col1[df1$Col1 == -1] <- 0
  
  inputPositive <- df1[which(df1$Col1 == 1), ]
  inputNegative <- df1[which(df1$Col1 == 0), ]
  
  inputPositiveTrainingRows <- sample(1:nrow(inputPositive), 0.8 * nrow(inputPositive))
  inputNegativeTrainingRows <- sample(1:nrow(inputNegative), 0.8 * nrow(inputNegative))
  
  trainingPositives <- inputPositive[inputPositiveTrainingRows, ]
  trainingNegatives <- inputNegative[inputNegativeTrainingRows, ]
  trainingData <- rbind(trainingPositives, trainingNegatives)
  
  testPositives <- inputPositive[-inputPositiveTrainingRows, ]
  testNegatives <- inputNegative[-inputNegativeTrainingRows, ]
  testData <- rbind(testPositives, testNegatives)
  
  y <- trainingData[, 1]
  x <- trainingData[, 2:ncol(trainingData)]
  modFit <- glm(y ~ data.matrix(x), family = binomial("logit"))
  
  x <- testData[, 2:ncol(testData)]
  predictions <- predict(modFit, newdata = x, type = "response")
  optCutOff <- optimalCutoff(testData$Col1, predictions)
  confMatrix <- confusionMatrix(testData$Col1, predictions, threshold = optCutOff)
  plotROC(testData$Col1, predictions)
  print(confMatrix)
  
  diagMatrix <- diag(as.matrix(confMatrix))
  rowsSum <- apply(confMatrix, 1, sum)
  colsSum <- apply(confMatrix, 2, sum)
  
  precision = diagMatrix / colsSum 
  recall = diagMatrix / rowsSum 
  f1 = 2 * precision * recall / (precision + recall) 
  return( data.frame(precision, recall, f1) )
}
