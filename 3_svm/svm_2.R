library(e1071)

data_train <- read.table(
  "data/svmdata2.txt",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = TRUE
)

data_test <- read.table(
  "data/svmdata2test.txt",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = TRUE
)

# Raw data scatter plot
plot(
  x = data_train$X1,
  y = data_train$X2,
  
  col = as.character(data_train$Color),
  type = "p",
  pch = 19,
  
  xlab = "X1",
  ylab = "X2",
  main = "Тренировочная выборка"
)

plot(
  x = data_test$X1,
  y = data_test$X2,
  
  col = as.character(data_test$Color),
  type = "p",
  pch = 19,
  
  xlab = "X1",
  ylab = "X2",
  main = "Тестовая выборка"
)

# Find optimal C for train data
cat("Train data\n")
train_accuracy <- c()

for (c in 1:30) {
  svm_model <- svm(
    Color ~ X1 + X2,
    data = data_train,
    type = "C-classification",
    cost = c,
    kernel = "linear"
  )
  
  pred <- predict(svm_model, data_train)
  current_accuracy <- sum(pred == data_train$Color) / nrow(data_train) * 100
  train_accuracy <- append(train_accuracy, current_accuracy)
  
  cat("C =", c, "Accuracy:", current_accuracy, "% \n")
}

optimal_train_c <- which.max(train_accuracy)
cat("Optimal C for train data: ", optimal_train_c, "\n")

svm_model <- svm(
  Color ~ X1 + X2,
  data = data_train,
  type = "C-classification",
  cost = optimal_train_c,
  kernel = "linear"
)

plot(
  svm_model,
  data_train,
  
  grid = 100,
  dataSymbol = '●',
  svSymbol = 'x',
  symbolPalette = c("green", "red"),
)

# Find optimal C for test data
cat("Test data\n")
test_accuracy <- c()

for (c in 1:30) {
  svm_model <- svm(
    Color ~ X1 + X2,
    data = data_train,
    type = "C-classification",
    cost = c,
    kernel = "linear"
  )
  
  pred <- predict(svm_model, data_test)
  current_accuracy <- sum(pred == data_test$Color) / nrow(data_test) * 100
  test_accuracy <- append(test_accuracy, current_accuracy)
  
  cat("C =", c, "Accuracy:", current_accuracy, "% \n")
}

optimal_test_c <- which.max(test_accuracy)
cat("Optimal C for train data: ", optimal_test_c, "\n")

svm_model <- svm(
  Color ~ X1 + X2,
  data = data_train,
  type = "C-classification",
  cost = optimal_test_c,
  kernel = "linear"
)

plot(
  svm_model,
  data_test,
  
  grid = 100,
  dataSymbol = '●',
  svSymbol = 'x',
  symbolPalette = c("green", "red"),
)

