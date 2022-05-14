library(e1071)

data_train <- read.table(
  "data/svmdata1.txt",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = TRUE
)

data_test <- read.table(
  "data/svmdata1test.txt",
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
  main = "Тесовая выборка"
)

svm_model <- svm(
  Color ~ X1 + X2,
  data = data_train,
  type = "C-classification",
  cost = 1,
  kernel = "linear"
)

plot(
  svm_model,
  data_train,

  grid = 100,
  dataSymbol = '●',
  svSymbol = 'x',
  symbolPalette = c("green", "red"),
  color.pallete = c("PaleGreen", "PaleTurquoise")
)

pred_train <- predict(svm_model, data_train)
pred_train_err_idx <- pred_test[pred_test != data_train$Color]
pred_train_accuracy <- sum(pred_train == data_train$Color) / nrow(data_train) * 100

pred_test <- predict(svm_model, data_test)
pred_test_err_idx <- pred_test[pred_test != data_test$Color]
pred_test_accuracy <- sum(pred_test == data_test$Color) / nrow(data_test) * 100

cat("Number of SV:", nrow(svm_model$SV), "\n")
cat("Train data classification error points:", pred_train_err_idx, "\n")
cat("Train data classification accuracy:", pred_train_accuracy, "% \n")
cat("Test data classification error points:", pred_test_err_idx, "\n")
cat("Test data classification accuracy:", pred_test_accuracy, "% \n")
