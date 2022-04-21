library(e1071)

data_train <- read.table(
  "data/svmdata5.txt",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = TRUE
)

data_test <- read.table(
  "data/svmdata5test.txt",
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

# Parameters for SVM models
c <- 10
gamma_values <- 10^seq(-3, 2) # 0.001 0.01, 0.1, 1, 10, 100

# Research setup
result_colnames <- c("Gamma", "Accuracy")
result_table <- data.frame(matrix(nrow = 0, ncol = length(result_colnames)))
colnames(result_table) = result_colnames

# Radial kernel
accuracy <- c()

for (gamma in gamma_values) {
  svm_classifier <- svm(
    Color ~ X1 + X2,
    data = data_train,
    type = "C-classification",
    kernel = "radial",
    gamma = gamma,
    cost = c,
  )
  
  pred <- predict(svm_classifier, data_test)
  current_accuracy <- sum(pred == data_test$Color) / nrow(data_test) * 100
  accuracy <- append(accuracy, current_accuracy)
  
  result_row = data.frame(Gamma = gamma, Accuracy = current_accuracy)
  result_table <- rbind(result_table, result_row)
  
  plot(
    svm_classifier,
    data_train,
    
    grid = 100,
    dataSymbol = '●',
    svSymbol = 'x',
    symbolPalette = c("green", "red"),
  )
  
  plot(
    svm_classifier,
    data_test,
    
    grid = 100,
    dataSymbol = '●',
    svSymbol = 'x',
    symbolPalette = c("green", "red"),
  )
}

print(result_table)

optimal_gamma <- gamma_values[which.max(accuracy)]
cat("Optimal Gamma for test data: ", optimal_gamma, "\n")
cat("Radial kernel best accuracy on test data:", max(accuracy), "\n")

# Sigmoid kernel
result_table <- result_table[-c(1:nrow(result_table)),]
accuracy <- c()

for (gamma in gamma_values) {
  svm_classifier <- svm(
    Color ~ X1 + X2,
    data = data_train,
    type = "C-classification",
    kernel = "sigmoid",
    gamma = gamma,
    cost = c,
  )
  
  pred <- predict(svm_classifier, data_test)
  current_accuracy <- sum(pred == data_test$Color) / nrow(data_test) * 100
  accuracy <- append(accuracy, current_accuracy)
  
  result_row = data.frame(Gamma = gamma, Accuracy = current_accuracy)
  result_table <- rbind(result_table, result_row)
  
  plot(
    svm_classifier,
    data_train,
    
    grid = 100,
    dataSymbol = '●',
    svSymbol = 'x',
    symbolPalette = c("green", "red"),
  )
  
  plot(
    svm_classifier,
    data_test,
    
    grid = 100,
    dataSymbol = '●',
    svSymbol = 'x',
    symbolPalette = c("green", "red"),
  )
}

print(result_table)

optimal_gamma <- gamma_values[which.max(accuracy)]
cat("Optimal Gamma for test data: ", optimal_gamma, "\n")
cat("Sigmoid kernel best accuracy on test data:", max(accuracy), "\n")

# Polynomial kernel
result_table <- result_table[-c(1:nrow(result_table)),]
accuracy <- c()

for (gamma in gamma_values) {
  svm_classifier <- svm(
    Color ~ X1 + X2,
    data = data_train,
    type = "C-classification",
    kernel = "polynomial",
    degree = 2,
    gamma = gamma,
    cost = c,
  )
  
  pred <- predict(svm_classifier, data_test)
  current_accuracy <- sum(pred == data_test$Color) / nrow(data_test) * 100
  accuracy <- append(accuracy, current_accuracy)
  
  result_row = data.frame(Gamma = gamma, Accuracy = current_accuracy)
  result_table <- rbind(result_table, result_row)
  
  plot(
    svm_classifier,
    data_train,
    
    grid = 100,
    dataSymbol = '●',
    svSymbol = 'x',
    symbolPalette = c("green", "red"),
  )
  
  plot(
    svm_classifier,
    data_test,
    
    grid = 100,
    dataSymbol = '●',
    svSymbol = 'x',
    symbolPalette = c("green", "red"),
  )
}

print(result_table)

optimal_gamma <- gamma_values[which.max(accuracy)]
cat("Optimal Gamma for test data: ", optimal_gamma, "\n")
cat("Polynomial kernel best accuracy on test data:", max(accuracy), "\n")