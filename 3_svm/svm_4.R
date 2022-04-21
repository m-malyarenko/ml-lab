library(e1071)

data_train <- read.table(
  "data/svmdata4.txt",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = TRUE
)

data_test <- read.table(
  "data/svmdata4test.txt",
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

# Radial kernel
c_values <- seq(1,30, 1)
accuracy <- c()

for (c in c_values) {
  svm_classifier <- svm(
    Color ~ X1 + X2,
    data = data_train,
    type = "C-classification",
    cost = c,
    kernel = "radial"
  )
  
  pred <- predict(svm_classifier, data_test)
  current_accuracy <- sum(pred == data_test$Color) / nrow(data_test) * 100
  accuracy <- append(accuracy, current_accuracy)
}

plot(
  x = c_values,
  y = accuracy,
  
  col = "blue",
  type = "l",
  
  xlab = "C",
  ylab = "Точность классификации",
  main = "Зависимость точности классифкации от C\nЯдро Radial"
)

optimal_c <- which.max(accuracy)
cat("Optimal C for test data: ", optimal_c, "\n")
cat("Radial kernel best accuracy on test data:", max(accuracy), "\n")

svm_classifier <- svm(
  Color ~ X1 + X2,
  data = data_train,
  type = "C-classification",
  cost = optimal_c,
  kernel = "radial"
)

plot(
  svm_classifier,
  data_test,
  
  grid = 100,
  dataSymbol = '●',
  svSymbol = 'x',
  symbolPalette = c("green", "red"),
)

# Sigmoid kernel
c_values <- seq(1,10,0.05)
accuracy <- c()

for (c in c_values) {
  svm_classifier <- svm(
    Color ~ X1 + X2,
    data = data_train,
    type = "C-classification",
    cost = c,
    kernel = "sigmoid"
  )
  
  pred <- predict(svm_classifier, data_test)
  current_accuracy <- sum(pred == data_test$Color) / nrow(data_test) * 100
  accuracy <- append(accuracy, current_accuracy)
}

plot(
  x = c_values,
  y = accuracy,
  
  col = "blue",
  type = "l",
  
  xlab = "C",
  ylab = "Точность классификации",
  main = "Зависимость точности классифкации от C\nЯдро Sigmoid"
)

optimal_c <- which.max(accuracy)
cat("Optimal C for test data: ", optimal_c, "\n")
cat("Sigmoid kernel best accuracy on test data:", max(accuracy), "\n")

svm_classifier <- svm(
  Color ~ X1 + X2,
  data = data_train,
  type = "C-classification",
  cost = optimal_c,
  kernel = "sigmoid"
)

plot(
  svm_classifier,
  data_test,
  
  grid = 100,
  dataSymbol = '●',
  svSymbol = 'x',
  symbolPalette = c("green", "red"),
)

# Polynomial kernel
degree_values <- seq(2, 10, 1)
c_values <- seq(1,50, 1)

for (degree in degree_values) {
  accuracy <- c()
  
  for (c in c_values) {
    svm_classifier <- svm(
      Color ~ X1 + X2,
      data = data_train,
      type = "C-classification",
      cost = c,
      kernel = "polynomial",
      degree = degree
    )
    
    pred <- predict(svm_classifier, data_test)
    current_accuracy <- sum(pred == data_test$Color) / nrow(data_test) * 100
    accuracy <- append(accuracy, current_accuracy)
  }
  
  plot(
    x = c_values,
    y = accuracy,
    
    col = "blue",
    type = "l",
    
    xlab = "C",
    ylab = "Точность классификации",
    main = paste(
      "Зависимость точности классифкации от C\nЯдро polinomial",
      degree)
  )
  
  cat("Optimal C for degree", degree, "is", which.max(accuracy), "\n")
  cat("Polinomial", degree, "kernel best accuracy:", max(accuracy), "\n")
  
  svm_classifier <- svm(
    Color ~ X1 + X2,
    data = data_train,
    type = "C-classification",
    cost = which.max(accuracy),
    kernel = "polynomial",
    degree = degree
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
