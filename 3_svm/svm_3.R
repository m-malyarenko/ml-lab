library(e1071)

data <- read.table(
  "data/svmdata3.txt",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = TRUE
)

# Raw data scatter plot
plot(
  x = data$X1,
  y = data$X2,
  
  col = as.character(data$Color),
  type = "p",
  pch = 19,
  
  xlab = "X1",
  ylab = "X2",
  main = "Тренировочная выборка"
)

# Radial kernel
c_values <- seq(1,50, 1)
accuracy <- c()

for (c in c_values) {
  svm_model <- svm(
    Color ~ X1 + X2,
    data = data,
    type = "C-classification",
    cost = c,
    kernel = "radial"
  )
  
  pred <- predict(svm_model, data)
  current_accuracy <- sum(pred == data$Color) / nrow(data) * 100
  accuracy <- append(accuracy, current_accuracy)
  
  # cat("C =", c, "Accuracy:", current_accuracy, "% \n")
}

plot(
  x = c_values,
  y = accuracy,
  
  col = "blue",
  type = "l",
  
  xlab = "C",
  ylab = "Точность классификации",
  main = "Зависимость точности классифкации от C\nЯдро radial"
)

optimal_c <- which.max(accuracy)
cat("Optimal C for train data: ", optimal_c, "\n")
cat("Radial kernel best accuracy:", max(accuracy), "\n")

svm_model <- svm(
  Color ~ X1 + X2,
  data = data,
  type = "C-classification",
  cost = optimal_c,
  kernel = "radial"
)

plot(
  svm_model,
  data,
  
  grid = 100,
  dataSymbol = '●',
  svSymbol = 'x',
  symbolPalette = c("green", "red"),
)

# Sigmoid kernel
c_values <- seq(1,10,0.05)
accuracy <- c()

for (c in c_values) {
  svm_model <- svm(
    Color ~ X1 + X2,
    data = data,
    type = "C-classification",
    cost = c,
    kernel = "sigmoid"
  )
  
  pred <- predict(svm_model, data)
  current_accuracy <- sum(pred == data$Color) / nrow(data) * 100
  accuracy <- append(accuracy, current_accuracy)
  
  # cat("C =", c, "Accuracy:", current_accuracy, "% \n")
}

plot(
  x = c_values,
  y = accuracy,
  
  col = "blue",
  type = "l",
  
  xlab = "C",
  ylab = "Точность классификации",
  main = "Зависимость точности классифкации от C\nЯдро sigmoid"
)

optimal_c <- which.max(accuracy)
cat("Optimal C for train data: ", optimal_c, "\n")
cat("Sigmoid kernel best accuracy:", max(accuracy), "\n")

svm_model <- svm(
  Color ~ X1 + X2,
  data = data,
  type = "C-classification",
  cost = optimal_c,
  kernel = "sigmoid"
)

plot(
  svm_model,
  data,
  
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
    svm_model <- svm(
      Color ~ X1 + X2,
      data = data,
      type = "C-classification",
      cost = c,
      kernel = "polynomial",
      degree = degree
    )
    
    pred <- predict(svm_model, data)
    current_accuracy <- sum(pred == data$Color) / nrow(data) * 100
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
  
  svm_model <- svm(
    Color ~ X1 + X2,
    data = data,
    type = "C-classification",
    cost = which.max(accuracy),
    kernel = "polynomial",
    degree = degree
  )
  
  plot(
    svm_model,
    data,
    
    grid = 100,
    dataSymbol = '●',
    svSymbol = 'x',
    symbolPalette = c("green", "red"),
  )
}
