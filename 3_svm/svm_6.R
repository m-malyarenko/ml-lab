library(e1071)

data <- read.table(
  "data/svmdata6.txt",
  sep = "\t",
  header = TRUE,
)

# Raw data scatter plot
plot(
  x = data$X,
  y = data$Y,
  
  col = "red",
  type = "p",
  pch = 20,
  
  xlab = "X",
  ylab = "Y",
  main = "Исходные данные"
)

# Parameters for SVM models
c <- 1
epsilon_values <- seq(0, 1.6, 0.05)

# Regression task
svm_regression <- svm(
  x = as.double(data$X),
  y = as.double(data$Y),
  type = "eps-regression",
  kernel = "radial",
  epsilon = 0.25,
  cost = c,
)

pred <- predict(svm_regression, data$X)

plot( # all data points
  x = data$X,
  y = data$Y,
  
  col = "red",
  type = "p",
  pch = 20,
  
  xlab = "X",
  ylab = "Y",
  main = "Результаты ε-регрессии, ε = 0.25",
)
points( # all support vectors
  x = data$X[svm_regression$index],
  y = data$Y[svm_regression$index],
  
  col = "blue",
  type = "p",
  pch = 15,
)
lines( # regeression
  x = data$X,
  y = pred,
  col = "blue",
  lwd = 2,
)
lines( # regression +error
  x = data$X,
  y = pred + svm_regression$epsilon,
  col = "cyan",
)
lines( # regression -error
  x = data$X,
  y = pred - svm_regression$epsilon,
  col = "cyan",
)

# MSE against ε regression parameter
svm_rmse <- c()

for (epsilon in epsilon_values) {
  svm_regression <- svm(
    x = as.double(data$X),
    y = as.double(data$Y),
    type = "eps-regression",
    kernel = "radial",
    epsilon = epsilon,
    cost = c,
  )
  
  pred <- predict(svm_regression, data$X)
  residual <- c(pred -  data$X)
  svm_rmse <- append(sqrt(mean(residual^2)), svm_rmse)
}

plot(
  x = epsilon_values,
  y = svm_rmse,
  
  col = "blue",
  type = "l",
  
  xlab = "ε",
  ylab = "RMSE",
  main = "Зависимость RMSE от параметра регрессии ε",
)
