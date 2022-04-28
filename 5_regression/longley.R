library(MASS)

data("longley")
data <- subset(longley, select = -Population)

n <- nrow(longley)
# set.seed(128)
data <- data[order(runif(n)), ]

data_train <- data[1:(n / 2), ]
data_test <- data[(n / 2 + 1):n, ]

# RMSE calculation

rmse <- function(observ, pred) {
  sse <- sum((pred - observ)^2)
  
  return(sqrt(sse / length(observ)))
}

# Ridge regression
lambda_values <- 10^(-3 + 0.2 * seq(0, 25))
ridge_train_rmse <- c()
ridge_test_rmse <- c()

for (lambda in lambda_values) {
  ridge <- glmnet(
    x = data.matrix(subset(data_train, select = -Employed)),
    y = data_train$Employed,
    alpha = 0,
    lambda = lambda
  )
  
  ridge_fitted <- predict(
    ridge,
    s = lambda,
    newx = data.matrix(subset(data_train, select = -Employed))
  )
  
  ridge_train_rmse <- append(
    ridge_train_rmse,
    rmse(data_train$Employed, ridge_fitted)
  )
  
  ridge_fitted <- predict(
    ridge,
    s = lambda,
    newx = data.matrix(subset(data_test, select = -Employed))
  )
  
  ridge_test_rmse <- append(
    ridge_test_rmse,
    rmse(data_test$Employed, ridge_fitted)
  )
}

plot(
  x = lambda_values,
  log = "x",
  y = ridge_train_rmse,
  
  pch = 20,
  col = "blue",
  type = "b",
  
  xlab = "λ",
  ylab = "RMSE",
  main = paste(
    "Зависимость RMSE регрессии на тренировойчной выборке", '\n',
    "от штрафного параметра λ"
  )
)
plot(
  x = lambda_values,
  log = "x",
  y = ridge_test_rmse,
  
  pch = 20,
  col = "blue",
  type = "b",
  
  xlab = "λ",
  ylab = "RMSE",
  main = paste(
    "Зависимость RMSE регрессии на тестовой выборке", '\n',
    "от штрафного параметра λ"
  )
)
