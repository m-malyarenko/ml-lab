library(mlbench)
library(adabag)
library(rpart)

data("Vehicle")
data <- Vehicle
n <- nrow(data)

train_idx <- sample(1:n, 0.7 * n)
tree_count_values <- seq(1, 301, by = 10)
boost_err_values <- c()

for (tree_count in tree_count_values) {
  boost_model <- boosting(
    Class ~ .,
    data = data[train_idx, ],
    mfinal = tree_count,
    maxdepth = depth_limit
  )
  
  boost_pred <- predict(
    boost_model,
    newdata = data[-train_idx, ]
  )
  
  boost_err_values <- append(boost_err_values, boost_pred$error)
}

err_data <- data.frame(
  TreeCount = tree_count_values,
  Err = 
)
regression <- lm(boost_err_values[-1] ~ tree_count_values[-1])

plot(
  x = tree_count_values,
  y = boost_err_values,
  
  xlab = "Количество деревьев",
  ylab = "Ошибка классификации ансамбля",
  main = "Зависимость тестовой ошибки от количества деревьев в ансамбле",
  
  type = "b",
  pch = 19,
  col = "blue"
)
lines(
  x = tree_count_values[-1],
  y = regression$fitted.values,
  
  col = "blue"
)
