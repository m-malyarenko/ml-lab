library(mlbench)
library(rpart)

data("Glass")
data <- Glass
n <- nrow(data)

train_idx <- sample(1:n, 0.7 * n)
test_idx <- seq(1, n)[-train_idx]
tree_count_values <- seq(1, 201, by = 10)

bagging_err_values <- c()

for (tree_count in tree_count_values) {
  bagging_model <- bagging(
    Type ~ .,
    data = data[train_idx, ],
    mfinal = tree_count,
    maxdepth = depth_limit
  )
  
  bagging_pred <- predict(
    bagging_model,
    newdata = data[test_idx, ]
  )
  
  bagging_pred_table <- table(bagging_pred$class, data$Type[test_idx])
  bagging_err <- 1 - (sum(diag(bagging_pred_table)) / sum(bagging_pred_table))
  bagging_err_values <- append(bagging_err_values, bagging_err)
}

regression <- lm(bagging_err_values[-1] ~ tree_count_values[-1])

plot(
  x = tree_count_values,
  y = bagging_err_values,
  
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