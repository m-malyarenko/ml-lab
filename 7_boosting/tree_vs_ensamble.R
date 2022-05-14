library(adabag)
library(kknn)
library(mlbench)

data("glass")
data("Vehicle")

# Glass
data <- glass
n <- nrow(data)

train_idx <- sample(1:n, 0.7 * n)
test_idx <- seq(1, n)[-train_idx]

# Ensamble
boosting_model <- bagging(
  Type ~ .,
  data = data[train_idx, ],
  mfinal = 10,
  maxdepth = 5
)

boosting_pred <- predict(
  boosting_model,
  newdata = data[test_idx, ]
)

boosting_pred_table <- table(boosting_pred$class, data$Type[test_idx])
boosting_err <- 1 - (sum(diag(boosting_pred_table)) / sum(boosting_pred_table))

# Single tree
tree_model <- rpart(
  Type ~ .,
  data = data[train_idx, ],
  maxdepth = 5
)

tree_pred <- predict(
  tree_model,
  newdata = data[test_idx, ],
  type = "class"
)

tree_pred_table <- table(tree_pred, data$Type[test_idx])
tree_err <- 1 - (sum(diag(tree_pred_table)) / sum(tree_pred_table))

cat(
  "Glass dataset\n",
  "\tEnsamble error:", boosting_err, "\n",
  "\tSingle tree error:", tree_err, "\n"
)

# Vehicle
data <- Vehicle
n <- nrow(data)

train_idx <- sample(1:n, 0.7 * n)
test_idx <- seq(1, n)[-train_idx]

# Ensamble
boosting_model <- bagging(
  Class ~ .,
  data = data[train_idx, ],
  mfinal = 100,
  maxdepth = 10
)

boosting_pred <- predict(
  boosting_model,
  newdata = data[test_idx, ]
)

boosting_pred_table <- table(boosting_pred$class, data$Class[test_idx])
boosting_err <- 1 - (sum(diag(boosting_pred_table)) / sum(boosting_pred_table))

# Single tree
tree_model <- rpart(
  Class ~ .,
  data = data[train_idx, ],
  maxdepth = 10
)

tree_pred <- predict(
  tree_model,
  newdata = data[test_idx, ],
  type = "class"
)

tree_pred_table <- table(tree_pred, data$Class[test_idx])
tree_err <- 1 - (sum(diag(tree_pred_table)) / sum(tree_pred_table))

cat(
  "Vehicle dataset\n",
  "\tEnsamble error:", boosting_err, "\n",
  "\tSingle tree error:", tree_err, "\n"
)