source("adaboost_knn.R")
library(tree)

# Vehicle dataset
data(Vehicle)
n <- nrow(Vehicle)
data <- Vehicle[order(runif(nrow(Vehicle))), ]
train_idx <- sample(1:n, n * 0.7)

data_train <- data[train_idx, ]
data_test <- data[-train_idx, ]

tree_model <- tree(
  Class ~ .,
  data = data_train,
  split = "gini"
)
tree_predict <- predict(tree_model, data_test, type = "class")
tree_err <- sum(tree_predict != data_test$Class) / nrow(data_test)

X_train <- subset(data_train, select = -Class)
y_train <- data_train$Class

X_test <- subset(data_test, select = -Class)
y_test <- data_test$Class

boost_model <- adaboost_knn_train(X_train, y_train, 15)
boost_predict <- adaboost_knn_predict(boost_model, X_test)
boost_err <- sum(boost_predict != y_test) / nrow(data_test)

cat(
  "Vehicle dataset",
  "\n\tTree modell error:", tree_err,
  "\n\t KNN AdaBoost error:", boost_err,
  "\n"
)

# Class dataset
data(Glass)
n <- nrow(Glass)
data <- Glass[order(runif(nrow(Glass))), ]
train_idx <- sample(1:n, n * 0.7)

data_train <- data[train_idx, ]
data_test <- data[-train_idx, ]

tree_model <- tree(
  Type ~ .,
  data = data_train,
  split = "gini"
)
tree_predict <- predict(tree_model, data_test, type = "class")
tree_err <- sum(tree_predict != data_test$Type) / nrow(data_test)

X_train <- subset(data_train, select = -Type)
y_train <- data_train$Type

X_test <- subset(data_test, select = -Type)
y_test <- data_test$Type

boost_model <- adaboost_knn_train(X_train, y_train, 15)
boost_predict <- adaboost_knn_predict(boost_model, X_test)
boost_err <- sum(boost_predict != y_test) / nrow(data_test)

cat(
  "Glass dataset",
  "\n\tTree modell error:", tree_err,
  "\n\t KNN AdaBoost error:", boost_err,
  "\n"
)

