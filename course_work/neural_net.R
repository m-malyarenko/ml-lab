library(caret)
library(tsne)

set.seed(128)

data <- read.csv("data/ionosphere.csv", stringsAsFactors = TRUE)
n <- nrow(data)

train_idx <- sample(1:n, n * 0.7)
data_train <- data[train_idx, ]
data_test <- data[-train_idx, ]

train_size <- nrow(data_train)
test_size <- nrow(data_test)

train_control <- trainControl(
  method = "cv",
  number = 10
)

tunung_grid <- expand.grid(
  size = 1:34
)

neural_net_classifier <- train(
  Class ~ .,
  data = data_train,
  method = "mlp",
  preProcess = c("center"),
  trControl = train_control,
  tuneGrid = tunung_grid
)

neural_net_classifier

neural_net_prediction <- predict(
  neural_net_classifier,
  newdata = subset(data_test, select = -Class)
)

neural_net_accuracy <-
  sum(neural_net_prediction == data_test$Class) / test_size

print(neural_net_classifier$finalModel$tuneValue)
cat("\nMLP classifier accuracy:", neural_net_accuracy)
