library(caret)
library(tsne)

# set.seed(128)

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
  mfinal = 5:20,
  maxdepth = 5:10,
  coeflearn = 'Breiman'
)

adaboost_classifier <- train(
  Class ~ .,
  data = data_train,
  method = "AdaBoost.M1",
  trControl = train_control,
  tuneGrid = tunung_grid
)

adaboost_prediction <- predict(
  adaboost_classifier,
  newdata = subset(data_test, select = -Class)
)

adaboost_accuracy <- sum(adaboost_prediction == data_test$Class) / test_size

print(adaboost_classifier$finalModel$tuneValue)
cat("\nAdaBoost classifier accuracy:", adaboost_accuracy)
