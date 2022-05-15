library(caret)

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
  number = 5
)

nb_classifier <- train(
  Class ~ .,
  data = data_train,
  method = "naive_bayes",
  trControl = train_control,
)

nb_prediction <- predict(
  nb_classifier,
  newdata = subset(data_test, select = -Class)
)

nb_accuracy <- sum(nb_prediction == data_test$Class) / length(nb_prediction)

summary(nb_classifier)
cat("Naive bayesian classifier accuracy:", nb_accuracy)
