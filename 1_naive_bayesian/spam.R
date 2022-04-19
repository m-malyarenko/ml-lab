library(kernlab)
library(e1071)

data(spam)

n <- nrow(spam)

data_shuffled <- spam[order(runif(n)), ]

for (train_ratio in c(0.5, 0.6, 0.7, 0.8, 0.9)) {
  n_train <- as.integer(n * train_ratio)
  n_test <- n - n_train;
  
  data_train <- data_shuffled[1:n_train, ]
  data_test <- data_shuffled[(n_train + 1):n, ]
  
  bayes_classifier <- naiveBayes(type ~ ., data = data_train)
  
  pred <- predict(bayes_classifier, data_test)
  pred_table <- table(pred, data_test$type)
  
  accuracy <- (sum(pred == data_test$type) / n_test) * 100
  inaccuracy <- (sum(pred != data_test$type) / n_test) * 100
  
  cat("Training/Test data ratio:", train_ratio, '\n')
  cat("Training data frame size:", n_train, '\n')
  cat("Test data frame size:", n_test, '\n')
  print(pred_table)
  cat('\n')
  cat("Accuracy:", round(accuracy, 1), "%\n")
  cat("Inccuracy:", round(inaccuracy, 1), "%\n")
  cat('\n')
}
