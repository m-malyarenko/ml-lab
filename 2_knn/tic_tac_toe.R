library(kknn)

data <- read.table("data/tic_tac_toe.csv", sep = ",", stringsAsFactors = TRUE)

n <- nrow(data)

set.seed(128)
data <- data[order(runif(n)), ]

for (train_ratio in c(0.5, 0.6, 0.7, 0.8, 0.9)) {
  n_train <- floor(n * train_ratio)
  n_test <- n - n_train;
  
  data_train <- data[1:n_train, ]
  data_test <- data[(n_train + 1):n, ]
  
  knn_classifier <- kknn(
    V10 ~ .,
    data_train,
    data_test,
    k = round(sqrt(n_train)),
    kernel = "triangular"
  )
  
  pred <- knn_classifier$fitted.values
  pred_table <- table(pred, data_test$V10)
  
  accuracy <- (sum(pred == data_test$V10) / n_test) * 100
  inaccuracy <- (sum(pred != data_test$V10) / n_test) * 100
  
  cat("Training/Test data ratio:", train_ratio, '\n')
  cat("Training data frame size:", n_train, '\n')
  cat("Test data frame size:", n_test, '\n')
  print(pred_table)
  cat('\n')
  cat("Accuracy:", round(accuracy, 1), "%\n")
  cat("Inccuracy:", round(inaccuracy, 1), "%\n")
  cat('\n')
}