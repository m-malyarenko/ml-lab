library(kknn)

data_train <- read.table(
  "data/svmdata4.txt",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = TRUE
)
data_train <- data_train[ , -1]

data_test <- read.table(
  "data/svmdata4test.txt",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = TRUE
)
data_test <- data_test[ , -1]

cross_train <- train.kknn(
  Colors ~ .,
  data = data_train,
  kmax = 15,
  kcv = 10
)

optimal_k <- cross_train$best.parameters$k
optimal_kernel <- cross_train$best.parameters$kernel

accuracy <- c()

for (k in 1:30) {
  knn_classifier <- kknn(
    Colors ~ .,
    data_train,
    data_test,
    k = k,
    kernel = optimal_kernel
  )
  
  pred <- knn_classifier$fitted.values
  pred_table <- table(pred, data_test$Colors)
  current_accuracy <- (sum(pred == data_test$Colors) / nrow(data_test))
  accuracy <- append(accuracy, current_accuracy)
}

plot(
  x = 1:30,
  y = accuracy,
  
  type = "b",
  pch = 19,
  
  xlab = "k",
  ylab = "Точность классификации",
  
  main = "Зависимость точности классификации от k"
)

optimal_k <- which.max(accuracy)
cat("Optimal k:", optimal_k, "\n")

plot(
  x = data_test$X1,
  y = data_test$X2,

  pch = 19,
  col = as.character(data_test$Colors),
  
  xlab = "X1",
  ylab = "X2",
  
  main = "Обучающая выборка"
)
