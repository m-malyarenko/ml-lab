library(e1071)

n <- 100
n_a <- 50
n_b <- n - n_a

points_a <- data.frame(
  X1 = rnorm(n_a, mean = 10, sd = 4),
  X2 = rnorm(n_a, mean = 14, sd = 4),
  class = rep("A", times = n_a)
)

points_b <- data.frame(
  X1 = rnorm(n_b, mean = 20, sd = 3),
  X2 = rnorm(n_b, mean = 18, sd = 3),
  class = rep("B", times = n_b)
)

points <- rbind(points_a, points_b)

# Plot
plot(
  x = points$X1,
  xlab = "X1",
  y = points$X2,
  ylab = "X2",
  pch = 19, # filled circle
  col = ifelse(points$class == "A", "red", "blue")
)

grid(10, 10)

legend(
  x = "topright",
  legend = c("Class A", "Class B"),
  col = c("red", "blue"),
  bty = "n",
  pch = 19
)

# Naive Bayes Classifier
data_shuffled <- points[order(runif(n)), ]

for (train_ratio in c(0.5, 0.6, 0.7, 0.8, 0.9)) {
  n_train <- as.integer(n * train_ratio)
  n_test <- n - n_train;
  
  data_train <- data_shuffled[1:n_train, ]
  data_test <- data_shuffled[(n_train + 1):n, ]
  
  bayes_classifier <- naiveBayes(class ~ ., data = data_train)
  
  pred <- predict(bayes_classifier, data_test)
  pred_table <- table(pred, data_test$class)
  
  accuracy <- (sum(pred == data_test$class) / n_test) * 100
  inaccuracy <- (sum(pred != data_test$class) / n_test) * 100
  
  cat("Training/Test data ratio:", train_ratio, '\n')
  cat("Training data frame size:", n_train, '\n')
  cat("Test data frame size:", n_test, '\n')
  print(pred_table)
  cat('\n')
  cat("Accuracy:", round(accuracy, 1), "%\n")
  cat("Inccuracy:", round(inaccuracy, 1), "%\n")
  cat('\n')
}

