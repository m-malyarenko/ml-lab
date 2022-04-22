library(tree)
library(DAAG)
library(e1071)
library(rpart)

set.seed(12345)
data <- data.frame(DAAG::nsw74psid1)
data <- data[order(runif(nrow(data))), ]

# n <- nrow(data)
# n_train <- round(n * 0.9)
# n_test <- n - n_train
# 
# data_train <- data[1:n_train, ]
# data_test <- data[(n_train + 1):n, ]

# Как сделать регрессионное дерево по непрерывным значениям, а не классам

tree_regression <- rpart(
  re78 ~ .,
  data = data,
  method = "anova",
)

plot(tree_regression, uniform = TRUE)
text(tree_regression, cex = 0.7)

svm_regression <- svm(
  x = data[-data$re78],
  y = data$re78,
  type = "eps-regression",
  kernel = "radial",
)

pred_tree <- predict(tree_regression, data[-data$re78])
pred_svm <- predict(svm_regression, data[-data$re78])

tree_regression_rmse <- sqrt(mean((pred_tree - data$re78)^2))
svm_regression_rmse <- sqrt(mean((pred_svm - data$re78)^2))

cat("Tree regression RMSE:", tree_regression_rmse, "\n")
cat("SVM regression RMSE", svm_regression_rmse, "\n")

plot(pred_tree, pch = 17, col = "red", main = "Tree regression")
points(data$re78, pch = 16, col = "green")

plot(pred_svm, pch = 17, col = "red", main = "SVM regression")
points(data$re78, pch = 16, col = "green")
