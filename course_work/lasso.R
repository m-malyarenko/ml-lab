library(glmnet)

data <- read.csv("data/ionosphere.csv", stringsAsFactors = TRUE)
n <- nrow(data)

X <- as.matrix(subset(data, select = -Class))
y <- as.numeric(data$Class)

lambda_values <- 10^seq(-3, 3)
lasso_cv <- cv.glmnet(
  X,
  y,
  alpha = 1,
  lambda = lambda_values,
  nfolds = 10
)

best_lambda <- lasso_cv$lambda.min
lasso <- glmnet(
  X,
  y,
  alpha = 1,
  lambda = best_lambda,
  label = TRUE
)
print(lasso$beta)
