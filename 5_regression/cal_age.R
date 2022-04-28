library(glmnet)

data <- read.table("data/cygage.txt", sep = "\t", header = TRUE)

predictors <- data.matrix(data[ , c('Depth', 'Weight')])
response <- data$calAge

# R-squared calculation

r.sqr <- function(observ, pred) {
  sst <- sum((observ - mean(observ))^2)
  sse <- sum((pred - observ)^2)
  
  return(1 - sse / sst)
}

# RMSE calculation

rmse <- function(observ, pred) {
  sse <- sum((pred - observ)^2)
  
  return(sqrt(sse / length(observ)))
}

# RSS regression

rss <- lm(
  calAge ~ Depth + Weight,
  data = data
)

rss_r_sqr <- r.sqr(data$calAge, rss$fitted.values)
rss_rmse <- rmse(data$calAge, rss$fitted.values)

# Ridge regression

ridge_cv <- cv.glmnet(
  x = predictors,
  y = response,
  alpha = 0
)

ridge_best_lambda <- ridge_cv$lambda.min

ridge <- glmnet(
  x = predictors,
  y = response,
  alpha = 0,
  lambda = ridge_best_lambda
)

ridge_fitted <- predict(
  ridge,
  s = ridge_best_lambda,
  newx = predictors
)

ridge_r_sqr <- r.sqr(response, ridge_fitted)
ridge_rmse <- rmse(response, ridge_fitted)

# Lasso regression

lasso_cv <- cv.glmnet(
  x = predictors,
  y = response,
  alpha = 1
)

lasso_best_lambda <- lasso_cv$lambda.min

ridge <- glmnet(
  x = predictors,
  y = response,
  alpha = 1,
  lambda = lasso_best_lambda
)

lasso_fitted <- predict(
  ridge,
  s = lasso_best_lambda,
  newx = predictors
)

lasso_r_sqr <- r.sqr(response, lasso_fitted)
lasso_rmse <- rmse(response, lasso_fitted)

print(paste("RSS R-squared:", rss_r_sqr, "RMSE:", rss_rmse))
print(paste("Ridge R-squared:", ridge_r_sqr, "RMSE:", ridge_rmse))
print(paste("Lasso R-squared:", lasso_r_sqr, "RMSE:", lasso_rmse))
