data <- read.table("data/reglab2.txt", sep = "\t", header = TRUE)

# MSE calculation

mse <- function(observ, pred) {
  sse <- sum((pred - observ)^2)
  
  return(sse / length(observ))
}

# Find optimal predictors combination

response <- data$y

predictor_names <- colnames(data)[-1]
predictors_comb <- list()

for (i in 1:length(predictor_names)) {
  comb <- combn(predictor_names, i)
  
  for (j in 1:ncol(comb)) {
    predictors_comb <- append(predictors_comb, list(comb[ , j])) 
  }
}

lm_mse <- c()

for (comb in predictors_comb) {
  comb_str <- paste(comb, collapse = " + ")
  f <- as.formula(paste("y ~", comb_str))

  model <- lm(
    f,
    data = data
  )
  
  current_mse <- mse(response, model$fitted.values)
  
  print(f)
  cat("\nMSE:", current_mse, "\n\n")
  
  lm_mse <- append(lm_mse, current_mse)
}

min_mse_comb <- predictors_comb[which.min(lm_mse)]
min_mse <- min(lm_mse)

print(paste("Min MSE:", min_mse, "by combination", min_mse_comb))
