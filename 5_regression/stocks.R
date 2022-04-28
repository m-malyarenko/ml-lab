library(datasets)
data("EuStockMarkets")

plot(
  EuStockMarkets[, 1],
  ylim = c(1000, 9000),
  col = "blue",
  
  xlab = "Time",
  ylab = "Price"
)
lines(
  EuStockMarkets[, 2],
  col = "red"
)
lines(
  EuStockMarkets[, 3],
  col = "green"
)
lines(
  EuStockMarkets[, 4],
  col = "purple"
)
legend(
  x = "topleft",
  legend = c("DAX", "SMI", "CAC", "FTSE"),
  col = c("blue", "red", "green", "purple"),
  bty = "n",
  pch = 19
)

# Linear regression
Days <- 1:nrow(EuStockMarkets)
data <- data.frame(EuStockMarkets)

index_names <- colnames(data)
index_colors <- c("blue", "red", "green", "purple")

data <- cbind(data, Days)

for (i in 1:ncol(EuStockMarkets)) {
  f <- as.formula(paste(index_names[i], "~ Days"))

  linear_regression <- lm(
    f,
    data = data
  )
  
  plot(
    data$DAX,
    ylim = c(1000, 9000),
    
    ylab = "Price",
    xlab = "Days",
    main = as.character(index_names[i]),

    type = "l",
    col = index_colors[i]
  )
  lines(
    linear_regression$fitted.values,
    type = "l",
    col = index_colors[i]
  )
  
  print(paste(
    index_names[i],
    "linear regression: y =", linear_regression$coefficients[2],
    "* x +", linear_regression$coefficients[1]
  ))
}
