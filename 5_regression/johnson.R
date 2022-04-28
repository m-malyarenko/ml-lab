library(datasets)

data("JohnsonJohnson")

plot(
  JohnsonJohnson,
  xlab = "Year",
  ylab = "Income"
)

jj_qrt1 <- JohnsonJohnson[seq(1, length(JohnsonJohnson), by = 4)]
jj_qrt2 <- JohnsonJohnson[seq(2, length(JohnsonJohnson), by = 4)]
jj_qrt3 <- JohnsonJohnson[seq(3, length(JohnsonJohnson), by = 4)]
jj_qrt4 <- JohnsonJohnson[seq(4, length(JohnsonJohnson), by = 4)]

data <- data.frame(
  Qrt1 = jj_qrt1,
  Qrt2 = jj_qrt2,
  Qrt3 = jj_qrt3,
  Qrt4 = jj_qrt4,
  Year = seq(1960, 1980)
)

column_names <- c("Qrt1", "Qrt2", "Qrt3", "Qrt4")
column_colors <- c("blue", "red", "darkgreen", "purple")
dynamic_coef <- c()

for (i in 1:4) {
  f <- as.formula(paste(column_names[i], "~ Year"))
  
  linear_regression <- lm(
    f,
    data = data
  )
  
  plot(
    x = data$Year,
    y = data[ , i],
    ylim = c(0, 20),
    
    ylab = "Income",
    xlab = "Year",
    main = paste("Quarter #", i, sep = ""),
    
    type = "b",
    pch = 19,
    col = column_colors[i]
  )
  lines(
    x = data$Year,
    y = linear_regression$fitted.values,
    col = column_colors[i]
  )
  
  dynamic_coef <- append(dynamic_coef, linear_regression$coefficients[2])
}

# Find Min Max dynamics in income by quarter

print(paste(
  "Max dynamic in",
  colnames(data)[which.max(dynamic_coef)],
  ":",
  max(dynamic_coef)
))

print(paste(
  "Min dynamic in",
  colnames(data)[which.min(dynamic_coef)],
  ":",
  min(dynamic_coef)
))

# Predict 2016 income
sample <- data.frame(Year = 2016)
qrt_predictions <- c()

for (i in 1:4) {
  f <- as.formula(paste(column_names[i], "~ Year"))

  lm_model <- lm(
    f,
    data = data
  )
  
  pred <- predict(lm_model, newdata = sample)
  qrt_predictions <- append(qrt_predictions, pred)

  print(paste("Quarter #", i, " 2016 prediction: ", pred, sep = ""))
}

print(paste("Year average 2016 prediction: ", mean(qrt_predictions), sep = ""))
