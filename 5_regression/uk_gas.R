data <- read.csv("data/uk_gas.csv")
data <- data[ , -1]

lm_model <- lm(
  UKgas ~ time,
  data = data
)

plot(
  data,
  type = "l",
  
  xlab = "Год",
  ylab = "Расход газа",
  main = "Динамика расхода газа в Великобритании"
)
lines (
  x = seq(1960, 1986.75, by = 0.25),
  y = lm_model$fitted.values,
  col = "blue"
)

qrt1 <- data[seq(1, nrow(data), by = 4), 2]
qrt2 <- data[seq(2, nrow(data), by = 4), 2]
qrt3 <- data[seq(3, nrow(data), by = 4), 2]
qrt4 <- data[seq(4, nrow(data), by = 4), 2]

data <- data.frame(
  Qrt1 = qrt1,
  Qrt2 = qrt2,
  Qrt3 = qrt3,
  Qrt4 = qrt4,
  Year = seq(1960, 1986)
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
    ylim = c(0, 1200),
    
    ylab = "Расход газа",
    xlab = "Год",
    main = paste("Квартал #", i, sep = ""),
    
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
