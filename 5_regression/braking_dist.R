library(datasets)
data("cars")

linear_regression <- lm(
  dist ~ speed,
  data = cars
)

plot(
  dist ~ speed,
  data = cars,
  
  xlab = "Скорость, миль/ч",
  ylab = "Тормозной путь, фут",
  main = "Зависимость тормозного пути от скорости",
  type = "b",
  pch = 20
)
lines(
  x = cars$speed,
  y = linear_regression$fitted.values,
  col = "red"
)

sample <- data.frame(speed = c(40))

pred <- predict(linear_regression, newdata = sample)
print(paste("Braking distance for 40 mph prediction:", pred[1], "ft"))
