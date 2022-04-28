library(datasets)
data("sunspot.year")

lm_model <- lm(
  sunspot.year ~ seq(1700, 1988)
)

plot(
  sunspot.year,
  
  xlab = "Год",
  ylab = "Количество солнечных дней",
  main = "Динамика изменения количества слонечных дней"
)
lines (
  x = seq(1700, 1988),
  y = lm_model$fitted.values,
  col = "blue"
)