library(kknn)

data <- read.csv("data/glass.csv")
data <- data[,-1]
data$Class <- as.factor(data$Class)

# Classification error against k & kernel type
k_values <- 1:30
kernel_values <- c(
  "rectangular",
  "triangular",
  "epanechnikov",
  "biweight",
  "gaussian"
)

cross_train <- train.kknn(
  Class ~ .,
  data = data,
  ks = k_values,
  kernel = kernel_values,
  kcv = 10,
  distance = 2
)

err <- cross_train$MISCLASS

for (col in colnames(err)) {
  plot(
    x = k_values,
    y = err[ , col],
    
    type = "l",
    cex = 0.5,
    pch = 19,
    col = "blue",
    
    xlab = "k",
    ylab = "Ошибка классификации",
    main = paste(
      "Зависимость ошибки классификации от параметра k\n",
      "Ядро:",
      col
    ),
  ) 
}

# Classification error against distance parameter
distance_values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

err <- c()

for (distance in distance_values) {
  cross_train <- train.kknn(
    Class ~ .,
    data = data,
    ks = c(round(sqrt(nrow(data)))),
    kernel = "optimal",
    kcv = 10,
    distance = distance
  )
  
  err <- append(err, cross_train$MISCLASS)
}

plot(
  x = distance_values,
  y = err,
  
  type = "l",
  cex = 0.5,
  pch = 19,
  col = "blue",
  
  xlab = "k",
  ylab = "Ошибка классификации",
  main = "Зависимость ошибки классификации от параметра расстояния Минковского"
)

# Classification example
sample <- data.frame(
  RI = 1.516,
  Na = 11.7,
  Mg = 1.01,
  Al = 1.19,
  Si = 72.59,
  K  = 0.43,
  Ca = 11.44,
  Ba = 0.02,
  Fe = 0.1 
)

knn_result <- kknn(Class ~ ., data, sample, k = 10)
pred <- knn_result$fitted.values

# Attribute impact on classification
accuracy <- c()
factors <- colnames(data)
factors <- factors[-length(factors)]

for (x in factors) {
  f <- as.formula(paste("Class ~ . -", x))

  cross_train <- train.kknn(
    f,
    data = data,
    ks = c(round(sqrt(nrow(data)))),
    kernel = "optimal",
    kcv = 10,
    distance = distance
  )

  accuracy <- append(accuracy, 1 - cross_train$MISCLASS[1])
}

plot(
  x = 1:(ncol(data) - 1),
  y = accuracy,
  
  type = "b",
  pch = 15,
  col = "red",
  
  xlab = "Признак",
  xaxt = "n",
  ylab = "Точность классификации",
  main = "Зависимость точности классификации от признака"
)
axis(1, at = 1:(ncol(data) - 1), labels = factors)

min_impact_factor <- factors[which.min(accuracy)]

cat("Min impact factor:", min_impact_factor)
