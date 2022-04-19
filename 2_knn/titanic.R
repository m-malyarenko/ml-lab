library(kknn)
library(tidyr)
library(dplyr)

data_train <- read.csv("data/titanic_train.csv")
data_test <- read.csv("data/titanic_test.csv")

# Fill missing data
data_train <- data_train %>% fill(Age)
data_test <- data_test %>% fill(Age)

# Set up class factors
data_train$Survived <- as.factor(data_train$Survived)
data_test$Survived <- as.factor(data_train$Survived)

model_formula <- as.formula(
  Survived ~
    Pclass +
    Sex +
    Age +
    SibSp +
    Parch +
    Fare
)

cross_train <- train.kknn(
  model_formula,
  data = data_train,
  kmax = 50,
  kcv = 50
)

optimal_k <- cross_train$best.parameters$k
optimal_kernel <- cross_train$best.parameters$kernel

knn_classifier <- kknn(
  model_formula,
  data_train,
  data_test,
  k = optimal_k,
  kernel = optimal_kernel
)

pred <- levels(data_train$Survived)[knn_classifier$fitted.values]
pred <- append("0", pred) # WTF! Where is 1-st row?!
pred <- data.frame(PassengerId = data_test$PassengerId, Survived = pred)

write.csv(pred, "data/titanic_pred.csv", quote = FALSE, row.names = FALSE)

# Accuracy score: 0.66985
