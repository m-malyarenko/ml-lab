library(tree)
library(tidyr)
library(dplyr)

data_train <- read.csv("data/titanic_train.csv")
data_test <- read.csv("data/titanic_test.csv")

for (i in 1:ncol(data_train)) {
  missing <- sum(is.na(data_train[i]))
  print(paste(colnames(data_train)[i], " missing:", missing))
}

for (i in 1:ncol(data_test)) {
  missing <- sum(is.na(data_test[i]))
  print(paste(colnames(data_test)[i], " missing:", missing))
}

# Fill missing data and cast types
data_train <- data_train %>% fill(Age)
data_train$Survived <- as.factor(data_train$Survived)
data_train$Sex <- as.factor(data_train$Sex)
data_train$Age <- as.integer(data_train$Age)

data_test <- data_test %>% fill(Age)
data_test <- data_test %>% fill(Fare)
data_test$Age <- as.integer(data_test$Age)
data_test$Sex <- as.factor(data_test$Sex)

# Tree classifier
model_formula <- as.formula(
  Survived ~
    Pclass +
    Sex +
    Age +
    SibSp +
    Parch +
    Fare
)

tree_classifier <- tree(
  model_formula,
  data = data_train,
  method = "gini"
)

plot(tree_classifier, type = "uniform")
text(tree_classifier, cex = 0.7)

pred <- predict(tree_classifier, data_test, type = "class")
pred <- levels(data_train$Survived)[pred]
pred <- data.frame(PassengerId = data_test$PassengerId, Survived = pred)

write.csv(pred, "data/titanic_pred.csv", quote = FALSE, row.names = FALSE)

# Score: 0.77033
