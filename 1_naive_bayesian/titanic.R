library(e1071)

data_train <- read.csv("data/titanic_train.csv")
data_test <- read.csv("data/titanic_test.csv")

bayes_classifier <- naiveBayes(
  Survived ~
    Pclass +
    Sex +
    Age +
    Ticket +
    Cabin,

  data = data_train
)

pred <- predict(bayes_classifier, data_test)
pred <- data.frame(PassengerId = data_test$PassengerId, Survived = pred)

write.csv(pred, "data/titanic_pred.csv", quote = FALSE, row.names = FALSE)

# Accuracy: 0.77990