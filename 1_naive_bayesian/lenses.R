# Лабораторная работа №1
# Наивный Байесовский классификатор

library(e1071)

A_raw <- read.table("data/lenses.txt", stringsAsFactors = TRUE)
A_raw <- A_raw[,-1]

n <- dim(A_raw)[1]
A_raw[A_raw == "1"] <- "a"
A_raw[A_raw == "2"] <- "b"
A_raw[A_raw == "3"] <- "c"

set.seed(12345)
A_rand <- A_raw[ order(runif(24)), ]

A_train <- A_rand[1:23, ]
A_test <- A_rand[24:24, ]

prop.table(table(A_train$V2))
prop.table(table(A_test$V2))

A_classifier <- naiveBayes(V2 ~ ., data = A_train, laplace = 1)
A_predicted <- predict(A_classifier, A_test)
table(A_predicted, A_test$V2)

A_predicted <- predict(A_classifier, A_test)
