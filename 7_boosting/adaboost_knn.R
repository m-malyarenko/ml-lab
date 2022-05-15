library(caret)

# Prediction encoding to {-1, 1} vector
sign_one_hot <- function(y) {
  encoding <- model.matrix(~ 0 + y)
  colnames(encoding) <- levels(y)
  encoding[encoding == 0] <- -1
  
  return(encoding)
}

# AdaBoost algorithm for KNN weak classifier
adaboost_knn_train <- function(X, y, nweak) {
  n <- nrow(X)
  
  w <- rep(1 / n, times = n)
  alpha <- rep(0, times = nweak)
  weak_classifiers <- rep(list(list), times = nweak)
  
  knn_control <- trainControl(
    method = "cv",
    number = 10
  )
  
  for (i in 1:nweak) {
    knn_classifier <- train(
      X,
      y,
      method = "knn",
      trControl = knn_control,
      weights = w
    )
    
    knn_predict <- predict(
      knn_classifier,
      newdata = X
    )
    
    misclass <- as.numeric(y != knn_predict)
    err <- sum(w * misclass)
    alpha[i] <- (1 / 2) * log((1 - err) / err)
    w <- w * exp((-1)^misclass * alpha[i])
    w <- w / sum(w)
    weak_classifiers[[i]] <- knn_classifier
  }
  
  return(list(
    ensemble = weak_classifiers,
    alpha = alpha,
    levels = levels(y)
  ))
}

adaboost_knn_predict <- function(model, newdata) {
  vote_table <- matrix(0, nrow(newdata), length(model$levels))
  
  for (i in 1:length(model$ensemble)) {
    pred <- predict(model$ensemble[[i]], newdata = newdata)
    pred <- sign_one_hot(pred)
    pred <- pred * model$alpha[i]
    
    vote_table <- vote_table + pred
  }
  
  y <- c()
  for (i in 1:nrow(newdata)) {
    voting <- which.max(vote_table[i, ])
    y <- append(y, model$levels[voting])
  }
  
  y <- as.factor(y)
  levels(y) <- model$levels
  
  return(y)
}