library(tree)

data <- read.table("data/lenses.txt", stringsAsFactors = TRUE)
data <- data[ ,-1]

data$V2 = as.factor(data$V2)
data$V3 = as.factor(data$V3)
data$V4 = as.factor(data$V4)
data$V5 = as.factor(data$V5)
data$V6 = as.factor(data$V6)

set.seed(12345)
data <- data[order(runif(nrow(data))), ]

tree_classifier <- tree(
  V6 ~ .,
  data = data,
  split = "gini",
)

plot(tree_classifier, type = "uniform")
text(tree_classifier, cex = 0.7)

sample <- data.frame(
  V2 = factor(2, levels(data$V2)),
  V3 = factor(1, levels(data$V3)),
  V4 = factor(2, levels(data$V4)),
  V5 = factor(1, levels(data$V5))
)

pred <- predict(tree_classifier, sample)
cat("Prediction class:", pred, "\n")
# Predicted class: 3
