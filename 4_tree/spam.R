library(tree)
library(DAAG)

set.seed(12345)
data <- data.frame(DAAG::spam7)
data <- data[order(runif(nrow(data))), ] # Shuffle

n <- nrow(data)
n_train <- round(n * 0.8)
n_test <- n - n_train

data_train <- data[1:n_train, ]
data_test <- data[(n_train + 1):n, ]

tree <- tree(
  yesno ~ .,
  data = data_train,
  split = "gini",
)

plot(tree, type = "uniform")
text(tree, cex = 0.7)

result_colnames <- c("k", "Accuracy")
result_table <- data.frame(matrix(nrow = 0, ncol = length(result_colnames)))
colnames(result_table) = result_colnames

# Prune tree to sequence of trees
prune_tree_seq <- prune.tree(tree, method = "misclass")
prune_k_seq <- prune_tree_seq$k[-1]

accuracy <- c()

for (k in prune_k_seq) {
  prune_tree <- prune.tree(tree, as.double(k))

  plot(prune_tree, type = "uniform")
  text(prune_tree, cex = 0.7)
  
  pred <- predict(prune_tree, data_test, type = "class")
  current_accuracy <- sum(pred == data_test$yesno) / n_test * 100
  accuracy <- append(accuracy, current_accuracy)
  
  result_row <- data.frame(k = k, Accuracy = current_accuracy)
  result_table <- rbind(result_table, result_row)
}

print(result_table)

# Pick optimal k
plot(x = prune_k_seq, y = accuracy, type = "b", pch = 20)
best_accuracy <- prune.tree(tree, prune_k_seq[which.max(accuracy)])
accuracy_size_tradeoff <- prune.tree(tree, prune_k_seq[18])

# Best accuracy: 10, k = 2
# Accuracy-Size tradeoff: 18, k = 31.5

plot(best_accuracy, type = "uniform")
text(best_accuracy, cex = 0.7)

plot(accuracy_size_tradeoff, type = "uniform")
text(accuracy_size_tradeoff, cex = 0.7)
