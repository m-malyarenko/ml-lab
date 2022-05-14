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

result_colnames <- c("k", "Accuracy", "Splits")
result_table <- data.frame(matrix(nrow = 0, ncol = length(result_colnames)))
colnames(result_table) = result_colnames

# Prune tree to sequence of trees
prune_tree_seq <- prune.tree(tree, method = "misclass")
prune_k_seq <- prune_tree_seq$k[-1]
n_k <- length(prune_k_seq)

accuracy <- c()

for (k in prune_k_seq) {
  prune_tree <- prune.tree(tree, as.double(k))

  plot(prune_tree, type = "uniform")
  text(prune_tree, cex = 0.7)
  
  pred <- predict(prune_tree, data_test, type = "class")
  current_accuracy <- sum(pred == data_test$yesno) / n_test * 100
  accuracy <- append(accuracy, current_accuracy)
  
  result_row <- data.frame(
    k = k,
    Accuracy = current_accuracy,
    Splits = nrow(prune_tree$frame)
  )
  result_table <- rbind(result_table, result_row)
}

print(result_table)
write.csv(result_table, "data/spam_tree_k_stat.csv", quote = FALSE, row.names = FALSE)

result_table$Splits <- scale(result_table$Splits)
result_table$Accuracy <- scale(result_table$Accuracy)


# Pick optimal k
plot(
  x = prune_k_seq[1:(n_k - 2)],
  y = result_table$Splits[1:(n_k - 2)],
  type = "b",
  pch = 20,
  col = "red",
  
  xlab = "k",
  ylab = ""
)
points(
  x = prune_k_seq[1:(n_k - 2)],
  y = result_table$Accuracy[1:(n_k - 2)],
  type = "b",
  pch = 20,
  col = "green"
)
legend(
  x = "topright",
  legend = c("Точность", "Количество узлов"),
  fill = c("green", "red")
)

best_accuracy <- prune.tree(tree, prune_k_seq[which.max(accuracy)])
accuracy_size_tradeoff <- prune.tree(tree, prune_k_seq[18])

# Best accuracy: 10, k = 2
# Accuracy-Size tradeoff: 18, k = 31.5

plot(best_accuracy, type = "uniform")
text(best_accuracy, cex = 0.7)

plot(accuracy_size_tradeoff, type = "uniform")
text(accuracy_size_tradeoff, cex = 0.7)
