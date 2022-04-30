data("pluton")

within_ss <- c()

for (i in 1:(nrow(pluton) - 1)) {
  clusters <- kmeans(pluton, i)
  print(paste("k:", i, "within_ss:", clusters$tot.withinss))
  within_ss <- append(within_ss, clusters$tot.withinss)
}

optimal_k <- which.min(within_ss)
cat("Min within ss = ", within_ss[optimal_k], "optimal k =", optimal_k, "\n")
