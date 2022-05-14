library(cluster)
data("pluton")

within_ss <- c()

for (i in 1:100) {
  clusters <- kmeans(pluton, 3, iter.max = i, nstart = 3)
  print(paste("k:", i, "within_ss:", clusters$tot.withinss))
  within_ss <- append(within_ss, clusters$tot.withinss)
}

optimal_iter_max <- which.min(within_ss)
cat(
  "Min within ss = ",
  within_ss[optimal_iter_max],
  "optimal iter =",
  optimal_iter_max, "\n")
