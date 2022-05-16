library(cluster)
library(pvclust)

# set.seed(128)

data <- read.csv("data/ionosphere.csv", stringsAsFactors = TRUE)
n <- nrow(data)

X <- subset(data, select = -Class)
y <- data$Class

medoids_clustering <- pam(
  X,
  k = 2,
  metric = "euclidean"
)

medoids_clustering <- medoids_clustering$clustering
medoids_clustering[medoids_clustering == 1] = "g"
medoids_clustering[medoids_clustering == 2] = "b"

err <- 1 - (sum(medoids_clustering == y) / n)
cat("Clusterization error:", err)

X_dist <- dist(X)
hierarchy <- hclust(X_dist)
plot(hierarchy, cex = 0.4)

hierarchy_clustering <- cutree(hierarchy, k = 2)
rect.hclust(hierarchy, k = 2, border = "red")
accuracy_table <- table(hierarchy_clustering, y)
err <- 1 - (sum(diag(accuracy_table)) / sum(accuracy_table))
