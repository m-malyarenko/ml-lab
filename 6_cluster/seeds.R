library(cluster)

data <- read.table("data/seeds_dataset.txt", sep = "\t")
colnames(data) <-
  c("Area", "Perimeter", "Compact", "Len", "Width", "Asymm", "EarLen", "Var")
data <- data[order(runif(nrow(data))), ]

kmeans_cluster <- kmeans(subset(data, select = -Var), 3)
kmedoids_cluser <- clara(subset(data, select = -Var), 3)

kmeans_cluster$tot.withinss
kmedoids_cluser$diss

plot(data$Area, col = kmeans_cluster$cluster, main = "k-means")
plot(data$Perimeter , col = kmeans_cluster$cluster, main = "k-means")
plot(data$Compact , col = kmeans_cluster$cluster, main = "k-means")
plot(data$Len , col = kmeans_cluster$cluster, main = "k-means")
plot(data$Width , col = kmeans_cluster$cluster, main = "k-means")
plot(data$Asymm , col = kmeans_cluster$cluster, main = "k-means")
plot(data$EarLen , col = kmeans_cluster$cluster, main = "k-means")

plot(data$Area, col = kmedoids_cluser$cluster, main = "k-medoids")
plot(data$Perimeter , col = kmedoids_cluser$cluster, main = "k-medoids")
plot(data$Compact , col = kmedoids_cluser$cluster, main = "k-medoids")
plot(data$Len , col = kmedoids_cluser$cluster, main = "k-medoids")
plot(data$Width , col = kmedoids_cluser$cluster, main = "k-medoids")
plot(data$Asymm , col = kmedoids_cluser$cluster, main = "k-medoids")
plot(data$EarLen , col = kmedoids_cluser$cluster, main = "k-medoids")
