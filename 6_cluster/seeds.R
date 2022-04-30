library(cluster)

data <- read.table("data/seeds_dataset.txt", sep = "\t")
colnames(data) <-
  c("Area", "Perimeter", "Compact", "Len", "Width", "Asymm", "EarLen", "Var")
data <- data[order(runif(nrow(data))), ]

kmeans_cluster <- kmeans(subset(data, select = -Var), 3)
kmedoids_cluser <- clara(subset(data, select = -Var), 3)

plot(data$Area, col = kmeans_cluster$cluster)
plot(data$Perimeter , col = kmeans_cluster$cluster)
plot(data$Compact , col = kmeans_cluster$cluster)
plot(data$Len , col = kmeans_cluster$cluster)
plot(data$Width , col = kmeans_cluster$cluster)
plot(data$Asymm , col = kmeans_cluster$cluster)
plot(data$EarLen , col = kmeans_cluster$cluster)

plot(data$Area, col = kmedoids_cluser$cluster)
plot(data$Perimeter , col = kmedoids_cluser$cluster)
plot(data$Compact , col = kmedoids_cluser$cluster)
plot(data$Len , col = kmedoids_cluser$cluster)
plot(data$Width , col = kmedoids_cluser$cluster)
plot(data$Asymm , col = kmedoids_cluser$cluster)
plot(data$EarLen , col = kmedoids_cluser$cluster)
