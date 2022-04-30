n_a <- runif(1, 300, 500)
n_b <- runif(1, 300, 500)
n_c <- runif(1, 300, 500)

points_a <- data.frame(
  x = rnorm(n_a, mean = 0, sd = 40),
  y = rnorm(n_a, mean = 0, sd = 3)
)

points_b <- data.frame(
  x = rnorm(n_b, mean = 0, sd = 40),
  y = rnorm(n_b, mean = 30, sd = 3)
)

points_c <- data.frame(
  x = rnorm(n_c, mean = 0, sd = 40),
  y = rnorm(n_c, mean = 60, sd = 3)
)

points <- rbind(
  points_a,
  points_b,
  points_c
)

plot(
  x = points$x,
  y = points$y,
  
  xlab = "x",
  ylab = "y",
  pch = 19,
  cex = 0.5
)

# K-medoid clustering
library(cluster)

metric_values <- c("euclidean", "manhattan")

for (metric in metric_values) {
  for (use_stand in c(TRUE, FALSE)) {
    clustering <- clara(
      points,
      3,
      metric = metric,
      stand = use_stand
    )
    
    print(clustering$clusinfo)
    plot(
      points,
      
      col = clustering$clustering,
      
      xlab = "x",
      ylab = "y",
      main = paste(
        "Clustering, metric: ",
        metric,
        ", standartisation: ",
        use_stand,
        sep = ""
      ),
      pch = 19,
      cex = 0.5
    )
  }
}
