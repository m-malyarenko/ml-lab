library(Rtsne)

data <- read.csv("data/ionosphere.csv", stringsAsFactors = TRUE)

tsne <- Rtsne(data, check_duplicates = FALSE)
plot(
  tsne$Y,
  pch = 21,
  bg = c("red", "blue"),
  
  main = "t-SNE of 'ionosphere' dataset",
  xlab = "",
  ylab = ""
)
