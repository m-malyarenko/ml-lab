library(tree)

data <- read.csv("data/glass.csv")
data <- data[,-1]
data$Class <- as.factor(data$Class)

tree <- tree(
  Class ~ .,
  data = data,
  split = "gini",
)

plot(tree, type = "uniform")
text(tree, cex = 0.7)

print(tree$frame$splits)

# Remove redundant tree nodes
nodes_to_rm <- c(15, 6)
tree <- snip.tree(tree, nodes = nodes_to_rm)

plot(tree, type = "uniform")
text(tree, cex = 0.7)

tree <- prune.tree(tree, k = 7.454439)
plot(tree, type = "uniform")
text(tree, cex = 0.7)

# Classification example
sample <- data.frame(
  RI = 1.516,
  Na = 11.7,
  Mg = 1.01,
  Al = 1.19,
  Si = 72.59,
  K  = 0.43,
  Ca = 11.44,
  Ba = 0.02,
  Fe = 0.1
)

pred <- predict(tree, sample, type = "class")
print(sample)
cat("Prediction class:", pred[1])
