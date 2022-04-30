library(cluster)
data("animals")

# Plot clustering dendogram
plot(agnes(animals))
