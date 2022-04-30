library(cluster)
data("votes.repub")

# Plot clustering dendogram
plot(agnes(votes.repub))