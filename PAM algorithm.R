# Partitioning Around Medoids (PAM) Algorithms


# I. Explorative Data Analysis on the Training Data


# The PAM algorithm was developed by Leonard Kaufman and Peter J. Rousseeuw, and this algorithm is very similar to K-means, mostly because both are partitional algorithms.
# In other words, both break the dataset into groups (clusters), and both work by trying to minimize the error, but PAM works with medoids, which are an entity of the dataset representing the group in which it is inserted.
# In contrast, K-means works with centroids, which are artificially created entity that represent its cluster.
# The PAM algorithm can work over two kinds of input.
# The first is the matrix representing every entity and the values of its variables.
# The second is the dissimilarity matrix directly in which the user can provide the dissimilarity directly as an input to the algorithm, instead of the data matrix representing the entities. 
# Either way the algorithm reaches a solution to the problem, in a general analysis the algorithm proceeds this way:
# 1) Choose K entities to become the medoids, or in case these entities were provided use them as the medoids.
# 2) Calculate the dissimilarity matrix if it was not informed.
# 3) Assign every entity to its closest medoid
# 4) For each cluster search if any of the entities of the cluster lower the average dissimilarity coefficient, if it does select the entity that lowers this coefficient the most as the medoid for this cluster.
# 5) If at least one medoid has changed go to (3), else end the algorithm.


training <- read.csv("H:/PGao Documents/quick copy/PAM_data.txt", sep="", header=F)
training
class(training)
training$object <- training$V1
training$x1 <- training$V2
training$x2 <- training$V3
training$V1 <- NULL
training$V2 <- NULL
training$V3 <- NULL
training$object <- NULL
training

library(cluster)
library(ggplot2)
library(plyr)


# II. Output and Interpretation

result <- pam(x=training, k=2, diss=FALSE, metric="euclidean") # you cannot have indexes (it should only contain feature vectors)
result
summary(result)
dev.new()
plot(result$data, col = result$clustering, main='Clustering Result')
points(result$medoids, col = 1:2, pch = 4)


# References:
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/Partitioning_Around_Medoids_(PAM) 
# https://en.wikipedia.org/wiki/Silhouette_(clustering)

