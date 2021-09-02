# K-Nearest Neighbors (KNN) Example 


#############################################


# I. Data Loading

data(iris)
head(iris)
tail(iris)
class(iris)
names(iris)
nrow(iris) # count the number of observations
ncol(iris) # count the number of attributes
PackageTest <- function(x){
                 if (!require(x,character.only = TRUE)){
                   install.packages(x,dep=TRUE)
                   if(!require(x,character.only = TRUE)) stop("Package not found")
                 }
               }
PackageTest("plyr")
PackageTest("class")
PackageTest("gmodels")


# II. Exploratory Data Analyses

str(iris) # proc content
summary(iris) # only Species is a categorical variable
library(plyr)
proc.freq <- function(var, dataset){
             y=count(dataset, var)
             y$percent <- y$freq/sum(y$freq)
             return(y)
             }
proc.freq('iris$Sepal.Length', iris)
proc.freq('iris$Sepal.Width', iris)
proc.freq('iris$Petal.Length', iris)
proc.freq('iris$Petal.Width', iris)
proc.freq('iris$Species', iris)


# III. K-Nearest Neighbors

# The K-nearest neighbor method uses observations in the training set closest in input space to x to the predicted value yhat .
# In order to assess the model's performance later, we need to divide the data set into two disjoint parts: a training set and a test set. 
# The first is used to train the system, while the second is used to evaluate the learned or trained system. 
# The most common splitting choice is to take 2/3 of your original data set as the training set, while the 1/3 that remains will compose the test set.

set.seed(1234)
s <- sample(x=2, size=nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
s # the x is set to be 2 here to divide the dataset into 2 parts (training and test)

# The sample() function takes a sample of the specified size from the elements of x using either with or without replacement. 
# The syntax for sample() is sample(x, size, replace = , prob = ).
# The argument 'x' is either a vector of one or more elements from which to choose, or a positive integer.
# If x has a length of one (and numeric) and x>=1,then sampling via the sample() function means it takes place from 1:x.
# Otherwise, x can be any R objects.
# The argument 'size' is the sample size.
# The optional argument 'replace' dictates whether the sample is drawn with or without replacement (default=FALSE).
# The optional argument 'prob' is a vector of probability weights for obtaining the elements of the vector being sampled.

iris.training <- iris[s==1, 1:4] # drawing the first 4 columns for training dataset
iris.test <- iris[s==2, 1:4] # drawing the first 4 columns for the test dataset
iris.trainLabels <- iris[s==1, 5] # drawing the last column
iris.testLabels <- iris[s==2, 5]

# The knn() function in the 'class' library takes as arguments the training set, the test set, the train labels and the amount of neighbours you want to find with this algorithm. 
# The result of this function is a factor vector with the predicted classes for each row of the test data. 
# The paramter k is usually set to be odd and the distance function is implicitly assumed to be Euclidean distance.
# The knn() function's default setting is that all distances equal to the kth largest are included.
# If we want to change the setting, we can specify the optional argument use.all to be FALSE. 

library(class)
predict1 <- knn(train=iris.training,test=iris.test, cl=iris.trainLabels, k=3)
predict1 # this gives labels
predict2 <- knn(train=iris.training,test=iris.test, cl=iris.trainLabels, k=3, prob=T)
predict2 # this gives probabilities

# Now we want to analyze the degree of correctness of the model's predictions.
# We compare what is actually labeled in the data vs. what is predicted by our model.
eval <- data.frame(iris.testLabels, predict1)
eval$flag <- ifelse(eval$iris.testLabels==predict1, 1, 0) # creating a flag to indicate correct prediction
eval
prediction.rate <- sum(eval$flag)/nrow(eval)
prediction.rate
PackageTest("gmodels")
library(gmodels)
CrossTable(x = iris.testLabels, y = predict1, prop.chisq=FALSE) # cross-tabulation analysis


# References: 
# https://www.datacamp.com/community/tutorials/machine-learning-in-r