# Linear and Quadratic Discriminant Analyses Example 


#############################################


# I. Data Loading and Preparation

# The purpose of linear discriminant analysis (LDA) is to find the linear combinations of the original variables that gives the best possible separation between the groups.
# The idea behind discriminant analysis is to study the (conditional) posterior distribution of the random variable y=k (class labeling for class membership, k=1,2,...K) given the data.
# With k groups you will need k-1 discriminators for classification.
# We know by Bayes rule that p(y|x) is (p(x|y)*p(y))/p(x), where p(y) is the prior distribution, and p(x)=integral(p(x|y)*p(y))dy, so the key is to study the form of p(x|y).
# In other words, to understand the probability of the y=k (belonging to the kth group), the key is to understand probability of x given the membership y=k.
# Linear discriminant analysis thus assumes that the conditional density p(x|y) is normally distributed with respective means and covariances (x|y=k ~ N(miu(k), V(k)), while n>K).
# This is a convenient assumption on p(x|y), and of course, p(x|y) can be modeled differently using other distributions or nonparametrically.
# We will use the iris data as an illustrative example for discriminant analysis. 
# Specifically, we'd like to to use sepal's length, width, petal's length and width to predict 'species'.

PackageTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
PackageTest('MASS')
library(MASS)
data(iris)
head(iris)
tail(iris)
class(iris)
names(iris)
nrow(iris) # count the number of observations
ncol(iris) # count the number of attributes
str(iris)
summary(iris)

# We see that the variable "Species" is categorical, so we do a frequency analysis on the variable.
# The reason here is that we need to assign a prior probability for membership (usu. uniform).
# We also briefly examine the distribution of other variables to better understand the data structure. 

PackageTest('plyr')
library(plyr)
proc.freq <- function(var, dataset){
             y=count(dataset, var)
             y$percent <- y$freq/sum(y$freq)
             return(y)
             }
proc.freq('Species', iris)
hist(iris$Petal.Width, col=c('maroon'), main='Petal Width Histogram', xlab='Petal Width Records')


# II. Linear Discriminant Analysis

# We now call the lda() function to conduct linear discriminant analysis (LDA), which is to used to label data into non-overlapping K classes.
# The function lda() requires at least 3 arguments (formula, data and prior).
# The function prints discriminant functions based on centered (not standardized) variables.
# The 'prior' argument sets the prior probabilities of class membership. 
# If unspecified, the class proportions for the training set are used. 
# If present, the probabilities should be specified in the order of the factor levels.
# Note that the results of the analysis are calculated so that the within-group variance of each discriminant function for each group is equal to 1.
# Note also that it doesn’t matter whether the input variables for linear discriminant analysis are standardized or not, unlike for principal components analysis in which it is often necessary to standardize the input variables.
# But for convenience, in linear discriminant analysis, the standardised version of an input variable is defined so that it has mean zero and within-groups variance of 1.
# Although the loadings for the group-standardized variables are easier to interpret than the loadings for the unstandardized variables, the values of the discriminant function are the same regardless of whether we standardize the inputs.
# Finally, the predict() function in the 'MASS' package helps classify multivariate observations in conjunction with lda() while projecting data onto the linear discriminants. 
# The function lda() has a default setting s.t. prior probabilities of the classes are the proportions in the training set or what was set in the call to lda(). 

iris.lda <- lda(formula=Species ~., data=iris, prior=c(1/3, 1/3, 1/3)) # The period sign "." in the 'formula' argument means to use all the remaining variables in data as covariates
names(iris.lda)
iris.lda # this means the 1st linear discriminant function is given by 0.8294*Sepal.Length + 1.5345*Sepal.Width - 2.2012*Petal.Length - 2.8105*Petal.Width
plot(iris.lda) # Scatter plot using the 1st two discriminant dimensions 
plda <- predict(object=iris.lda, newdata=iris)
plda
names(plda)

# The 'newdata' is a new training dataset (data frame) whose values are to be predicted (classified). 
# Missing values in newdata are handled by returning NA if the linear discriminants cannot be evaluated.


# III. Quadratic Discriminant Analysis

# To obtain a quadratic discriminant function use qda( ) instead of lda( ). 
# Quadratic discriminant function does not assume homogeneity of variance-covariance matrices.

iris.qda <- qda(formula=Species ~., data=iris, prior=c(1/3, 1/3, 1/3)) # The period sign "." in the 'formula' argument means to use all the remaining variables in data as covariates
names(iris.qda)
iris.qda
pqda <- predict(object=iris.qda, newdata=iris)
pqda

# References:
# http://www.statmethods.net/advstats/discriminant.html
# http://www.r-bloggers.com/computing-and-visualizing-lda-in-r/
# http://www.r-tutor.com/elementary-statistics/quantitative-data/histogram
# https://media.readthedocs.org/pdf/little-book-of-r-for-multivariate-analysis/latest/little-book-of-r-for-multivariate-analysis.pdf