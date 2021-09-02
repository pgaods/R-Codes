# Decision Trees 


#############################################


# I. Introduction and Data Preparations

# Decision tree methods essentially stratify or segment the predictor space into a number of simple regions. 
# In order to make a prediction for a given observation, we typically use the mean or the mode of the training observations in the region to which it belongs.
# Traditional tree-based methods are not as competitive as the best supervised learning approaches in terms of prediction accuracy, but they are easy to interpret and their performance can be enhanced using bagging, random forests, and boosting.  
# In R, the packages 'tree' and 'rpart' handle the CART algorithn, while the library 'party' provides nonparametric regression trees for nominal, ordinal, numeric, censored, and multivariate responses. 
# We focus on the CART algorithm and we then later move onto enhancement methods (boosting, bagging and random forests).

PackageTest <- function(x){
                 if (!require(x,character.only = TRUE)){
                   install.packages(x,dep=TRUE)
                   if(!require(x,character.only = TRUE)) stop("Package not found")
                 }
               }
PackageTest("ggplot2")
PackageTest("plyr")
PackageTest("gridExtra")
PackageTest("tree")
PackageTest("rpart")
PackageTest("MASS")
PackageTest("ISLR")
PackageTest("randomForest")
PackageTest("gbm")


# II. Regression Trees

# As a first example, we will use the "Hitters" dataset to predict a baseball player's salary based on years (the number of professional years) and the number of hits that he made in the previous years.
# We first do some explorative analysis, then apply regression tree to the problem using different R packages.

data(Hitters)
summary(Hitters)
str(Hitters)
print(Hitters[1:10, ])

# We now create a function that does a proc frequency analysis on all of the factors in a data frame automatically.
# This user-defined function first identifies all factor elements in a data frame, then apply the proc.freq function to each factor.
# We also create a function to report all variables that have a missing value in a data frame, and then we get the cleaned version of the data without any missing values.

proc.freq.allfactors <- function(dataframe){
                          library(plyr)
                          proc.freq <- function(var, dataset){
                                         y=count(dataset, var)
                                         y$percent <- y$freq/sum(y$freq)
                                         return(y)
                                       }
                          factor_set <- as.list(sapply(dataframe, is.factor))
                          factor_set2 <- names(which(factor_set==TRUE)) # extracting all the factors' names
                          result <- lapply(factor_set2, proc.freq, dataset=dataframe) # the result must be a list (sapply will not work here)
                          if(length(factor_set2)==0){
                            print("No variables are factors in the data frame")
                          }
                          else {
                            print("These are the factors in the current dataset:")
                            print(factor_set2)
                            result
                          }
                        }
proc.freq.allfactors(Hitters)
proc.missing <- function(dataframe) {
                  sapply(dataframe, function(x) sum(is.na(x)))   
                }
proc.missing(Hitters) # only the variable 'Salary' has 59 records having missing values
Hitters<-Hitters[which(is.na(Hitters$Salary)==FALSE), ]

# We now use the 'rpart' package to apply the CART algorithm to do regression trees. 
# The CART algorithm can handle both regression and classification trees, and is one of the most widely used algorithms in industries.

CART.reg1 <- rpart(formula=Salary~., data=Hitters, method='anova') # using all variables (263 records)
print(CART.reg1) # this displays the tree strcture in words

# The rpart() function has an optional argument 'na.action', whose default action is to delete all observations for which the response variable is missing, but keeps those in which one or more predictors are missing. 
# The rpart.control() function controls various parameter dictating all aspects of the 'rpart' fit.
# The default is that we do 10-fold cross validation, and the minimum number of observations that must exist in a node in order for a split to be attempted must be 20. 
# There are other default parameters and users with no advanced training in statistics are not recommended to toy with them.

printcp(CART.reg1) # this display the cost complexity parameters sequence (the tree stops splitting when the CP value is smaller than a threshold)
summary(CART.reg1) # this provides detailed results including surrogate splits and the variable importance
rpart.plot <- function(tree.object, cex, color, dataset_name) {
                dev.new()
                plotcp(tree.object) # plotting cross validation result
                dev.new()
                plot(tree.object, main=paste('Regression Tree for', dataset_name))
                text(tree.object, cex=cex, col=color) # the cex should be small if your tree is large (default value is 1)
              }
rpart.plot(tree.object=CART.reg1, cex=0.7, color='blue', dataset_name='Hitters')

# We now prune the tree to avoid overfitting the data, which requires us to select a tree size that minimizes the cross validation error (which is the 'xerror' column printed by printcp() function).
# Once we selected the pruned tree, we redo the previous step using the optimal CP (complexity parameter).

opt <- CART.reg1$cptable[which.min(CART.reg1$cptable[,"xerror"]),"CP"] # this selects the optimal value for Cp
opt
CART.reg1.pruned <-prune(CART.reg1, cp=opt)
print(CART.reg1.pruned)
printcp(CART.reg1.pruned) # display the cost complexity factor
summary(CART.reg1.pruned)
rpart.plot(tree.object=CART.reg1.pruned, cex=0.7, color='blue', dataset_name='Hitters')

# We now do the prediction and calculate the training error (recall training errors refers to in-sample error and test error refers to the out-of-sample error).
# Strategically, we will create the yhat (estimated values) and compare it with the actual values through root mean squared error.

yhat <- predict(CART.reg1.pruned, type='vector')
y <- Hitters$Salary[is.na(Hitters$Salary)==FALSE] # getting rid of the missing values
dev.new()
plot(yhat, y, main='Examing the the predicted value vs. the actual value')
abline(0,1)
MSE <- mean((yhat-y)^2) # 69320.74
sqrt(MSE) # this means that the model leads to a training prediction that are within around 285,000 dollars of true salary value for the player

# As a second example, we will use the 'Boston' data in the 'MASS' library, but this time we use the 'tree' package rather than the 'rpart' package.
# The 'Boston' dataset contains housing values in the suburbs of Boston, and our goal is to predict the medium house value 'medv'.
# The variable 'lstat' measures the percentage of individuals with lower socieconomic status.
# Before implementing CART, we will create a training dataset and a test dataset (50/50) so we can do CV later and calculate test error.

library(MASS)
data(Boston)
summary(Boston)
str(Boston)
print(Boston[1:10, ])
proc.freq.allfactors <- function(dataframe){
                          library(plyr)
                          proc.freq <- function(var, dataset){
                                         y=count(dataset, var)
                                         y$percent <- y$freq/sum(y$freq)
                                         return(y)
                                       }
                          factor_set <- as.list(sapply(dataframe, is.factor))
                          factor_set2 <- names(which(factor_set==TRUE)) # extracting all the factors' names
                          result <- lapply(factor_set2, proc.freq, dataset=dataframe) # the result must be a list (sapply will not work here)
                          if(length(factor_set2)==0){
                            print("No variables are factors in the data frame")
                          }
                          else {
                            print("These are the factors in the current dataset:")
                            print(factor_set2)
                            result
                          }
                        }
proc.freq.allfactors(Boston)
proc.missing <- function(dataframe) {
                  sapply(dataframe, function(x) sum(is.na(x)))   
                }
proc.missing(Boston) # no missing values in any variables
set.seed(1)
train <- sample(x=1:nrow(Boston), size=nrow(Boston)/2) # this is a vector

# We now use the 'tree' package to apply the CART algorithm to do regression trees rather than the 'rpart' package.. 
# In the context of a regression tree, the deviance is simply the sum of squared errors for the tree. 

CART.reg2 <- tree(medv ~ ., Boston, subset=train)
print(CART.reg2)
summary(CART.reg2) # only three variables have been used in constructing the tree
tree.plot <- function(tree.object, cex, color, dataset_name, line) {
               dev.new()
               plot(tree.object)
               text(tree.object, cex=cex, col=color) # the cex should be small if your tree is large (default value is 1)
               title(paste('Regression Tree for', dataset_name), line=line) # the title() function controls the position of the title
             }
tree.plot(tree.object=CART.reg2, cex=0.6, color='brown', dataset_name='Boston', line=0.5) # line=0 means bringing up the title

# We now prune the tree to avoid overfitting the data, which requires us to select a tree size that minimizes the cross validation error (which is the 'xerror' column printed by printcp() function).
# The prune.tree() function helps us see whether pruning the tree will improve performance. 
# The best part of the prune.tree() function is that it allows the user to trim the tree down by directlys specifying the number of terminal nodes.

CART.reg2.pruned <- prune.tree(CART.reg2, best=6) # allowing 6 terminal nodes in the end
CART.reg2.pruned
summary(CART.reg2.pruned)
tree.plot(tree.object=CART.reg2.pruned, cex=0.6, color='orange', dataset_name='Boston', line=0.5) # line=0 means bringing up the title

# We now do the prediction and calculate the test error (recall training errors refers to in-sample error and test error refers to the out-of-sample error).

yhat <- predict(CART.reg2.pruned, newdata=Boston[-train, ], type='vector')
y <- Boston[-train, "medv"]
dev.new()
plot(yhat, y, main='Examing the the predicted value vs. the actual value')
abline(0,1)
MSE <- mean((yhat-y)^2) 
test.error <- sqrt(MSE) 
test.error # this means that the model leads to test predictions that are within around 5310 dollars of the true median home value for the suburb


# III. Classfication Trees



# IV. Bagging

# We now perform bagging, which is a special case of random forests under the framework of tree-based methods (m=K). 

library(randomForest)
set.seed(1)
mtry=ncol(Boston)-1
mtry
bag.boston= randomForest(medv~.,data=Boston , subset=train, mtry=mtry,importance=TRUE)
bag.boston
yhat <- predict(bag.boston, newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
dev.new()
qplot(yhat, boston.test, main='y vs yhat plot', color=I('purple'))+geom_abline()
MSE <- mean((yhat-boston.test)^2)
test.error <- sqrt(MSE)
test.error


# V. Random Forests

# VI. Boosting

# VII. Computational Issues

# From a computation standpoint, the majority of the packages in this module cannot handle predictors that have factor levels more than 32. 
# This is unfortunate because some of these packages like 'tree' and 'randomForest' are written in Fortrann, which do not support long vectors.
# To bypass the problem, one needs to examine the contexts for projects on a case-by-case basis.
# For example, in healthcare data, if one of the predictors iis the diagnosis code which may take on many levels, we can compactify the levels of factors under 32 by using paid amount.
# The idea is to use other information in the dataset to reclassify the groups and compactify them under 32.
# First, we determine the number of occurences (or associated dollar amount) of each level of the factor and rank them.
# For the top 31 levels we leave them as it is, but for the ones after the 31st, we code them into one single factor. 



# References:
# http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Sixth%20Printing.pdf
# https://onlinecourses.science.psu.edu/stat857/node/22
# http://www.stat.wisc.edu/~loh/treeprogs/guide/wires11.pdf
# https://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/ 
# https://www.r-project.org/conferences/useR-2009/slides/Hothorn+Zeileis.pdf
# https://statweb.stanford.edu/~lpekelis/talks/13_datafest_cart_talk.pdf
# http://www.statmethods.net/advstats/cart.html
# https://www.bu.edu/sph/files/2014/05/MorganCART.pdf
# http://stackoverflow.com/questions/10118249/rmore-than-52-levels-in-a-predicting-factor-truncated-for-printout 
