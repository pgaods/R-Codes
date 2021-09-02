# Support Vector Machine Example


#############################################


# I. Data Loading

# We use support vector machines to perform classification tasks.

PackageTest <- function(x){
                 if (!require(x,character.only = TRUE)){
                   install.packages(x,dep=TRUE)
                   if(!require(x,character.only = TRUE)) stop("Package not found")
                 }
               }
PackageTest("ggplot2")
PackageTest("plyr")
PackageTest("gridExtra")
PackageTest("ISLR")
PackageTest("kernlab")
PackageTest("e1071")

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

# We now create a training dataset and a test dataset and trim the data frame using only 3 variables.
# We want to predict the 'Division' factor using the variables the number of hits, the number of home runs, and the salary.

Hitters1 <- Hitters[, c(2, 3, 15, 19)] # trimming the dataset for only certain variables
sample_size <- floor(0.75*nrow(Hitters1)) # 75% of the data go to training data and 25% go to the test data
set.seed(123)
train_ind <- sample(seq_len(nrow(Hitters1)), size = sample_size)
train <- Hitters1[train_ind, ]
test <- Hitters1[-train_ind, ]


# SVM 

svmfit =svm(Division~., data=train, kernel ="linear", cost=0.1, scale=FALSE)
summary(svmfit)
plot(svmfit, train, formula=Hits~Salary)



# References:
# http://statweb.stanford.edu/~tibs/ElemStatLearn/
# https://escience.rpi.edu/data/DA/svmbasic_notes.pdf
# http://www.di.fc.ul.pt/~jpn/r/svm/svm.html
