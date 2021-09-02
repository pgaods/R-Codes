# Data Validation Using Domain Knowledge Example


#############################################


# I. The Goal

# Domain knowledge sometimes can help us perform data validation for an existing dataset.
# For example, if you have a dataset that contains demographic information with one record indicating age=2000 for a human being, you know the record has a data issue.
# R provides a package called 'validate' to perform such type of validation.
# The package allows you to test your datasets against predefined rules, import or export rule sets from structured or free-format files, and do basic rule maintenance tasks.
# The package also helps you investigate and visualize the results of a data validation step. 
# There are two main important concepts involved with this package: 'validator' and 'confrntation'.
# A 'validator' is an object representing a set of rules your data must satisfy.
# A 'confrontation' is an object representing the results of confronting data with rules. 
# The word 'confront' here in this particular setting means to evaluate the validation rules in the context of one or more datasets.
# We will use the dataset 'women' as an illustrative example. 

PackageTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
PackageTest("validate")
library("validate")
women #  a small dataset with 15 records
class(women)
names(women)
summary(women)


# II. Validation and Visualization 

# Validating data is all about checking whether a data set meets presumptions or expectations you have about it.
# The 'validate' package makes it easy for you to define those expectations.
# The check_that() function implements this task by letting you specify the data source, and a list of criteria (expressions) to be evaluated.
# The output of the summary result of the check_that() function gives you all the details of the check.
# These include rules (V1, V2, V3, ...etc.), number of observations checked, the pass/failure counts, and whether the check resulted in an error.

checksheet <- check_that(women, height>0, weight>0, height/weight>0.5)
checksheet # the number of confrontations is the number of the rules we specified in total to be checked
summary(checksheet) # this command gives the best result of the check in words
barplot(checksheet, main="Validation Checks on the Current Data")

# The validator object has stored the rule and assigned names to them for future reference. 
# To check this, we can confront the data set with the validation rules we've just defined.

criteria <- validator(height > 0, weight > 0, height/weight > 0) # This gives us 
criteria
cf <- confront(women,criteria)
cf
aggregate(cf,by='record') # this gives checks record by record on aggregation


# III. Two Useful functions in base R

# Besides all of the above functionalitiesin the package, a useful primitve function in base R is the any() function.
# The any() function determines whether at least one of the values is true, given a set of logical vectors.
# The syntax for the function is any(..., na.rm=FALSE).
# Its main argument is zero or more logical vectors, and the na.rm determines whether we remove NA before the result is computed.
# Similarly, the all() function is the complement of any(), with exactly the same syntax.

vec <- c(1,3,4)
any(vec>5)
all(vec<5)


# References:
# https://cran.r-project.org/web/packages/validate/vignettes/intro.html
