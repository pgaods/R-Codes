# Standardizing Multivariate Distributions Example


#############################################


# I. The Goal

# Given a multivariate distribution, many times we need to standardize them and visualize a multivariate distribution.
# Many statistical and machine learning procedures such as principal component analysis and discriminant analysis often deal with multivariate distributions.
# To perform these advanced techniques, it is often important to standardize them first and visualize the multivariate distribution.
# We will use the dataset 'wine' as an illustrative example that can be retrieved from "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data".
# The dataset contains concentrations of 13 different chemicals in wines grown in the same region in Italy that are derived from threee different culivars.
# There is one row per wine sample, and the first column contains the cultivar of a wine sample (labeled 1, 2, or 3).
# The subsequent 13 columns contain the concentrations of the 13 different chemicals in that sample. 
# The file has commas as delimiters and has 178 observations with 14 variables in total.

wine <- read.table ("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=",")
str(wine)
mode(wine) # list
class(wine) # data.frame
PackageTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }


# II.  Visualization

# There are many ways of presenting multivariate data visually (such as using R functions and ggplot2 library).
# Here we focus on matrix scatterplots and profile plots (the R libraries involved is 'car', which has a lot of functions for visualization purpose).
# The matrix scatterplot techniques can help show each pair of variables plotted against each other (using the scaterplotMatrix() function).
# The function gives a matrix of pictures with diagonal elements showing the histograms of each of the variables themselves and non-diagonal elements showing pairwise plots.
# Traditional plot() function in R can dig deeper into specific pairwise variable relationships with the help of the text() function labeling the class variable. 

PackageTest("car")
scatterplotMatrix(wine[2:5])
plot(wine$V5, wine$V10)
text(wine$V5, wine$V10, labels=wine$V1, cex=0.8, pos=4, col="blue") # pos=4 indicating text just to the right of hte symbol and cex=0,5 indicating the text at a half the default size

# Another type of plot that is called profile plot that shows the variation in each of the variables by plotting the values for each of the samples.
# To see this, we need to create a user-defined function 'makeProfilePlot()', as R itself does not provide such a function.
# The function essentially brings every variable in one graph and plots it after limiting the axes values and setting colors for each variable.
# Through this graph, you can have a visual idea about the mean and standard deviation of each distribution.

makeProfilePlot <- function(mylist,names) {
                     PackageTest("RColorBrewer")  
                     numvariables <- length(mylist) # find out how many variables we want to include         
                     colours <- brewer.pal(numvariables,"Set1")  # choose ’numvariables’ random colors     
                     mymin <- 1e+20  
                     mymax <- 1e-20
                     for (i in 1:numvariables) {
                       vectori <- mylist[[i]]
                       mini <- min(vectori)
                       maxi <- max(vectori)
                       if (mini < mymin) {mymin <- mini}
                       if (maxi > mymax) {mymax <- maxi} 
                     }  
                     for (i in 1:numvariables) {
                       vectori <- mylist[[i]]
                       namei <- names[i]
                       colouri <- colours[i]
                       if (i == 1) {plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax))}
                       else {points(vectori, col=colouri,type="l")}
                       lastxval <- length(vectori)
                       lastyval <- vectori[length(vectori)]
                       text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
                     }
                   }
names <- c("V2","V3","V4","V5","V6")
mylist <- list(wine$V2,wine$V3,wine$V4,wine$V5,wine$V6)
makeProfilePlot(mylist,names)


# III. Correlation Analysis

# Besides looking at pictures of multivariate distributions, it's important to do a correlation analysis.
# To check correlation between two distributions, the cor.test() function will suffice.
# The function calculates the Pearson's product-moment correlation (including t value, degrees of freedom, sample estimate and p-values etc.).
# The null hypothesis is that the two underlying distributions are not correlated (i.e., rho=0 in population), and small p-values reject the null hypothesis.
# Note that the test is non-robust and not outlier-resistant, and fat-tailed distribution will influence the test results easily.

cortest <- cor.test(wine$V2, wine$V3)
cortest
names(cortest) # "statistic" "parameter" "p.value" "estimate" "null.value" "alternative" "method" "data.name" "conf.int"  
cortest$p.value

# Another common procedure we need to use for correlation is to examine the pairwise correlation for every combinations of two variables, and then rank them.
# The cor() function itself produces a matrix of correlation coefficients but it doesn't give you a ranking of correlations.
# We can create a user-defined function pw.cor() that will print out the linear correlation coefficients for each pair of variables in your data set, in order of the correlation coefficient. 
# This lets you see very easily which pair of variables are most highly correlated.
cor(wine)
pw.cor <- function(mydataframe,numtoreport) {
            cormatrix <- cor(mydataframe) # find the correlations
            diag(cormatrix) <- 0 # set the correlations on the diagonal or lower triangle to zero so they will not be reported as the highest ones
            cormatrix[lower.tri(cormatrix)] <- 0
            fm <- as.data.frame(as.table(cormatrix)) # flatten the matrix into a dataframe for easy sorting
            names(fm) <- c("First.Variable", "Second.Variable", "Sample.Correlation") # assign human-friendly names
            staging <- head(fm[order(abs(fm$Sample.Correlation),decreasing=T),],n=numtoreport) # sort and print the top n correlations
            print(staging, row.names=FALSE) # printing without row numbers
              }
pw.cor(wine[, 2:14], 8)


# IV. Normalization of Multivariate Distribution

# We now learn how to standardize variables (demeaning and scaling by standard deviation) by using the scale() function.
# The syntax for the function is scale(x, center=TRUE, scale=TRUE), where x is the numeric matrix or a data frame.
# The 'center' and 'scale' arguments are both logical arguments or numeric vectors of length equal to the number of columns of x. 
# If 'center' is a numeric vector with length equal to the number of columns of x, then each column of x has the corresponding value from center subtracted from it. 
# If 'center' is TRUE, then centering is done by subtracting the column means (omitting NAs) of x from their corresponding columns.
# If 'center' is FALSE, no centering is done.
# If 'scale' is a numeric vector with length equal to the number of columns of x, then each column of x is divided by the corresponding value from it.
# If 'scale' is TRUE then scaling is done by dividing the (centered) columns of x by their standard deviations if 'center' is TRUE, and the root mean square otherwise. 
# If 'scale' is FALSE, no scaling is done.

m <- matrix(1:10, ncol=2);
m
demeaned.m <- scale(x, center=TRUE, scale=FALSE) # demeaned vector
demeaned.m
standardized.m <- scale(x, center=TRUE, scale=TRUE) # standardized variables
standardized.m

# For the wine data and some practical applications, we can standardize the variables and then check if the standardization works by using the sapply() function.
# The sapply() function is a user-friendly version and wrapper of lapply() by default returning a vector, matrix, or an array
# The lapply() function essentially returns a list of the same length as the original object, each element of which is the result of applying the function to the corresponding elements (basically a group-by statement).
# The key difference is that lapply() returns a list while sapply() returns, and they are both different to tapply().

wine2 <- as.data.frame(scale(wine[2:14]))
sapply(wine2, mean)
sapply(wine2, sd)
lapply(wine2, mean)
lapply(wine2, sd)
class(sapply(wine2, mean)) # numeric
class(lapply(wine2, mean)) # list
mode(sapply(wine2, mean)) # numeric
mode(lapply(wine2, mean)) # list


# References:
# https://media.readthedocs.org/pdf/little-book-of-r-for-multivariate-analysis/latest/little-book-of-r-for-multivariate-analysis.pdf
# http://support.sas.com/documentation/cdl/en/procstat/63104/HTML/default/viewer.htm#procstat_corr_sect013.htm