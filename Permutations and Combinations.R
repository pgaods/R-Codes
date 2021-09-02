# Simple Combinatorics Example


#############################################


# I. Built-in R functions

factorial(4) # 24=4*3*2*1

# If we want to make an exhaustic enumerative list of an object, we can use the expand.grid() function.
# The function make a draw of n times, each with replacement, so altogether we have n-by-n cases in total.
# It creates a data frame from all combinations of the supplied vectors or factors.

x <- c("x1", "x2", "x3")
x
y1 <- expand.grid(x, x)
y1
dim(y1) # this gives the dimensionality of the grid
y2 <- expand.grid(x, x)
y2
dim(y2)

# The combinatorics function C(n,k) picks k objects from n objects (in which order does not matter).
# The syntax for the combn() functions is combn(x, m, FUN = NULL, simplify = TRUE, ...)
# Here the argument x is the vector source for combinations.
# The argument m is the number of elements to choose.
# The optional argument 'simplify' is logical indicating if the result should be simplified to an array.
# If it's set to be FALSE, the function returns a list. 
# The default (TRUE) gives an array.

z1 <- combn(x, 2, FUN = NULL, simplify = FALSE)
z1
class(z1) # z1 is a list
z2 <- combn(x, 2, FUN = NULL, simplify = TRUE)
z2
class(z2) # z2 is a matrix


# II. Packages for Combinatorics

PackageTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
PackageTest("gtools")
library(gtools)

# The combinations() enumerates the possible combinations of a specified size from the elements of a vector. 
# The permutations() enumerates the possible permutations. 

w <- c('red', 'blue', 'yellow', 'black')
p1 <- permutations(n=4, r=3, v=w, repeats.allowed=T)
p1 # 4*4*4=64
p2 <- permutations(n=4, r=3, v=w, repeats.allowed=F)
p2 # 4*3*2=24
c1 <- combinations(n=4, r=3, v=w, repeats.allowed=T)
c1 # 20
c2 <- combinations(n=4, r=3, v=w, repeats.allowed=F)
c2 # (4*3*2)/(3*2*1)=4



