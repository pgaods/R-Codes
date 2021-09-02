# Creating Dummy Variables Example


#############################################


PackageTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }

PackageTest("dummies")
letters <- c( "a", "a", "b", "c", "d", "e", "f", "g", "h", "b", "b" )
length(letters)
d1 <-dummy(as.character(letters) )
d2 <-dummy(letters[1:6])
d1
d2
properties <- c(class(d1), class(d2))
properties # both d1 and d2 are matrices
