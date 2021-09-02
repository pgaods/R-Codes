# Merging and Joining Datasets Example 


#############################################


# I. Traditional Way 

Genus <- c('Acer', 'Acer', 'Ajuga', 'Conyza', 'Lamium')
Species <- c('platanoides', 'palmatum', 'reptum', 'sumatrensis', 'album')
Lifeform <- c('tree', 'tree', 'herb', 'annual', 'herb')
dsn1 <- data.frame(Genus, Species, Lifeform)
Genus <- c('Acer', 'Ajuga', 'Brassica', 'Camerion', 'Conyza', 'Lamium')
Species <- c('platanoides', 'reptum', 'napus', 'angustifolium', 'bilbaoana', 'album')
Flowering <- c('May', 'June', 'April', 'July', 'August', 'January')
dsn2 <- data.frame(Genus, Species, Flowering)

dsn3 <- merge(dsn1, dsn2) # inner join
dsn4 <- merge(dsn1, dsn2, all.x=T) # merge if a in SAS
dsn5 <- merge(dsn1, dsn2, all.y=T) # merge if b in SAS
dsn6 <- merge(dsn1, dsn2, all=T) # merge if a or b in SAS
dsn7 <- merge(dsn1, dsn2, by=NULL) # Cartesian join in SAS
dsn8 <- merge(dsn1, dsn2, by.x='Genus', by.y='Genus') # inner join on 'Genus'
dsn9 <- merge(dsn1, dsn2, by.x='Genus', by.y='Genus', all.x=T) # left outer join on 'Genus'
dsn10 <- merge(dsn1, dsn2, by.x='Genus', by.y='Genus', all.y=T)  # right outer join on 'Genus'
dsn11 <- merge(dsn1, dsn2, by.x='Genus', by.y='Genus', all.x=T, all.y=T) # outer join on 'Genus'

dsn1
dsn2
dsn3
dsn4
dsn5
dsn6
dsn7
dsn8
dsn9 
dsn10
dsn11

# dsn3 is the result of an inner join.
# Without specifying anything, the merge() function does an inner join with complete entries from both tables.
# Without the by-variable statement like SAS, the function figures out the by variable on its own (common variables in both datasets).
# dsn4 is equivalent to 'merge dsn1 (in=a) dsn2 (in=b)... by &key.... if a...' statement in SAS.
# dsn5 is equivalent to 'merge dsn1 (in=a) dsn2 (in=b)... by &key.... if b...' statement in SAS.
# dsn6 is equivalent to 'merge dsn1 (in=a) dsn2 (in=b)... by &key.... if a or b...' statement in SAS.
# dsn7 is the same as a Cartesian join.
# dsn8 is a join that only merges on Genus (inner join, but if there are columns with the same name they will be named differently in the new table).
# dsn9 is a left outer join like dsn4 but merging on only one variable (Genus).
# dsn10 is a right outer join like dsn5 but merging on only one variable (Genus).
# Note that for left and right joins, the number of observations for the final table does not need equal the number of the observations in the base tables.
# dsn11 is the outer join on only one variable.


# II. The SQL Way

PackageTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
PackageTest("plyr")
library(plyr)
blah1 <- join(dsn1, dsn2, type = "inner") #same as dsn3
blah2 <- join(dsn1, dsn2, type='left') # same as dsn4
blah3 <- join(dsn1, dsn2, by='Genus', type='right') # same as dsn10

# The require() function tries to load the package using the library() function and returns a logical value indicating sucess/failure.
# So the library() function loads the package and require() tries to load the package. 