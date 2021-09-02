# Common Data Manipulation Techniques for Structured Data 

#############################################


# I. Data Loading

# The topics of Data Manipulation strictly speaking pertains to the realm of text mining.
# In text mining, there are two big topics-mining structured data and mining unstructured data.
# Structured data here means that the data are given in a way that can be put into a table, like in SQL.
# Examples of unstructured data include a document with only words, or files with free formats etc. 
# There are many text mining packages, such as 'tm' or 'reader', and in addition, R itself provides many basic functions for dealing with different needs for text mining.
# A good overview paper on text mining is "Text Mining Infrastructure in R(2008)"
# From now on, we mainly focus on structured data. 

# We first define where we want to put the file. 
# The getwd() function returns the default filepath representing the current working directory of the R process.
# For simplicity, we can put all the files we need for this session there in the default filepath.
# Alternatively we can also use the setwd() function to reset the default library.

getwd() 
setwd("G:/PGZ Documents/Statistics Workshop/PDF textbooks/Machine Learning/The Elements of Statistical Learning/Datasets for Elements of Statistical Learning")

# There are different kinds of file formats, each corresponding to a read-in function.
# For example, to read delimited files, we need to use the read.delim() function.
# To read in csv files, we need use the read.csv() function etc.
# One can also read in SAS datasets, in that case we need the read.sas7bdat() function from the 'sas7bdat' library.
# There are other packages made to read in varieties of files (e.g. xls, xlsx, even pdf etc.), but not we focus mainly on flat files (e.g. csv, txt etc.).
# Once we read in the data, we can do an overall summary over the data frame to gain basic insights on the data structure.

r <- read.delim("APRDRGDescription.csv", header=T, sep="|") # sep denotes the limiter (here in the example is the pipe)
r[1:10, ] # take a look at the first 10 records
class(r) # making sure it's a data frame
names(r)
str(r) # note the class of each variable (e.g. 3 levels of effdate)
summary(r)

# Here is another example reading csv files.
# The dataset is an ETF dataset that has price information on them.

etf <- read.csv("G:/PGZ Documents/Statistics Workshop/PDF textbooks/Machine Learning/The Elements of Statistical Learning/Datasets for Elements of Statistical Learning/ETF database.csv")
names(etf)
str(etf)
summary(etf)

# Notice that by default, strings in the data are automatically converted to factors when we use the read.csv() function.
# The default setting means that all the text columns will be treated as factors.
# To override the default setting, you can choose to set the option stringsAsFactors=FALSE in the argument.

# Here is an example reading SAS data. 
# Reading in SAS data requires the 'sas7bdat' library.

PackageTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
PackageTest('sas7bdat')
library(sas7bdat)
sasdata <- read.sas7bdat("sas_researchdata.sas7bdat")
sasdata
str(sasdata)


# II. Data Manipulation-Understanding R objects

# To understand data manipulation, it is imperative to understand the concepts for R objects (the nature and the property of the R objects).
# From a technical standpoint, the entities R operates on are technically known as objects. 
# Examples of R objects include vectors of numeric (real/complex) values, vectors of logical values and vectors of character strings. 
# These are known as atomic structures since their components are all of the same type, or mode, namely numeric, complex, logical, character or raw. 

# R objects have many properties, one of which is called mode.
# Mode refers to the basic type of its fundamental constitutes of an object (thus atomic).
# Vectors must have their values of the same mode, thus any given vector must be unambiguously logical, numeric, complex, character, or raw. 
# Note that a vector can be empty but still has a mode. 
# Some examples of modes include numeric, character, complex, logical, integer, and factor etc.
# Besides mode,another such property of R objects is called length.
# Properties such as mode and length are called the intrinsic attributes of an object.

# A similar concept in R is class, which is essentially the definition of an object (all objects in R has a class).
# Typically a class contains several slots that are used to hold class-specific information.
# Some examples of classes include data frame, factor, table, or matrix etc.
# Operationally speaking, R programming is based on objects or instances of classes, while computations are carried out via methods.
# Methods are defined to be basically functions that are specialized to carry out specific calculations on objects, usually of a specific class.

# In addition, R also operates on objects called lists, which are ordered sequences of objects which individually can be of any mode.
# Lists are known as recursive structures rather than atomic structures since their components can themselves be lists in their own right. 
# The other recursive structures are those of mode function and expression. 

o1 <- c(1,3,9,9)
mode(o1)
length(o1)
class(o1)
attributes(o1)
o2 <- (o1<8 & o1>0) # here the & sign cannot be replaced by the English word 'and'
o2
mode(o2)
length(o2)
class(o2)
attributes(o2)
o3 <- c(3+9i, 4-8i, 5, 9-6i)
mode(o3)
class(o3)
attributes(o3)
o4 <- c("I", "love", "Beethoven")
o4
mode(o4)
class(o4)
attributes(o4)
o5 <- list(ltr=LETTERS, TRUE, print(1:10), print, 1:10, o1>5, pianist <- c("Glenn Gould", "Vladmir Horowitz", "Sviatoslav Richter", ""), mtcars)
o5[1] # this gives you the first object $ltr
o5[[1]] # this gives you directly what the object $ltr is 
o5[2]
o5[[2]]
o5[3]
o5[4]
o5[5]
o5[[5]][3] # this calls the 5th object's 3rd component
o5[6]
o5[7]
o5[[7]][1] # Glenn Gould
o5[[7]][3] # Sviatoslav Richter
o5[8]
o5[[8]] # this gives the whole data set
o5[[8]][7] # this gives the 7th column of the whole dataset
mode(o5[1]) # list 
class(o5[1]) # list
mode(o5[[1]]) # character
class(o5[[1]]) # character
mode(o5[2]) # list
mode(o5[[2]]) # logical
class(o5[[2]]) # logical
mode(o5[3]) # list
mode(o5[[5]][3]) # numeric
mode(o5[[7]][1]) # character
mode(o5[[7]][3]) # character
mode(o5[8]) # list
class(o5[8]) # list
mode(o5[[8]]) # list
class(o5[[8]]) # data.frame
mode(o5[[8]][7]) # list
class(o5[[8]][7]) # data.frame
mode(o5) # this is a list
length(o5) # this equals 8 because there are 8 atomic components
class(o5) # list

# A peculiarity has to be mentioned here about factors and the function factor().
# The syntax for factor() is factor(x = character(), levels, labels = levels, exclude = NA, ordered = is.ordered(x), nmax = NA).
# If the argument 'ordered' is set to be TRUE, the result has class c("ordered", "factor").
# Otherwise, the mode of a factor is numeric (which is very counter-intuitive).

o6 <- c('CA', 'TX', 'VT', '', '', 'TX', 'TX', 'CA')
o7 <- factor(o6)
o8 <- factor(o6, ordered=TRUE)
o7[1]
o7[[1]]
levels(o7) # getting the levels of the factor
mode(o7[1]) # numeric
class(o7[1]) # factor
mode(o7[[1]]) # numeric
class(o7[[1]]) # factor
class(o8) # ordered factor

# Another important concept in R (and in more general computer science languages as well) is regular expressions.
# In theoretic computer science, a ‘regular expression’ is a pattern that describes a set of strings. 
# Two types of regular expressions are used in R, extended regular expressions (the default) and Perl-like regular expressions used by perl = TRUE.
# For computer scientists, perl is a high-level general purpose programming language used especially for developing web applications.
# The concept of regular expressions are usually closed related to character functions that deals with text strings.


# III. Data Manipulation-Using R Functions 

# R provides many built-in functions to handle data manipulations. 
# We introduce a few common used R built-in functions here.

# Firstly, the rep() function is a generic function that replicates the values in the object. 
# The syntax for rep() is rep(x, ...), where x stands for the R object to be replicated.
# The default behavior of rep() is rep(x, times = 1, length.out = NA, each = 1).

rep(1:4, 2)
rep(1:4, each = 2) 
rep(1:4, c(2,2,2,2)) # same as rep(1:4, each = 2) 
rep(1:4, c(2,1,2,1)) # 1 1 2 3 3 4
rep(1:4, each = 2, len = 4) # 1 1 2 2 (first 4 only)
rep(1:4, each = 2, len = 10) # 1 1 2 2 3 3 4 4 1 1 (recycling old values)
rep(1:4, each = 2, times = 3) # length 24 with 3 complete replications

# The second common function is the seq() function that generates a sequence.
# The default setting is seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)), length.out = NULL, along.with = NULL, ...).

seq(5, 10, by=1.2)
seq(0, 1, length.out = 11)
seq(17) # same as 1:17
1:17

# The cut() function divides the range of a numeric vector and codes the values in the vector according to which interval they fall.
# The leftmost interval corresponds to level one, the next leftmost to level two and so on. 
# The general syntax of the cut() function is cut(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, dig.lab = 3, ordered_result = FALSE, ...).
# Here 'x' is a numeric vector which is to be converted to a factor by cutting.
# The argument 'breaks' is a numeric vector of two or more unique points or a single number giving the number of intervals into which 'x' is to be cut.
# The argument 'labels' stands for labels for the levels of the resulting category.
# By default, labels are constructed using "(a,b]" interval notation. 
# If labels = FALSE, simple integer codes are returned instead of a factor.
# The 'include.lowest' argument is logical, indicating if an ‘x[i]’ equal to the lowest (or highest, for right = FALSE) ‘breaks’ value should be included.
# The 'right' argument is logical too, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
# The 'ordered_result' argument is logical, indicating whether the result should be an ordered factor.
# When the argument 'breaks' is specified as a single number, the range of the data is divided into breaks pieces of equal length, and the outer limits are moved away by 0.1% of the range to ensure that the extreme values both fall within the break intervals. 
# If x is a constant vector, equal-length intervals are created, one of which includes the single value.
# If a 'labels' parameter is specified, its values are used to name the factor levels. 
# If none is specified, the factor level labels are constructed as "(b1, b2]", "(b2, b3]" etc. for right = TRUE and as "[b1, b2)", ... if right = FALSE.

y0 <- c(1,3,4,5,6,2,5,2,2,2,9,0,1)
y1 <- cut(y0, breaks=4)
y1
mode(y1) # numeric
class(y1) # factor
y2 <- cut(y0, breaks=3, labels=c('I1', 'I2', 'I3'))
y2

# The paste() function concatenates vectors after converting to character. 
# The syntax for paste() is paste(..., sep=" ", collapse=NULL).
# The sep argument is the separator (default set to be a blank space).
# The function essentially converts its arguments (via the as.character() function) to character strings, and concatenates them (separating them by the string given by sep). 
# If the arguments are vectors, they are concatenated term-by-term to give a character vector result. 
# Vector arguments are recycled as needed, with zero-length arguments being recycled to "". 
# The optional argument collapse is the string to separate objects (a rarely used argument).
# If a value is specified for collapse, the values in the result are then concatenated into a single string, with the elements being separated by the value of collapse. 

y3 <- paste("I", "love", "statistics")  # pretty much the same as compress() function in SAS
y3
y4 <- paste("I", "love", "statistics", sep="")
y4
y5 <- paste("I", "love", "statistics", sep="-")
y5
y6 <- paste(1:12)
y6
length(y6) # 12
y7 <- paste(1:12, c("st", "nd", "rd", rep("th", 9)), sep="")
y7
y8 <- paste(month.abb, y7, sep = ": ", collapse = "; ")
y8 # month.abb is a pre-defined vector of month abbreviations in R (just like the vectors letters and LETTERS)

# The tolower() and toupper() changes text strings' cases.
# There is no built-in proper function for proper case.

y9 <- "Chopin is the poet on the piano"
tolower(y9) # changing to lower case
toupper(y9) # changing to upper case
 
# The substr() extracts or replaces substrings in a character vector.
# The syntax for substr() is substr(x, start, stop), where start and stop arguments are both integers.

y10 <- "abcdefg"
y11 <- substr(y10, 2, 5)
y11 # bcde
substr(y10, 2, 5) <- 'AAA'
y10

# The grep() function is one of the most important functions in R.
# The function searches for matches to the argument 'pattern' within each element of a character vector.
# In addition, the function grepl(), regexpr() and gregexpr() perform the same tasks.
# They differ in the format of the amount of detail in the results. 
# The default syntax for grep() is given by grep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE, invert = FALSE).
# The default syntax for grepl() is given by grepl(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE).
# The default syntax for regexpr() is given by regexpr(pattern, text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE).
# The default syntax for gregexpr() is given by gregexpr(pattern, text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE).
# Here, the 'pattern' argument refers to character strings containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector.
# Missing values are allowed except for regexpr() and gregexpr().
# The 'x' and 'text' argument are character vectors where matches are sought, or an object which can be coerced by as.character() to a character vector.
# The 'ignore.case' is logical, and if false, the pattern matching is case sensitive.
# The 'value' argument is logical, and if false, a vector containing the integer indices of the mathces determined by grep() is returned.
# If the 'value' argument is true, a vector containing the matching elements themselves is returned. 
# The 'perl' argument is logical (if perl-compatible regexps() should be used).
# The argument 'fixed' is logical, and if true, the 'pattern' is a string to be matched as is.
# The argument 'useBytes' is logical, and if true, the matching is done byte-by-byte rather than character-by-character.
# The argument 'invert' is logical, and if true, it returns indices or values for elements that do not match.
# Note that for all these functions, arguments which should be character strings or character vectors are coerced to character if possible for these functions.
# The functions regexpr() and gregexpr() with perl = TRUE allow Python-style named captures, but not for long vector inputs.

grep("d", letters) # 4
grep(pattern="piano", x="Chopin is the piano poet") # 1 
grep(pattern="pianoes", x="Chopin is the piano poet") # integer(0) b/c we cannot find the text string anywhere
grep(pattern="piano", x=c("Brahms is very self-conscious", "Schubert's keyboard pieces are all cantible", "Chopin is the piano poet")) # 3
grep(pattern="key", x=c("Brahms is very self-conscious", "Schubert's keyboard pieces are all cantible", "Chopin is the piano poet")) # 2 b/c the string 'key' happens in the word 'keyboard'
grep(pattern="key", x=c("Brahms is very self-conscious", "Schubert's keyboard pieces are all cantible", "Chopin is the piano poet", "Alfred Cortot"), invert=TRUE) # 1 3 4 b/c of the invert=TRUE
grep(pattern="piano", x=c("Chopin is the piano poet", "Murray Perahia played Chopin etudes", "Pianoforte foundation has annual piano competitions", "Different pianos give pianists different feelings"), value=T)

# The sub() and gsub() perform replacements of the first and all matches respectively ('g' stands for 'global' and 'sub' stands for 'substitute'). 
# They are closely related to the grep() function and have similar arguments.
# The default syntax of sub() is sub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE).
# The default syntax of gsub() is gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE).

gsub(pattern="ut",replacement="ot",x="But")
gsub(pattern="uT",replacement="ot",x="But")
gsub("Tutorial","Examples",x=c("R Tutorial","PHP Tutorial", "HTML Tutorial", "C++ Textbook"))
sub("ress","", c("Regular", "expression", "examples of R language", "the press suppresses the results"))

# Another important function that separates character strings is strsplit(), whose output is a list..
# The function splits the elements of a character vector 'x' into substrings according to the matches to substring argument 'split' within them. 
# The syntax for the function is strsplit(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE).
# Here, 'x' is a character vector, each element of which is to be split.
# The 'split' argument is a character vector (or object which can be coerced to such) containing regular expression(s) (unless fixed = TRUE) to use for splitting. 
# If empty matches occur, in particular if 'split' has length 0, 'x' is split into single characters. 
# If 'split' has length greater than 1, it is re-cycled along 'x'.
# The argument 'fixed' is logical, and if true, it matches 'split' exactly, otherwise it uses regular expressions.
# Note that splitting into single characters can be done via split = character(0) or split = "".
# A missing value of split does not split the corresponding element(s) of 'x' at all.
# An important rule of using the function is that if there is a match at the beginning of a (non-empty) string, the first element of the output is "".
# But if there is a match at the end of the string, the output is the same as with the match removed. 

s1 <- "Please split these words in a sentence. Pay attention to the each result!"
strsplit(x=s1, split=" ")
strsplit(x=s1, split="")
strsplit(x=s1, split="--") # no effect on the original text string as there is no "--" string involved
strsplit(x=s1, split=". ") # splitting at each space with a preceding character
strsplit(x=s1, split=".") # this is different from using split=". "
strsplit(x=s1, split=". ", fixed=TRUE) 
strsplit(x=s1, split="these")
s2 <- c("1999-05-23", "2001-12-30", "2004-12-17")
strsplit(x=s2, split="-")
strsplit(x=s2, split="--")
s3 <- "5 times 5 times 5 equals 125"
strsplit(x=s3, split="5")
strsplit(x=s3, split=" times ")
strsplit(x=s3, split=" times")
strsplit(x=s3, split="times ")

# In addition, there are functions that deal with numeric variables, such as ceiling(), floor(), and round() etc..
round(3.475, digits=2) # 3.48
ceiling(3.56) # 4
floor(4.1) # 4


# IV. Dates and Time in R

# There are many built-in R function, such as as.Date() that handles dates (without time). 
# Likewise, the library 'chron' handles dates and times, but does not control for time zones.
# In addition, the POSIXct and POSIXlt classes allow for dates and times with control for time zones.
# Except for the POSIXlt class, dates are stored internally as the number of days or seconds from some reference date. 
# Thus dates in R will generally have a numeric mode, and the class function can be used to find the way they are actually being stored. 
# The POSIXlt class stores date/time values as a list of components (hour, min, sec, mon, etc.) making it easy to extract these parts. 
# In addition, to get the current date, the Sys.Date function will return a Date object which can be converted to a different class if necessary. 
# In general, R is very rich in methodds dealing with dates and time.

# The as.Date() function allows a variety of input formats through the format= argument.
# The default format is a four digit year, followed by a month, then a day, separated by either dashes or slashes.
# If your input dates are not in the standard format, a format string can be composed using the elements in some special forms.
# For example, one can specify format='%m/%d/%Y', which denotes month, day, year date-time codes respectively.
# There are 6 date-time codes used in format: %d (day of the month), %m (numeric month), %b (abbreviated month), %B (month in words), %y (2-digit year), and %Y (4-digit year).
# Internally, 'Date' objects are stored as the number of days since January 1, 1970, using negative numbers for earlier dates. 
# The as.numeric() function can be used to convert a 'Date' object to its internal form. 

as.Date('1915-6-16') # "1915-06-16"
as.Date('1990/02/17') #  "1990-02-17"
as.Date('1/15/2001',format='%m/%d/%Y') #  "2001-01-15"
as.Date('April 26, 2001',format='%B %d, %Y') #  "2001-04-26"
as.Date('April/26--2008',format='%B/%d--%Y') #  "2008-04-26"
as.Date('December~~25-/-2009',format='%B~~%d-/-%Y') #  "2009-12-25"
mode(as.Date('1915-6-16')) # numeric
class(as.Date('1915-6-16')) # Date
class(as.Date('1/15/2001',format='%m/%d/%Y')) # Date
as.numeric(as.Date('1970-1-3')) # 2 b/c the default cutoff year is 1970-1-1

# To extract the components of the dates, the weekdays(), months(), julian or quarters() functions can be used.
# They are very similar in SAS, and their core arguments are all R dates. 
# However, the base R does not supply as many date-time function as SAS (base R itself does not have years().
# The 'lubridate' pakcage fills this gap by providing day(), month() and year() functions.
# The package also provides seconds(), minutes(), hours(), milliseconds(), microseconds(), nanoseconds() and picoseconds() functions. 
# But note that the output of these functions may differ, as their respective classes differ.

bdays = c(tukey=as.Date('1915-06-16'),fisher=as.Date('1890-02-17'), cramer=as.Date('1893-09-25'), kendall=as.Date('1907-09-06'), default=as.Date('1970-01-01'))
weekdays(bdays)
months(bdays)
julian(bdays)
quarters(bdays)
PackageTest('lubridate')
library(lubridate)
day(bdays) # 16 17 25 6 1
weeks(bdays) # "-139461d 0H 0M 0S" "-204204d 0H 0M 0S" "-194992d 0H 0M 0S" "-159341d 0H 0M 0S" "0S" 
month(bdays) #  6 2 9 9 1
year(bdays) # 1915 1890 1893 1907 1970
class(year(bdays)) # numeric
class(weeks(bdays)) # Period

# Another important function is the chron() function in the 'chron' library, which converts dates and times to chron objects.
# It basically creates chronological objects which represent dates and times of day. 
# In the same library, the dates() and times() functions also create objects that represent dates and times.
# The syntax is chron(dates., times., format = c(dates = "m/d/y", times = "h:m:s"), out.format, origin.).
# Here, the 'dates' argument is a character or numeric vector specifying dates.
# If 'dates' is character, then it is assumed to be ine one of the datae formats allowed.
# If 'dates' is numeric, then it is assumed to be Julian dates (i.e. the number of days since 'origin', whose default value is c(month=1, day=1, year=1970)).
# The 'origin' argument is a vector specifying the date with respect to which Julian dates are computed. 
# For example, one can reset this default by typing the following statement: options(chron.origin=c(month=1, day=1, year=1990).
# The 'time' argument is an optional character or numeric vector specifying times of day. 
# If 'time' is character, then it is assumed to be ine one of the time formats allowed.
# If 'time' is numeric, then it is assumed to be fractions of a day. 
# The 'format' argument is a vector or list specifying the input format of the input. 
# The format can be either strings specifying one of the recognized formats or a list of user-supplied functions to convert dates from character into Julian dates and vice versa.
# The dates format can be any permutation of the characters "d", "m", or "y" delimited by a separator (possibly null).
# For example, e.g., "m/d/y", "d-m-y", "ymd", are all valid formats.
# The format can also be permutations of the words "day", "month" and "year" (with non-null separator), which produces the month name.
# For example, "month day year" produces "April 20 1992", and "day mon year" produces "20 Apr 1992".
# The 'out.format' argument is a vector or list specifying the date and time format for printing and display. 
# Note that the dates and times are provided to the chron() function as separate values, so some preprocessing may be necessary to prepare input date/times for the function.

PackageTest("chron")
library(chron)
day_v <- dates(c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92"))
time_v <- times(c("23:03:20", "22:29:56", "01:03:30", "18:21:03", "16:56:26"))
mode(day_v) # numeric
class(day_v) # dates times 
mode(time_v) # numeric
class(time_v) # times
chron_v1 <- chron(day_v, time_v)
chron_v1
chron_v2 <- chron(day_v, time_v, format=c("m-d-y", "h~m~s"))
chron_v2

# POSIX represents a portable operating system interface, primarily for UNIX systems, but available on other operating systems as well. 
# Dates stored in the POSIX format are date/time values (like dates with the 'chron' library), but also allow modification of time zones. 
# Unlike the 'chron' library, which stores times as fractions of days, the POSIX date classes store times to the nearest second, so they provide a more accurate representation of times. 
# There are two POSIX date/time classes, which differ in the way that the values are stored internally. 
# The POSIXct class stores date/time values as the number of seconds since January 1, 1970, while the POSIXlt class stores them as a list with elements for second, minute, hour, day, month, and year, among others. 
# Unless you need the list nature of the POSIXlt class, the POSIXct class is the usual choice for storing dates in R. 
# The default input format for POSIX dates consists of the year, followed by the month and day, separated by slashes or dashes.
# For date/time values, the date may be followed by white space and a time in the form hour:minutes:seconds or hour:minutes.
# Thus, the following are examples of valid POSIX date or date/time inputs: 1915/6/16, 2005-06-24 11:25, or 1990/2/17 12:20:05.
# Moreover, if the input times correspond to one of these formats, the function as.POSIXct() can be called directly.
# Furthermore, the individual components of a POSIX date/time object can be extracted by first converting to POSIXlt if necessary, and then accessing the components directly.
# Last but not least, many of the statistical summary functions, like mean, min, max, etc are able to transparently handle date objects.
# To use dates for numerical calculation, we usually use the as.Date() function to transform the data so that R can calculate it like numeric numbers.

dts1 <- c("2005-10-21 18:47:22", "2005-12-24 16:39:58", "2005-10-28 07:30:05 PDT")
as.POSIXlt(dts1)
mean(as.Date(dts1))
dts2 = as.POSIXlt('2005-4-19 7:01:00')
mode(dts2) # list
class(dts2) # "POSIXlt" "POSIXt" 
names(dts2) # "sec"   "min"   "hour"  "mday"  "mon"   "year"   "wday"  "yday"  "isdst"
dts2$hour # 7

# For standardization purposes of the date-time format, the ISOdate() and ISOdatetime() functions serve as convenience wrappers to create date-times from numeric representations.
# The default syntaxes are ISOdatetime(year, month, day, hour, min, sec, tz = ""), and ISOdate(year, month, day, hour = 12, min = 0, sec = 0, tz = "GMT").
# Here, 'tz' stands for timezone, and the rest of the arguments should be all numeric.
# To check system's time zone, we can use the Sys.timezone() function. The other useful system functions are Sys.time() and Sys.Date().

Sys.timezone() 
Sys.time()
Sys.Date()
b1 = ISOdate(1977,7,13) 
b1 # "1977-07-13 12:00:00 GMT"
b2 = ISOdate(2003,8,14, tz='America/Chicago')
b2 # "2003-08-14 12:00:00 CDT"
b2 - b1
mode(b1) # numeric
class(b2) # "POSIXct" "POSIXt" 


# V. Data Manipulation-Renaming/Adding/Dropping Variables

# Now we need to do some manipulation on the columns of the datasets as a whole.  
# We first rename columns using the colnames() function, which retrieves or sets the column names of a matrix-like object (usually a data frame).
# We also want to add a column to the data frame and drop an irrelevant one. 

colnames(r)[1] <- "drg"
colnames(r)[2] <- "drg_weights"
colnames(r)[3] <- "drg_desc"
names(r) # checking if the first 3 columns are renamed
plan <-rep("Southwest Health Plan", nrow(r)) # creating a new variable using the rep() function to replicate a value t times, with t being the number of records in the data
r1 <-data.frame(r, plan) # stacking the new column in the original data frame
r1$SeverityOfIllness <- NULL # dropping the variable SeverityOfIllness
names(r1)


# VI. Data Manipulation-Conditional Logic

# We now do some simple conditioning logic and create another new variable.
# We then create a subset using some conditional logic.

levels(r1$effdate) # the levels() function checks how many levels are there for the factor
current_flag <- ifelse(r1$effdate =="09/01/2011", "Yes", "No") # if the value is in 2015 then the current flag is set to be 'Yes'
weight_flag <- ifelse(r1$drg_weights >= 9.13, 1, 0) # another indicator variable
levels(r1$termdate)
year <- ifelse(r1$termdate == "10/31/2012" | r1$termdate == "11/01/2012", 2012,
          ifelse(r1$termdate == "09/01/2014", 2014,
            ifelse(r1$termdate == "09/30/2015", 2015, 2078)
          )
        )
r2 <- data.frame(r1, current_flag, weight_flag, year)
r2[799:806, ]
names(r2)

# Similarly, as another example, we can do conditional logic on the ETF dataset too.
# For this dataset, we create a flag variable (binary if-else logic) and we do a multiple chaining conditional logic.
# The first variable is called flag (based on whether the Sector is Financial).
# The second variable is called decision, again based on different sectors but with multiple possible strategies.

flag <- ifelse(etf$Sector=="Financial", 1, 0) # same as if-then statement in SAS
etf2=data.frame(etf,flag)
etf2$decision <- ifelse(etf2$Sector=="Energy", "Strong Buy", 
                 ifelse(etf2$Sector=="Financial", "Hold",
                 ifelse(etf2$Sector=="REIT", "Strong Hold", "Sell"
                 )))
print(etf2[c(540:550, 1850:1860), ]) # looking at different sectors behavior and the new variables


# VII. Data Manipulation-Subsetting

# Subsetting in R can be done either using the indexing methods or the subset() function.
# Indexing requires that we have a data frame or matrix to work with.
# The subset() function requires you to specify the dataset name, the condition, and the variables you want to select.
# Also note that for the subset function, you do not need to worry about the layers of objects (so you don't need to specify things like dataset$var within the function.

r3 <- r2[r2$year>=2078, c(1:5, 7:9)]
r3[1:3, ]
r4 <- subset(r2, weight_flag!=1, select=c(drg, drg_weights, effdate, weight_flag))
r4[1:3, ]

etf3 <- etf2[etf2$Sector=="REIT" & etf2$Volume>30000, ]
print(etf3[1:6, ])
etf4 <- subset(etf2, etf2$Sector=="Financial", select=c(Sector, Date, High, Low))
print(etf4[1:6, ])


# VIII. Frequency Table

# In SAS we have proc freq to check frequencies.
# In R we can do the same thing using the 'plyr' package.
# The user-defined function proc.freq() below will calculate the frequency and percentage of each class variable.

PackageTest("plyr") # this package is designed for proc freq and many other data manipulation techniques
library(plyr)
proc.freq <- function(var, dataset){
             y=count(dataset, var)
             y$percent <- y$freq/sum(y$freq)
             return(y)
             }
proc.freq('effdate', r2)
proc.freq('Sector', etf2)

# We now create a function that does a proc frequency analysis on all of the factors in a data frame automatically.
# This user-defined function first identifies all factor elements in a data frame, then apply the proc.freq function to each factor.

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
proc.freq.allfactors(etf2)


# IX. Data Manipulation-Sorting 

# Note that for sorting variables in a data frame, it's recommended that we use the order() function rather than the sort() function.
# This is because the sort() function can only be exerted on a vector (one-dimensional) yet order() can work on multiple variables at the same time.
# Sorting on numeric variables is really easy, because the default is ascending, and we put a negative sign in front of the variable to denote descending order.

r5 <- r2[order(r2$drg, -r2$drg_weights), ] # sorting on ascending drg and descending effdate
r5[1:10, ]

# However, sorting on character variables are a bit tricky, because the negative sign does not make sense for R.
# Moreover, it gets more tricky when we want to sort some variables by descending order while others by ascending orders in the same step.
# Fortunately, the plyr package solves the latter problem so that we can sort data by multiple columns with different directions.
# The plyr package has the built-in function arrange() to order a dataframe using its columns. 

myCars = cbind(vehicle=row.names(mtcars), mtcars) # getting the rownames using the row.names() function
myCars # now the dataset has row names
myCars2 <- arrange(df=myCars, vehicle, desc(disp), desc(carb), hp)
myCars2


# X. Data Manipulation-Aggregation and Group-By logic

# In SQL language, we often have a group-by statement to do calculation on different levels.
# In R, we can achieve the same thing by using the tapply() function.
# The tapply() function has a very simple syntax: tapply(var, group_by_var, aggregation_function).
# Specifically, it is equivalent to the SQL query: select aggregation_function(var) from var.table group by group_by_var.
# The result of the tapply() is an array (class).
# Just like SQL, one can even specify multiple factors as the grouping variable, for example treatment and sex, or team and handedness. 

r3[1:13, ]
q1 <- tapply(r4$drg_weights, r4$effdate, mean) # getting the average of the drg_weights by effective date
q1
q2 <- tapply(r4$drg, r4$effdate, length) # getting the drg count by effective date
q2
w1=tapply(etf2$Volume, etf2$Sector, FUN=sum) # adding up the volumes by sector
w1
w2=tapply(etf2$Volume, etf2$Sector, FUN=min) # getting the min volumes by sector
w2


# XI. Data Export

# There are also many built-in functions for R to write out files.
# Examples include sink(), write.csv(), write.table() and capture.output() etc.. 
# The sink() function diverts R output to a connection (path is the default path unless specified otherwise).

write.csv(myCars2, file="myCars2") 
write.table(myCars2, file="myCars2") # this writes out a generic file which can be opened by notepad or textpad. 



# References:
# http://www.inside-r.org/r-doc/base/sub
# http://rfunction.com/archives/1499
# http://www.stat.berkeley.edu/~s133/resources.html
# http://www.stat.berkeley.edu/~s133/all2011.pdf
# http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf 
# http://www.stat.berkeley.edu/~s133/dates.html
# http://www.statmethods.net/management/functions.html
# https://drive.google.com/file/d/0B4mP5kf41BrmYkxkdmdRdWFibDA/view?pref=2&pli=1

