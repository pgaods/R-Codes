# Advanced R Concepts in Machine Learning 


#############################################


# I. The Goal

# Machine Learning involves a lot of intensive computations, functional iterations, functional composition, and techniques of big data management and analyses.
# Having a deep understanding of the programming concepts in R is essential to be a data scientist in R.
# This module introduces the advanced concepts in R and their applications to machine learning.
# We will lay the foundations by introducing many important aspects of R such as data structures, R vocabularies, R functions, OO fields, and R environments.  
# We will also examine topics in functional programming, meta-programming, profiling, memory, and Rcpp etc.
# The entire module follows the structure of the official documentation "R Language Definition" and Hadly Wickham's online tutorial on 'Advanced R'.
# The "R Language Definition" pdf can be directly downloaded when you open up a new R session (under the 'Help' tab).


# II. R Objects and Data Structures

# The R language is a dialect of S which was designed in the 1980s and has been in widespread use.
# In every computer language, variables provide a means of accessing the data stored in memory.
# R does not provide direct access to the computer’s memory but rather provides a number of specialized data structures which are called 'objects'. 
# R’s base data structures can be organised by their dimensionality and whether they’re homogeneous (all contents must be of the same type) or heterogeneous (the contents can be of different types).
# For example, basic structures include atomic vectors, matrices, arrays, lists (heterogeneous) and data frames (heterogeneous).
# Basic R objects include vectors, lists, language objects, expression objects, function objects, NULL objects, and builtin objects (special forms).
# Besides the above, there are other rarely-used objects, such as promise objects, environment objects, pairlist objects, dot-dot-dot and any-type. 
# Since R is an object-oriented programming language, central to this style of programming are the concepts of class and of methods.
# A class is a definition of an object, and a class contains several 'slots' that are used to hold class-specific information.
# An object in the language must be an instance of some 'class', and programming is based on objects or instances of classes. 
# Computations are carried out via 'methods', which are defined to be basically functions that are specialized to carry out specific calculations on objects, usually of a specific class. 
# In R, generic functions are used to determine the appropriate method and they are responsible for determining the class of its argument(s) and uses that information to select the appropriate method. 

# The vectors and lists objects are the two most common R objects. 
# Both vectors and lists have three common properties: 1) type, 2) length, and 3) attributes.
# To obtain the respective information, the typeof(), length(), and attributes() functions can be applied. 
# The typeof() function determines the R internal type or storage mode of any object.
# The result could be vector types like "logical", "integer", "double" (real-valued vectors), "complex", "character", and "raw" etc.
# It could also be "list", "NULL", "closure" (function), "special", "builtin" (basic functions and operators), "environment", and "S4" (some S4 objects).
# In rare instances, typeof() could result in other values that are unlikely to be seen at user levels (e.g. "symbol", "pairlist", "promise", "language", "char", "...", "any", "expression", "externalptr", "bytecode" and "weakref"). 
# Note that the typeof() function is different from the mode() and storage.mode() function. 
# The mode() function is defined in the sense of the seminal work of Becker, Chambers & Wilks (1988), and is more compatible with other implementations of the S language.
# The storage.mode() is often used used when calling functions written in another language, such as C or FORTRAN, to ensure that R objects have the data type expected by the routine being called.
# String vectors have mode and storage mode "character", and missing values are specified with NA.
# Lists (“generic vectors”) are another kind of data storage, whose elements are accessed through three different indexing operations (lists are sometimes called recursive vectors, because a list can contain other lists). 

# To check object types, we can alway use the functions such as is.atomic(), is.logical(), is.double(), is.character(), or is.numeric() etc.
# To convert object types, we can invoke the functions such as as.numeric(), as.character(), as.integer(), or as.logical() etc.
# All elements of an atomic vector must be the same type, so when you attempt to combine different types they will be coerced to the most flexible type.
# Types from least to most flexible are: logical, integer, double, and character.
# Another important note is when a logical vector is coerced to an integer or double, TRUE becomes 1 and FALSE becomes 0. 

# We now discuss a very important property of R objects: attribute. 
# All objects except NULL can have one or more attributes attached to them.
# Attributes are stored as a pairlist where all elements are named, but should be thought of as a set of name=value pairs.
# A listing of the attributes can be obtained using attributes and set by the command"attributes <- ", and individual components are accessed using attr() and the command "attr <- ". 
# For example, matrices and arrays are simply vectors with the attribute dim and optionally dimnames attached to the vector. 
# Attributes are used to implement the class structure used in R, and if an object has a class attribute then that attribute will be examined during  evaluation. 
# The most common attributes include: names, dimensions, dimnames, classes, and time series attributes, 

# A names attribute, when present, labels the individual elements of a vector or list.
# For pairlist objects, a virtual names attribute is used (the names attribute is really constructed from the tags of the list components).
# For one-dimensional arrays the names attribute really accesses dimnames[[1]]. 
# The dim attribute is used to implement arrays, and the content of the array is stored in a vector in column-major order and the dim attribute is a vector of integers specifying the respective extents of the array. 
# A vector is not the same as a one-dimensional array since the latter has a dim attribute of length one, whereas the former has no dim attribute. 
# Arrays may name each dimension separately using the dimnames attribute which is a list of character vectors. 
# The dimnames list may itself have names which are then used for extent headings when printing arrays. 
# The tsp attribute is used to hold parameters of time series, start, end, and frequency.
# This construction is mainly used to handle series with periodic substructure such as monthly or quarterly data. 

# Attributes may change when we are copying them, for example, coercion drops all attributes.
# Whether attributes should be copied when an object is altered is a complex area, but there are some general rules (Becker, Chambers & Wilks, 1988, pp. 144–6). 
# Scalar functions (those which operate element-by-element on a vector and whose output is similar to the input) should preserve attributes with a few exceptions.
# Binary operations normally copy most attributes from the longer argument (and if they are of the same length from both, preferring the values on the first).
# Here 'most' means all except the names(), dim() and dimnames() which are set appropriately by the code for the operator. 
# Subsetting (other than by an empty index) generally drops all attributes except names(), dim() and dimnames() which are reset as appropriate.

x <- c(1,2,3) 
typeof(x) # double
length(x) # 3
mode(x) # numeric
storage.mode(x) # double
class(x) # numeric
is.atomic(x) # TRUE
attributes(x) # NULL
y <- c(TRUE, FALSE, T, F)
typeof(y) # logical
mode(y) # logical
storage.mode(y) # logical
class(y) # logical
is.atomic(y) # TRUE
z <- list(ltr=LETTERS, logic=TRUE, print(1:10), print, sequence=1:10, pianist = c("Glenn Gould", "Vladmir Horowitz", "Sviatoslav Richter", "", "Claudio Arrau"), dsn=mtcars[1:3, ], pianist2 <- c("Alfred Brendal", "Daniel Barenboim", NA))
typeof(z) # list
length(z) # 8
mode(z) # list
storage.mode(z) # list
names(z) # note that 'pianist' exists yet 'pianist2' does not exist
class(z) # list
is.atomic(z) # FALSE
w <- list(list(list(list())))
str(w)
attributes(w) # NULL
M <- matrix(c(2, 4, 3, 1, 5, 7), nrow=2, ncol=3, byrow=T)
M
nrow(M)
ncol(M)
typeof(M) # double
length(M) # 6
mode(M) # numeric
storage.mode(M) # numeric
str(M)
class(M) # matrix
is.atomic(M) # TRUE

# There are three types of objects that constitute the R language objects: they are 'calls', 'expressions', and 'names', who have modes "call", "expression", and "name", respectively.
# They can be created directly from expressions using the quote() mechanism and converted to and from lists by the as.list() and as.call() functions respectively.
# The quote() function takes only one argument (expression) and returns its argument as a symbol, which can also be created by the as.name() function.
# Symbols have mode "name", storage mode "symbol", and type "symbol". 

quote(m-n) # m-n
quote(M) # M
quote(s <- c("S Language", "R Language")) # s <- c("S Language", "R Language")
func <- function(x1, x2) {
        x1+x2
        }
quote(func) # func
typeof(quote(func)) # symbol
length(quote(func)) # 1
mode(quote(func)) # name
storage.mode(quote(func)) # symbol

# An R expression contains one or more statements, which are syntactically correct collection of tokens.
# Expression objects are special language objects which contain parsed but unevaluated R statements.
# An expression object behaves much like a list and its components should be accessed in the same way as the components of a list. 
# The eval() function evaluates an R expression in a specified environment, whose objects can be of types of calls, expressions, names, promises, or any of the basic types like vectors, functions or environments.

ex1 <- expression(c(3*2, 4*2))
ex1 # expression(c(3*2, 4*2))
eval(ex1) # 6, 8
typeof(ex1) # expression
length(ex1) # 1
mode(ex1) # expression
storage.mode(ex1) # expression
class(ex1) # expression
is.atomic(ex1) # FALSE
attributes(ex1) # NULL
ex2 <- expression(z)
ex2
eval(ex2)
typeof(ex2) # expression
length(ex2) # 1
mode(ex2) # expression
storage.mode(ex2) # expression

# The NULL object has no type and no modifiable properties, and testing requires is.null().
# There is actually only one NULL object in R (which is 'atomic' of course), to which all instances refer. 

n <- NULL
typeof(n) # NULL
length(n) # 0
mode(n) # NULL
storage.mode(n) # NULL
class(n) # NULL
is.atomic(n) # TRUE

# R functions (or more precisely, function closures) have three basic components: a formal argument list, a body and an environment. 
# The argument list is a comma-separated list of arguments, and body is a parsed R statement.
# An argument can be a symbol, or a 'symbol = default' construct, or the special argument '...'.
# The '...' is used if the number of arguments is unknown or in cases where the arguments will be passed on to another function. 
# The body is a collection of parsed R statements, which are usually a collection of statements in braces but it can be a single statement, a symbol or even a constant. 
# A function's environment is the environment that was active at the time that the function was created.
# Any symbols bound in that environment are captured and available to the function. 
# This combination of the code of the function and the bindings in its environment is called a 'function closure', a term from functional programming theory.
# It is possible to extract and manipulate the three parts of a closure object using 'formals', 'body', and 'environment' constructs (all three can also be used on the left hand side of  assignments). 
# When a function is called, a new environment (called the evaluation environment) is created, whose enclosure is the environment from the function closure. 
# This new environment is initially populated with the unevaluated arguments to the function, and as evaluation proceeds, local variables are created within it. 
# There is also a facility for converting functions to and from list structures using as.list() and as.function().

typeof(func) # closure
length(func) # 1
mode(func) # function
storage.mode(func) # function
attributes(func)

# Builtin objects and special forms contain the built-in functions of R.
# Builtin functions have all their arguments evaluated and passed to the internal function, in accordance with 'call-by-value', whereas special functions pass the unevaluated arguments to the internal function. 
# The is.primitive() function can be used to distinguish them from interpreted functions.

typeof(max) # builtin, b/c max() is a built-in function in base R
length(max) # 1
mode(max) # function
storage.mode(max) # function
is.primitive(max) # TRUE
is.primitive(func) # FALSE, b/c func() is a user-defined function rather than a built-in function in base R

# Promise objects are part of R’s lazy evaluation mechanism, which contain three slots: a value, an expression, and an environment. 
# When a function is called the arguments are matched and then each of the formal arguments is bound to a promise.
# The expression that was given for that formal argument and a pointer to the environment the function was called from are stored in the promise. 
# Until that argument is accessed there is no value associated with the promise.
# When the argument is accessed, the stored expression is evaluated in the stored environment, and the result is returned.
# Within the R language, promise objects are almost only seen implicitly, and actual function arguments are of this type.
# There is generally no way in R code to check whether an object is a promise or not, nor is there a way to use R code to determine the environment of a promise.

# An R environment is simply a place to store variables, which is a set of bindings between symbols and objects.
# If you start up R and make an assignment, you’re adding an entry in the global environment.
# More rigorously, environments can be thought of as consisting of two things: 1) a frame, consisting of a set of symbol-value pairs, and 2) an enclosure, a pointer to an enclosing environment. 
# When R looks up the value for a symbol the frame is examined and if a matching symbol is found its value will be returned.
# If not, the enclosing environment is then accessed and the process repeated.
# Environments form a tree structure in which the enclosures play the role of parents, and the tree of environments is rooted in an empty environment.
# Environments are created implicitly by function calls, and when a function is evaluated, R looks in a series of environments for any variables in 'scope'.
# In this case, the environment contains the variables local to the function (including the arguments), and its enclosure is the environment of the currently called function. 
# Unlike most other R objects, environments are not copied when passed to functions or used in assignments.
# Thus, if you assign the same environment to several symbols and change one, the others will change too. 
# An Environment object is one of the most complicated and abstruse objects in R languages. 

# Pairlist objects are used extensively in the internals of R, but are rarely visible in interpreted code, although they are returned by formals().
# Pairlists are handled in the R language in exactly the same way as generic vectors, and the use of pairlists is deprecated since generic vectors are usually more efficient to use. 
# When an internal pairlist is accessed from R it is generally (including when subsetted) converted to a generic vector. 

# The formals() function gets or sets the formal arguments of a function and returns the formal argument list of the function specified, as a pairlist, or NULL for a non-function or primitive. 
# The syntax for the function is formals(fun = sys.function(sys.parent())), or formals(fun, envir = environment(fun)) <- value.
# Here, the 'fun' argument is a function object, and the 'envir' argument specifies the environment.
# The 'value' argument is a list or a pairlist of R expressions, and note that only closures have formals, not primitive functions.
# For the first syntax form, 'fun' can also be a character string naming the function to be manipulated, which is searched for from the parent frame.
# If it is not specified, the function calling formals() is used. 
# Likewise, the body() function gives the body of the function and the environment() function gives the environment of the function.

formals(func)
length(formals(func))
typeof(formals(func)) # pairlist
mode(typeof(formals(func))) # character
storage.mode(formals(func)) # pairlist
class(formals(func)) # pairlist
is.atomic(formals(func)) # FALSE
formals(lm)
typeof(formals(lm)) # pairlist
body(func) # 2
length(body(func))
mode(body(func)) # call
storage.mode(body(func)) # language
attributes(formals(lm))
body(lm)
length(body(lm)) # 30
mode(body(lm)) # call
storage.mode(body(lm)) # language
class(body(lm)) # "{"
attributes(body(lm)) # NULL
is.atomic(body(lm)) # FALSE
environment(func) # <environment: R_GlobalEnv>
length(environment(func)) # 6
mode(environment(func)) # environment
storage.mode(environment(func)) # environment
environment(lm) # <environment: namespace:stats>
length(environment(lm)) # 1117
mode(environment(lm)) # environment
storage.mode(environment(lm)) # environment
class(environment(lm)) # environment
attributes(environment(lm)) # NULL

# Among all other R objects, factors are special compound objects which are used used to describe items that can have a finite number of values (gender, social class, etc.). 
# A 'factor' has a 'levels' attribute and class 'factor', and it may also contain a 'contrasts' attribute which controls the parametrization used when the factor is used in a modeling functions. 
# A 'factor' may be purely nominal or may have ordered categories, and they are are currently implemented using an integer array to specify the actual levels and a second array of names that are mapped to the integers.
# Note that while factors look (and often behave) like character vectors, they are actually integers (Some string methods like gsub() and grepl() will coerce factors to strings).
# A 'data frame' is a list of vectors, factors, and/or matrices all having the same length (number of rows in the case of matrices). 
# In addition, a data frame generally has a names attribute labeling the variables and a row.names attribute for labeling the cases. 
# A data frame can contain a list that is the same length as the other components, and the list can contain elements of differing lengths thereby providing a data structure for ragged arrays. 
# Another important phenomemon to remember is that most data loading functions in R automatically convert character vectors to factors.
# To resolve the issue, one may use the argument stringsAsFactors = FALSE to suppress this behavior, and then manually convert character vectors to factors using your knowledge of the data.

# A data frame is a list of vectors, factors, and/or matrices all having the same length (number of rows in the case of matrices).
# In addition, a data frame generally has a 'names' attribute labeling the variables and a 'row.names' attribute for labeling the cases.
# Data frames are the best R objects that handle data manipulation well, and they closely resemble those datasets in SAS.

factor1 <- factor(c("a", "b", "b", "a"))
factor1
levels(factor1) # "a" "b"
typeof(factor1) # integer
length(factor1) # 4
mode(factor1) # numeric
storage.mode(factor1) # integer
class(factor1) # factor
attributes(factor1)
names(factor1) # NULL
factor2 <- rev(factor(letters))
factor2
factor3 <- factor(letters, levels = rev(letters))
factor3
data(trees) # loading the dataset 
trees
typeof(trees) # list
length(trees) # 3
mode(trees) # list
storage.mode(trees) # list
class(trees) # data.frame
is.atomic(trees) # FALSE
dim(trees) # this gives the dimensionality of the data matrix
dimnames(trees) # this gives the observation numbers and variable names (lists)
names(trees)
attributes(trees)
var.t <- trees$Girth # pulling out one variable
names(var.t) # NULL
typeof(var.t) # double
length(var.t)
mode(var.t) # numeric
class(var.t) # numeric


# III. Evaluating Expressions

# When a user types a command at the prompt (or when an expression is read from a file) the first thing that happens to it is that the command is transformed by the parser into an internal representation.
# The evaluator executes parsed R expressions and returns the value of the expression. 
# All expressions have a value, and this is the core of the R language.
# We first take a look at simple evaluations, and then move onto more complicated topics such as control structures, elementary arithmetic operations, indexing and scope of variables. 

# Simple evaluations in R include constants, symbo lookup, function calls, and operators. 
# For constants, any number typed directly at the prompt is a constant and is instantly evaluated (numeric, not integer).
# If we type a numeric number and we want the result to be resolved to an integer, we can either use the as.integer() function or the 'L' suffix to qualify any number with the intent of making it an explicit integer.
# When a new variable is created it must have a name so it can be referenced and it usually has a value, and the name itself is a symbol.
# For R functions, most of the computations carried out in R involve the evaluation of functions, which we refer to as function invocation.
# Functions are invoked by name with a list of arguments separated by commas.
# Function calls can have tagged (or named) arguments, for example, plot(x, y, pch = 3).
# Arguments without tags are known as positional since the function must distinguish their meaning from their sequential positions among the arguments of the call.
# R also contains a number of operators, which are essentially functions mathematically. 
# Examples include: "~" (used for model formulae), "%%" (modulus, binary), "%/%" (integer divide, binary), "%*%" (matrix product), "%o%" (outer product), %x% (Kronecker product, binary), "$" (list subset, binary) etc.
# In particular, "&" stands for vectorized "and" and "&&" stands for non-vectorized "and".
# Similarly, "|" stands for vectorized "or", and "||" stands for non-vectorized "or".
# R deals with entire vectors of data at a time, and most of the elementary operators and basic mathematical functions like log are vectorized.
# This means that adding two vectors of the same length will create a vector containing the element-wise sums, implicitly looping over the vector index.

# We now discuss the control structures within R (e.g. 'if/else', 'looping' such as 'repeat', 'while', 'for', and 'switch').
# To start with, computatoins in R consists of sequentially evaluating statements, which can be separated by either a semicolon or a new line.
# Whenever the evaluator is presented with a syntactically complete statement, that statement is evaluated and the value returned. 
# A semicolon always indicates the end of a statement while a new line may indicate the end of a statement, and if the current statement is not syntactically complete, new lines are simply ignored by the evaluator.
# Statements can be grouped together using braces '{' and '}' (a group of statements is sometimes called a block).
# In standard R languages, a statement could mean either a single statement or a block.

# The if/else statement (block) is the basic control structure in R, which has similar functionalities with the ifelse() function.
# The syntax for the ifelse() function is ifelse(test_condition, yes, no), where 'yes' returns values for the true elements of 'test_condition'.
# The syntax for the if/else statement is similar: if (statement1) statement2 else statement3.
# We can also nest the if/else statements and perform multiple conditional execution. 
# This is simlar to the if...else if...else if....else statements in SAS, but it can only have at most 8 clauses. 
# The general syntax for the if/else block is given by the following:
#     if (statement1) {
#       statement2
#       } else if (statement3) {
#         statement4
#         } else if (statement5) {
#           statement6
#           } else
#             statement8
# To chain more clauses in the if/else block, we will need to nest the ifelse() functions.
# The general syntax for chaining if/else conditionals is given the following:
#     ifelse(statement_1, true_condition_1,
#       ifelse(statement_2, true_condition_2,
#         ifelse(statement_3, true_condition_3, 
#           ifelse(statement_4, true_condition_4,
#             ...
#               ifelse(statement_k, true_condition_k, false_condition_k)
#             )
#           ...
#         )
#       )
#     )

v1 <- c(10, 8, 89, 2, 35)
v2 <- ifelse(v1>10, 1, 0)
v2
v3 <- ifelse(v1>10, 'Correct', 'Wrong')
v3
v4 <- 10.5
v5 <- if (v4<9) print("v4<9") else print("v4>=9")
v6 <- if (any(v1<9)) print("At least one elemeents of v1 is smaller than 9") else print("All elements of v1 is smaller than 9")
if (v4<9) {
  print("v4<9") 
    } else if (v4>=9 && v4<10) {
      print("v4 in beteen 9 and 10") 
       } else if (v4==10) {
         print("v4 equals 10")
         } else print("v4>10") 

printit <- ifelse(v4==0 || v4<9, "Impossible",
             ifelse(v4>=8 && v4<9, "[8, 9)",
               ifelse(v4==9, "v4=9", 
                 ifelse(v4>9 & v4<10, "(9, 10)", 
                   ifelse(v4==10, "v4=10", 
                     ifelse(v4>10, "v4>10", "False")
                   )
                 )
               )
             )
           )
printit 

# R can perform looping in both explicit ways (for', while', 'repeat') and implicit ways (e.g. 'tapply', 'lapply' etc.).
# The 'repeat' statement causes repeated evaluation of the body until a break is specifically requested (usually, using the rep() function will suffice). 
# This means that you need to be careful when using repeat because of the danger of an infinite loop.  
# The syntax for the 'repeat' statement is given by the following:
#     repeat {
#       block_statement
#       }
# The syntax for the 'while' statement is given by the following:
#     while (condition) {
#       statement_1
#       statement_2
#       ...
#       statement_n
#       if (break_condition) break
#       }
# The syntax for the 'for' statement is given by the following:
#     for (name in vector) {
#       statement_1
#       statement_2
#       ...
#       statement_n
#       }
# The vector in the for statement can be either a vector or a list. 

total <- 0
repeat {
  total <- total + 1
  print(total)
  if (total > 6) break
  }
w <- 1
while(w<5) {
  w <- w+1
  print(w)
  }
u1 <- 1:5
u2 <- NULL
for(i in seq(along=u1)) { 
  if(u1[i]<3) u2 <- c(u2, u1[i]-1) else u2 <- c(u2, u1[i]/u1[i])                                                     
  }
u2

# Lastly for the control structures, the switch() function evaluates the underlying expression and accordingly chooses one of the further arguments.
# The function essentially does the same job as the if/else statement, except that switch() performs more efficiently according to certain empirical studies on R performance.

test1 <- function(type) {
         switch(type, mean = 1, median = 2, mode=2.7)
         }
test2 <- function(type) {
         if (type == "mean") 1
         else if (type == "median") 2
         else 2.7
         } 
test1('mean')
test2('mean')
test1('mode')
test2('mode')

# We now move onto the discussion of the evaluation of elementary arithmetic operations.
# We discuss the finer points of the rules that apply to basic operation.
# Specifically, we focus on 3 aspects: 1) recycling rules, 2) dimensional attributes and 3)NA handling.
# We also introduce the match() function which are extensively used in data handling. 

# If one tries to add two structures with a different number of elements, then the shortest is recycled to length of longest.
# That is, if for instance you add c(1, 2, 3) to a six-element vector then you will really add c(1, 2, 3, 1, 2, 3).
# If the length of the longer vector is not a multiple of the shorter one, a warning is given. 
# If you are instead adding matrix, dimensions must match, and if one of the matrix is a (column) vector, then R first recyles, and then checks if the dimensions fit (and will error if not).

u1
w
M
u1+w
u2
u2+var.t
M+u1
m1 <- matrix(1:5, 2) # since 5 is not a multiple of the number of rows then the elements are recycled 
m1

# Missing values in the statistical sense, that is, variables whose value is not known, have the value NA.
# Numeric and logical calculations with NA generally return NA, and in cases where the result of the operation would be the same for all possible values the NA could take, the operation may return this value. 
# In particular, 'FALSE & NA' is FALSE, 'TRUE | NA' is TRUE, and NA is not equal to any other value or to itself.
# Numeric calculations whose result is undefined, such as '0/0', produce the value NaN, which exists only in the double type and for real or imaginary components of the complex type (i.e. not a number).
# The function is.nan() is provided to check specifically for NaN.
# Coercing NaN to logical or integer type gives an NA of the appropriate type, but coercion to character gives the string "NaN".
# NaN values are incomparable, so tests of equality or collation involving NaN will result in NA.

0/0 # NaN
1/0 # Inf
is.na(NA) # TRUE
is.na(NaN) # TRUE
is.nan(NaN) # TRUE
is.nan(NA) # FALSE
u2[6] # NA, b/c length(u2)=5

# There is an exception that an NA value will match another NA value.
# This exception is the match() function, which returns a vector of the positions of (first) matches of its first argument in its second.
# The syntax for the match() function is given by match(x, table, nomatch = NA_integer_, incomparables = NULL).
# Here, the argument 'x' is a vector or NULL to be matched, and the 'table' argument is a vector or NULL to be matched against. 
# The 'nomatch' argument gives the value to be returned in the case when no match is found (note that this is coerced into integers).
# The last argument is a vector of values that cannot be matched, so that any value in 'x' matching a value in this vector is assigned the nomatch value.
# Factors, raw vectors and lists are converted to character vectors, and then 'x' and 'table' are coerced to a common type (the later of the two types in R's ordering) before matching.
# If the 'incomparables' argument has positive length, then it is coerced to the common type. 
# Note also that matching for lists is potentially very slow and best avoided except in simple cases. 
# For real and complex values, NaN values are regarded as matching any other NaN value, but not matching NA. 

u3 <- c(2,5,6,1,NA,5,0,2,3,7,6,8)
u4 <- 9:1
u5 <-match(u3, u4)
u5 # 8 5 4 9 NA 5 NA 8 7 3 4 2

# Now we discuss indexing in R beyond indexing components of a vector (e.g. matrices, lists, and multi-dimensional arrays).
# Of course, indexing can be used both to extract part of an object and to replace parts of an object (or to add parts), and in general, R allows many powerful constructions using vectors as indices. 
# R has 3 basic indexing operators, with syntaxes displayed by the following examples: x[i], x[i,j], x[[i]], x[[i,j]], x$a, and x$"a".
# For vectors and matrices, the double brackets forms are rarely used, though they have some slight semantic differences from the single bracket form.
# For lists, one generally uses double brackets to select any single element, whereas single bracket returns a list of the selected elements. 
# The form using "$" applies to recursive objects such as lists and pairlists.
# It allows only a literal character string or a symbol as the index (i.e., the index is not computable).
# When "$" is applied to a non-recursive object, the result could be NULL or an error, depending on the version of R..

# The simplest indexing paradigm is given by the single bracket, which mainly applies to vectors.
# Suppose we have a vector object x, and we index it by x[i], then the following possibilities exist according to the type of i: integer and numeric, logical, character, factor, empty and NULL.
# For integers: all elements of i must have the same sign, and if they are positive, the elements of x with those index numbers are selected.
# If i contains negative elements, all elements except those indicated are selected.
# In addition, if i is positive and exceeds length(x) then the corresponding selection is NA.
# A special case is the zero index, which has null effects (x[0] is an empty vector and otherwise including zeros among positive or negative indices has the same effect as if they were omitted).
# Non-integer values are converted to integer (by truncation towards zero) before use.
# For logical: i should generally have the same length as x, and if it is shorter, then its elements will be recycled, whereas if it is longer, x is conceptually extended with NAs.
# For character: the strings in i are matched against the names attribute of x and the resulting integers are used.
# For the double brackets and the '$' sign, partial matching is used if exact matching fails, so that x$aa will match x$aabb if x does not contain a component named "aa", and "aabb" is the only name which has prefix "aa".
# For double brackets, partial matching can be controlled via the exact argument which defaults to NA indicating that partial matching is allowed, but should result in a warning when it occurs.
# Setting exact to 'TRUE' prevents partial matching from occurring, a 'FALSE' value allows it and does not issue any warnings.
# Note that the single bracket always requires an exact match. 
# The string "" is treated specially: it indicates 'no name' and matches no element (not even those without a name). 
# For factors: the result of indexing a factor is identical to x[as.integer(i)] (i.e. the factor level is never used).
# For empty: the expression x[] returns x, but drops “irrelevant” attributes from the result (only 'names' and in multi-dimensional arrays 'dim' and 'dimnames' attributes are retained).
# Indexing with a missing (NA) value gives an NA result (the rule applies also to the case of logical indexing, so that the elements of x that have an NA selector in i get included in the result, but their value will be NA).

u3
c(class(u3), mode(u3))
u3[1]
u3[5]
v3
c(class(v3), mode(v3))
v3[2]
y
y[-3]
y[-4:-1] # logical(0)
z
z[1]
z[[1]] # this is different from z[1]
z[[1]][3] # "C"
z[[8]][2] # "Daniel Barenboim"
z[4]
z[[4]]
z$sequence
z$seq # this gives the same result as z$sequence
z$dsn[1:2, 1:4]
z[0]
z[]
factor1
factor1[as.integer(1)] # this gives the same result as factor1[1]

# We now go over indexing matrices and arrays, which can be considered as a multiple subscripted collection of data entries.
# A vector can be used by R as an array only if it has a dimension vector as its 'dim' attribute. 
# Subsetting multi-dimensional structures generally follows the same rules as single-dimensional indexing for each index variable, with the relevant component of 'dimnames' taking the place of 'names'. 
# Normally, a structure is accessed using the number of indices corresponding to its dimension.
# It is also possible to use a single index in which case the 'dim' and 'dimnames' attributes are disregarded and the result is effectively that of c(m)[i] (so m[i], m[i, ] and m[ ,i] could all be different).
# It is possible to use a matrix of integers as an index, but in general we do not recommend this practice, as the coding could be confusing and hard to debug.
# In this case the number of columns of the matrix should match the number of dimensions of the structure, and the result will be a vector with length as the number of rows of the matrix. 

m2 <- matrix(1:6, 3) # this is equivalent to matrix(data=1:6, nrow=3, byrow=FALSE)
m2
m2[1,2] # 4
m2[1, ] # 1 4
m2[, 2] # 4 5 6
m2[6] # 6 (this way of indexing is not recommended)
m2[7] # NA
judge.m2 <- c(is.matrix(m2[1, ]), is.atomic(m2[1, ]), class(m2[1, ]), mode(m2[1, ]), typeof(m2[1, ]))
judge.m2 # "FALSE" "TRUE" "integer" "numeric" "integer"
m3 <- matrix(c(1, 3, 4, NA), 2, byrow = TRUE)
m3
m3[2] # 4 (this way of indexing is not recommended)

# We now officially study arrays in R, which can be rigorously defined as (numeric) matrix objects with dimension attributes.
# So vectors are distinct from one-dimensional arrays in that the latter have 'dim' and 'dimnames' attributes (both of length one). 
# One can think of arrays as multi-dimensional matrices with 'dimnames' attributes.
# To understand arrays better, we will first construct an array, and then study how to index them.

array.1<- seq(1:30)
dim(array.1) <- c(3,5,2) # note that 3*5*2=30 creating 2 matrices each with 3 rows and 5 columns
array.1
judge.array.1 <- c(dim(array.1), class(array.1), mode(array.1), typeof(array.1))
judge.array.1
array.2 <- array(1:20, dim=c(4,5,1))
array.2
names(array.2) # note that the result is NULL, but for data frame this will not be NULL
judge.array.2 <- c(dim(array.2), class(array.2), mode(array.2), typeof(array.2))
judge.array.2
fake.array <- array(1:20, dim=c(4,5))
judge.fake.array <- c(dim(fake.array), class(fake.array), mode(fake.array), typeof(fake.array), is.array(fake.array))
judge.fake.array

# To index arrays, we need to specify all dimensions of the array. 
# Single indexing on an array is not recommended, albeit works from a syntax perspective. 

array.1
array.1[1,1,2] # 16 
array.1[,,1]
array.1[1,,]
judge3 <- c(dim(array.1[1,,]), class(array.1[1,,]), mode(array.1[1,,]), typeof(array.1[1,,]))
judge3
array.1[,2,]
judge4 <- c(dim(array.2[,2,]), class(array.2[,2,]), mode(array.2[,2,]), typeof(array.2[,2,]))
judge4
array.1[,3,2] # 22 23 24
array.1[16] # 16 (this way of indexing is not recommended)
array.2[21] # NULL (this way of indexing is not recommended)

# We now discuss the scope of variable, as almost every programming language has a set of scoping rules, allowing the same name to be used for different objects.
# R uses a "lexical scoping model" (also known as a static scoping model).
# It is a convention used with many programming languages that sets the scope (range of functionality) of a variable so that it may only be called from within the block of code in which it is defined.
# There are several important high-level concepts that need to be explained in detail.
# The first is the concept of global environment, which is the root of the user workspace. 
# An assignment operation from the command line will cause the relevant object to belong to the global environment.
# Its enclosing environment is the next environment on the search path, and so on back to the empty environment that is the enclosure of the base environment. 
# The second is the concept of lexical environment, and to understand this we first need to understand the definition of 'frames'.
# Simply put, every call to a function creates a 'frame' which contains the local variables created in the function, and is evaluated in an environment, which in combination creates a new environment. 
# A frame is a set of variables, whereas an environment is a nesting of frames (or equivalently, the innermost frame plus the enclosing environment). 
# Environments may be assigned to variables or be contained in other objects.
# A 'closure object' (mode 'function') will contain the environment in which it is created as part of its definition.
# When the function is subsequently called, its evaluation environment is created with the closure’s environment as enclosure (note that this is not necessarily the environment of the caller).
# Thus, when a variable is requested inside a function, it is first sought in the evaluation environment, then in the enclosure, the enclosure of the enclosure, etc.
# Once the global environment or the environment of a package is reached, the search continues up the search path to the environment of the base package. 
# If the variable is not found there, the search will proceed next to the empty environment, and will fail. 

# We now go over the abstruse concept of call stacks, which is extremely hard to understand as a first-time programmer without a computer science degree. 
# Every time a function is invoked a new evaluation frame is created.
# At any point in time during the computation the currently active environments are accessible through the something called the 'call stack'.
# Each time a function is invoked a special construct called a 'context' is created internally and is placed on a list of contexts.
# When a function has finished evaluating its context is removed from the call stack.
# Making variables defined higher up the call stack available is called dynamic scope.
# The binding for a variable is then determined by the most recent (in time) definition of the variable.
# This contradicts the default scoping rules in R, which use the bindings in the environment in which the function was defined (lexical scope).
# Some functions, particularly those that use and manipulate model formulas, need to simulate dynamic scope by directly accessing the call stack.
# Access to the call stack is provided through a family of functions which have names that start with 'sys.' (we will skip the details for simplicity).

# In addition to the evaluation environment structure, R has a search path of environments which are searched for variables not found elsewhere. 
# This is used for two things: packages of functions and attached user data. 
# The first element of the search path is the global environment and the last is the base package. 
# An 'Autoloads' environment is used for holding proxy objects that may be loaded on demand. 
# Other environments are inserted in the path using attach() or library(). 
# Packages which have a 'namespace' have a different search path.
# If a search for an R object is started from an object in such a package, the package itself is searched first, then its imports, then the base namespace and finally the global environment and the rest of the regular search path.
# The effect is that references to other objects in the same package will be resolved to the package, and objects cannot be masked by objects of the same name in the global environment or in other packages. 


# IV. Functions

# R allows user-defined language, and the details can be found in the official documentation 'Writing R Extensions' (this can be found under the 'Help' tab of R when you open up an R session).
# The syntax for writing a function is given by function (arglist) body, where 'arglist' stands for argument list.
# A formal argument can be a symbol, a statement of the form 'symbol = expression', or the special formal argument '...'. 
# Generally, the body is a group of expressions contained in curly braces ('{' and '}'), and functions are assigned to symbols but they do not need to be. 
# The value returned by the call to function() is a function, and if this is not given a name it is referred to as an anonymous function. 
# Anonymous functions are most frequently used as arguments to other functions (they are anonymous because they don't need a name).
# Functions are first class objects in R, and they can be used anywhere that an R object is required. 
# In particular they can be passed as arguments to functions and returned as values from functions. 

# When a function is called or invoked a new evaluation frame is created.
# In this frame the formal arguments are matched with the supplied arguments according to the rules below:
# 1) Exact matching on tags: for each named supplied argument the list of formal arguments is searched for an item whose name matches exactly.
# 2) Partial maching on tags: each remaining named supplied argument is compared to the remaining formal arguments using partial matching.
#    If the name of the supplied argument matches exactly with the first part of a formal argument then the two arguments are considered to be matched.
#    If the formal arguments contain ‘...’ then partial matching is only applied to arguments that precede it.
# 3) Positional matching: any unmatched formal arguments are bound to unnamed supplied arguments in order.
#    If there is a ‘...’ argument, it will take up the remaining arguments, tagged or not.
#    If any arguments remain unmatched at this moment after the 3-pass process, an error is declared.

# One of the most important things to know about the evaluation of arguments to a function is that supplied arguments and default arguments are treated differently. 
# The supplied arguments to a function are evaluated in the evaluation frame of the calling function, whereas the default arguments to a function are evaluated in the evaluation frame of the function.
# The semantics of invoking a function in R argument are 'call-by-value'.
# In general, supplied arguments behave as if they are local variables initialized with the value supplied and the name of the corresponding formal argument. 
# Changing the value of a supplied argument within a function will not affect the value of the variable in the calling frame.
# It is possible to access the actual (not default) expressions used as arguments inside the function (and the mechanism is implemnted via 'promises'.)
# When a function is being evaluated the actual expression used as an argument is stored in the promise together with a pointer to the environment the function was called from. 
# When (if) the argument is evaluated the stored expression is evaluated in the environment that the function was called from. 
# Since only a pointer to the environment is used any changes made to that environment will be in effect during this evaluation.
# The resulting value is then also stored in a separate spot in the promise, and subsequent evaluations retrieve this stored value (a second evaluation is not carried out). 

# When a function is called, each formal argument is assigned a promise in the local environment of the call with the expression slot containing the actual argument (if it exists) and the environment slot containing the environment of the caller.
# If no actual argument for a formal argument is given in the call and there is a default expression, it is similarly assigned to the expression slot of the formal argument, but with the environment set to the local environment.
# The process of filling the value slot of a promise by evaluating the contents of the expression slot in the promise’s environment is called forcing the promise. 
# A promise will only be forced once, the value slot content being used directly later on (a promise is forced when its value is needed).
# When a promise is forced, it is usually happening inside internal functions, but  a promise can also be forced by direct evaluation of the promise itself. 
# This is occasionally useful when a default expression depends on the value of another formal argument or other variable in the local environment. 
# The example illustrates the the point above, where the lone 'label' ensure that the label is based on the value of x before it is changed in the next line. 
# The deparse() function below turns unevaluated epxressions into character strings (a typical use of this is to create informative labels for datasets and plots).

x=c(3,4,5)
promising <- function(x, label = deparse(x)) {
               label
               x <- x + 1
               print(label)
             }
promising(x) # "c(3,4,5)"

# We now talk about scope, or scoping rules, which are simply the set of rules used by the evaluator to find a value for a symbol. 
# Scope or the scoping rules are simply the set of rules used by the evaluator to find a value for a symbol. 
# Every computer language has a set of such rules. In R the rules are fairly simple but there do exist mechanisms for subverting the usual, or default rules.
# R adheres to a set of rules that are called lexical scope. 
# This means the variable bindings in effect at the time the expression was created are used to provide values for any unbound symbols in the expression.
# Most of the interesting properties of scope are involved with evaluating functions and we focus on this issue. 
# A symbol can be either bound or unbound, and all of the formal arguments to a function provide bound symbols in the body of the function. 
# Any other symbols in the body of the function are either local variables or unbound variables.
# A local variable is one that is defined within the function. 
# Because R has no formal definition of variables, they are simply used as needed, it can be difficult to determine whether a variable is local or not. 
# Local variables must first be defined, and this is typically done by having them on the left-hand side of an assignment.

# During the evaluation process, if an unbound symbol is detected then R attempts to find a value for it. 
# The scoping rules determine how this process proceeds. 
# In R the environment of the function is searched first, then its enclosure and so on until the global environment is reached.
# The global environment heads a search list of environments that are searched sequentially for a matching symbol, and the value of the first match is then used.
# When this set of rules is combined with the fact that functions can be returned as values from other functions then some rather nice, but at first glance peculiar, properties obtain.

f <- function() {
         y <- 10
         g <- function(x) x + y
         return(g)
     }
h <- f()
h(3)

# Based on the example above, a rather interesting question is what happens when h is evaluated. 
# To describe this we need a bit more notation, and within a function body variables can be bound, local or unbound. 
# The bound variables are those that match the formal arguments to the function, whereas the local variables are those that were created or defined within the function body (the unbound variables are neither local nor bound).
# When a function body is evaluated there is no problem determining values for local variables or for bound variables. 
# Scoping rules determine how the language will find values for the unbound variables.
# Thus, when h(3) is evaluated we see that its body is that of g, and within that body x is bound to the formal argument and y is unbound. 
# In a language with lexical scope x will be associated with the value 3 and y with the value 10 local to f so h(3) should return the value 13. 
# In R this is indeed what happens.


# V. R as an Object-Oriented Programming

# We now study OOB.







# VI. Computing

# VII. Interfaces

# VIII. Exception Handling and Debugging

# IX. Parser



# References:
# http://stat.ethz.ch/R-manual/R-devel/doc/manual/R-lang.html
# http://adv-r.had.co.nz
# http://www.burns-stat.com/pages/Tutor/R_inferno.pdf
# https://cran.r-project.org/doc/manuals/R-exts.html#Writing-R-documentation
