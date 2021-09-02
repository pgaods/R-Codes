# Association Rule Learning 

#############################################


# I. Data Loading 

# Association rules are rules presenting association or correlation between itemsets. 
# This type of rule induction is often used in market basket analysis in which stores like Amazon extract rules to see what customers buy in bundles.
# An example would be {beer}->{diapers}, which suggests a strong relationship between the sale of beer and the sale of diapers. 
# Retailers can use this type of rules to help them identify new opportunities for cross-selling their products to the customers. 

# There are many applications of association rule learning, and below lists some notable examples in social science researches and projects:
#   1) Discovering demographic characteristics and behaviours of customers who are likely/unlikely to switch to other telcos
#   2) Finding characteristics of customers who are likely to default on credit card or mortgage
#   3) Finding relationships between individual stocks, or between stocks and economic factors
#   4) Identifying relationships between symptoms, test results and illness

# To proceed with our analysis, we first need to understand some basic concepts in association rule learning. 
# The usual dataset for association rule learning looks like the following: each record (row) is a transaction and each attribute (column) is an item.
# The data can be represented by a matrix of binary indicators (0 or 1, with 1 representing customer bought this item).
# A collection of zero or more items is called an itemset (the null set is an itemset that does not contain any items).
# If an itemset contains m items, it is called a m-itemset (e.g. {beer, diapers, milk} is a 3-itemset).
# In association analysis, the transaction width is defined as the number of items present in a transaction.
# A transaction t is said to contain an itemset X if X is a subset of t (t is a set here).
# The support count of an itemset is the number of transactions that contain a particular itemset. 
# An association rule is an implication expression of the form X->Y where X and Y are disjoint itemsets. 
# The strength of an association rule can be measured in terms of its support and confidence. 
# The support determines how often a rule is applicable to a given dataset, while the confidence determines how frequently items in Y appear in transaction that contain X.
# Mathematically, support(X->Y)=Pr(X and Y), and confidence(X->Y)=Pr(Y|X). 
# Support is an important measure because a rule that has very low support may occur simply by chance.
# A low support may also mean that the it's not profitable to promote items that customers seldom buy together.
# Confidence measures the reliability of the inference made by a rule. 
# For a given rule X->Y, the higher the confidence, the more likely it is for Y to be present in transactions that contain X. 
# Lift is defined as Pr(X or Y)/Pr(X)Pr(Y), which is another important measure of association strength. 
# Lifts are often interpreted as the deviation of the support of the whole rule from the support expected under independence given the supports of both sides of the rule. 
# Greater lift values(>1) indicate stronger associations.
# Measures like support, confidence and lift are generally called interest measures because they help with focusing on potentially more interesting rules.

# Researchers also often talk about the downward-closure property of support, which will be useful to prune the rules later. 
# In particular, for a frequent itemset, all its subsets are also frequent (if {A,B} is frequent, then both {A} and {B} are frequent).
# On the other hand, for an infrequent itemset, all its super-sets are infrequent (if {A} is infrequent, then {A,B}, {A,C} and {A,B,C} are infrequent).

# Association rules are typically generated in a two-step process. 
# First, minimum support is used to generate the set of all frequent itemsets for the data set. 
# Frequent itemsets are itemsets which satisfy the minimum support constraint. 
# Then, in a second step, each frequent itemsets is used to generate all possible rules from it and all rules which do not satisfy the minimum confidence constraint are removed. 

# We now pull the 'Titanic dataset in the 'datasets' package which describes the fate of passengers. 
# The dataset records social class, sex, age, and survival information for the passengers.
# We would like to use association rule learning to see if there are any association with these variables.
# Since the dataset is not a data frame, we need to do some data wrangling first to convert it to a data frame. 

PackageTest <- function(x){
                 if (!require(x,character.only = TRUE)){
                   install.packages(x,dep=TRUE)
                   if(!require(x,character.only = TRUE)) stop("Package not found")
                 }
               }
PackageTest('datasets')
PackageTest('dplyr')
PackageTest('arules')
PackageTest('arulesViz') # used for apriori rule visualization
PackageTest('tm')

data(Titanic)
Titanic
class(Titanic) # this is a table so we need to convert it into a data frame first
dsn=as.data.frame(Titanic)

# We create a user-defined function that counts how many columns are categorical variables (b/c association rule learning only cares about categorical variables). 

count_factors <- function(dataframe){
                   factor_set <- as.list(sapply(dataframe, is.factor))
                   factor_count=length(names(which(factor_set==TRUE)))
                 }
count_factors(dsn)
k=count_factors(dsn) # getting number of columns
k

# We now need to 'blow up' the dataset because each record of the dataset has a frequency associated with it. 

dsn2<-NULL
for (i in 1:k) {
  dsn2 <- cbind(dsn2, rep(as.character(dsn[ ,i]), dsn$Freq))
}
dsn2 <- as.data.frame(dsn2)
names(dsn2) <- names(dsn)[1:4] # assigning the names from the orignal dataset to the current one
dsn <- dsn2 # renaming the dataset


# II. EDA 

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
proc.missing <- function(dataframe) {
                  sapply(dataframe, function(x) sum(is.na(x)))   
                }

proc.freq.allfactors(dsn)
proc.missing(dsn)
summary(dsn)


# III. The APRIORI Algorithms

# A classic algorithm for association rule mining is APRIORI, which is level-wise, breadth-first algorithm counting transactions to find frequent itemsets and then derive rules from them.
# Another algorithm for association rule mining is the ECLAT algorithm, which finds frequent itemsets with equivalence classes, depth-first search and set intersection instead of counting.  
# Below we demonstrate association rule mining with the apriori() function in the 'arules' package.
# The default settings are supp=0.1 (minimum support of the rule is 0.1), conf=0.8 (minimum confidence of the rules is 0.8), and maxlen=10 (maximum length of rule/transaction width=10). 

rules.all <- apriori(dsn, parameter = list(supp = 0.005, conf = 0.8, target = "rules"))
quality(rules.all) <- round(quality(rules.all), digits=3)
arules::inspect(rules.all)
mode(rules.all) # this is an S4 object

# As a common phenomenon for association rule mining, many rules generated in the first time are uninteresting.
# Suppose that we are only interested in rules with rhs indicating survival, so we set rhs=c("Survived=No", "Survived=Yes") in the "appearance" parameter to achieve this. 
# Also, if you would like to excluce empty rules, you can set minlen=2.
# Moreover, the details of progress are suppressed with verbose=F.
# Below is our code after being sorted by the lift of the rules:

clean.rules <- apriori(dsn, control=list(verbose=F), parameter=list(minlen=2, supp=0.005, conf=0.8), appearance=list(rhs=c('Survived=No', 'Survived=Yes'), default='lhs')) 
quality(clean.rules) <- round(quality(clean.rules), digits=3)
clean.rules2 <- sort(clean.rules, by="lift")
arules::inspect(clean.rules2)

# Notice that when other settings are unchanged, with a lower minimum support, more rules will be produced, and the associatoins between itemsets shown in the rules will be more likely to be by chance. 
# Notice also that support, confidence, and lifts are three common measures for selecting interesting association rules.
# Besides them, there are many other measures, such as chi-square, conviction, gini and leverage etc. (the 'arules' package provides these options).

# We have generated a relatively cleaner rule, and now we need to do some further rule-pruning, because some rules are still redundant at this point. 
# For example, if you see a rule having one additional condition on the lhs yet gaining nothing on the rhs is considered redundant. 
# More formally, when a rule is a super rule of another rule, and the former has the same or a lower lift, the former rule is considered to be redundant. 
# Below, we use the following command to achieve the pruning task for rules. 
# In addition to rule-pruning, sometimes we would also need to subset the rules because sometimes we are only interested in a subset of the rules for a particular business reason. 
# Keep in mind that it's always good to prune the rules first before any subsetting is peformed. 

subset.matrix <- is.subset(clean.rules2, clean.rules2)
L <- lower.tri(subset.matrix, diag=T)
subset.matrix[L] <- F
redundant <- colSums(subset.matrix, na.rm=T)>=1
cat("These are the redundant rules that need to be pruned away:", "\n")
which(redundant) # this tells us which rules are redundant
rules.pruned <- clean.rules2[!redundant]
arules::inspect(rules.pruned)
rm(subset.matrix, L, redundant)

# We now subset the rules based on our own criteria.

rules.y <- subset(rules.pruned, subset = rhs %in% "Survived=Yes" & lift > 2.7) # subsetting the rules
rules.n <- subset(rules.pruned, subset = rhs %in% "Survived=No" & support >= 0.1) # subsetting the rules
arules::inspect(rules.y)
arules::inspect(rules.n)


# IV. Interpretation and Visualization 

# While it is easy to find high-lift rules from data, it is not an easy job to understand the identified rules.
# It is common that the association rules are miinterpreted to find their business meanings. 
# For instance, {Class=2nd, Age=Child}->{Survivied='Yes'} has a confidence of 1 and a lift of 3 and there are no rules on children of the 1st or 3rd classes.
# Therefore, it might be tempting to interpret it as children of the 2nd class had a higher survival rate than other children. 
# But this interpretation is wrong, as the rule states only that all children of clas 2 survived, but provides no information at all to cmopare the survival rates of different classes. 
# One must be super careful when it comes to reaching conclusions. 
# In general, by group comparison in association rule learning requires that we subset the rules properly beforehand.

rulesub <- apriori(dsn, parameter=list(minlen=3, supp=0.002, conf=0.2),
                   appearance=list(rhs=c("Survived=Yes"), lhs=c("Class=1st", "Class=2nd", "Class=3rd", "Age=Child", "Age=Adult"), default="none"), 
                   control=list(verbose=F))
rulesub2 <- sort(rulesub, by="confidence")
arules::inspect(rulesub2)

# In the above results, the first two rules show that children of the 1st class are of the same survival rate as children of the 2nd class and that all of them survived. 
# The rule of the 1st-class children didnt' appear before, simply because of its support was below the threshold pre-speciefied.
# Rule 5 appears to be a sad fact that children of class 3 had a low survival rate of 34%. 
# When we try to do further investigation, we need to make sure that the support is not too low but we want to make sure that the confidence is very high.

# Visualization is also available in the previous packages. 
# A straight-forward visualization of association rules is to use a scatter plot with two interest measures on the axes.
# The default method for plot() for association rules in 'arulesViz' is a scatter plot using support and confidence on the axes. 
# In addition a third measure (default: lift) is used as the color (gray level) of the points. 
# A color key is provided to the right of the plot.
# Matrix-based visualization techniques organize the antecedent and consequent itemsets on the x and y-axes, respectively. 
# A selected interest measure is displayed at the intersection of the antecedent and consequent of a given rule. 
# If no rule is available for a antecedent or consequent combination the intersection area is left blank.
# The most common plots include scatter plots, balloon plots, graphs and parallel coordinates plots (there are many others, please see the reference section).

dev.new()
plot(rules.all, col='azure')
dev.new()
plot(rules.all, measure=c("support", "lift"), shading="confidence", col='orange')
dev.new()
plot(rules.all, method='grouped')
dev.new()
plot(rules.all, method='graph')



# References:
# http://www.rdatamining.com/docs/association-rule-mining-with-r
# ftp://cran.r-project.org/pub/R/doc/contrib/Zhao_R_and_data_mining.pdf
# https://www.r-bloggers.com/examples-and-resources-on-association-rule-mining-with-r/
# https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf 
