# Linear Panel Data with Balanced Data Example


#############################################


# I. Data Loading and Preprocessing

# We study linear panel data methods in this module (pooled OLS, fixed effect and random effect models).
# As an example, we will use the 'Medicare' dataset to see the model performance. 
# We consider T=6 years of data from 1990 to 1995 of data for inpatient hospital charges that are covered by the Medicare program. 
# This is a short panel data (small T large n), which is common in most of the microeconometris studies.
# We use the state as subjects so n=54 (all states plus Washington DC, Virgin Islands, Puerto Rico and an unspecified 'other' category).
# The response variable is the severity of component, covered claims per discharge, which we label as CCPD.
# This is our interest as the Medicare program reimburses hospitals on a per-stay basis at that point. 

getwd() 
setwd("C:/Users/pgao/Documents/PGZ Documents/Statistics Workshop/PDF textbooks/Machine Learning/The Elements of Statistical Learning/Datasets for Elements of Statistical Learning")  

PackageTest <- function(x){
                 if (!require(x,character.only = TRUE)){
                   install.packages(x,dep=TRUE)
                   if(!require(x,character.only = TRUE)) stop("Package not found")
                 }
               }
PackageTest("ggplot2")
PackageTest("plyr")
PackageTest("gridExtra")
PackageTest('sas7bdat')
PackageTest('nlme')
PackageTest('plyr')

Medicare <- read.sas7bdat("Medicare.sas7bdat") # you can also choose your file by typing Medicare = read.table(choose.files(), sep ="\t", quote = "",header=TRUE)
class(Medicare) # data frame
summary(Medicare)
str(Medicare)
print(Medicare[1:30, ])

# We now create some other variables to prepare for later regressions.

Medicare$AVE_DAYS= Medicare$TOT_D/Medicare$NUM_DCHG # average days
Medicare$CCPD=Medicare$COV_CHG/Medicare$NUM_DCHG # CCPD
Medicare$NUM_DCHG=Medicare$NUM_DCHG/1000 # number of dicharges per a thousand
Medicare$YEAR=Medicare$YEAR+1989
Medicare$FSTATE = factor(Medicare$STATE)


# II. EDA

# The first thing we should do for a panel data is to check if we have a balanced/unbalanced panel data. 
# For our current example, it is a balanced panel (we can create a function as below to achieve the check).
# The function can handle both long and short panels, and it will tell you whether the panel is balanced or not.

panel.balance.check <- function(dataframe, panel.subject, panel.time){
                         library(plyr)
                         panel_dimension=c(dim(dataframe))
                         panel.row=panel_dimension[1] # number of observations
                         panel.column=panel_dimension[2] # number of variables (columns)
                         print(paste("There are ", panel.row, " observations and ", panel.column, " variables altogether in the current dataset!"))  
                         if (panel.row > panel.column) {
                           print("This is a short panel (n>T).")
                           mytable1 <- table(panel.time,panel.subject)
                           mytable2 <- data.frame(margin.table(mytable1, 1)) # panel.time frequencies (summed over panel.subject) 
                           mytable3 <- rename(mytable2, c(Freq="panel.subject.count")) # need the 'plyr' packages
                           mytable3
                         } 
                         else if (panel.row < panel.column) {
                           print("This is a long panel (n<T)")
                           mytable4 <- table(panel.subject,panel.time) 
                           mytable5 <- data.frame(margin.table(mytable4, 1)) # panel.subject frequencies (summed over panel.time) 
                           mytable6 <- rename(mytable5, c(Freq="panel.timeperiod.count")) # need the 'plyr' packages
                           mytable6
                         }
                       }
panel.balance.check(dataframe=Medicare, panel.subject=Medicare$NMSTATE, panel.time=Medicare$YEAR)

# The gsummary() function in the 'nlme' package provides by group EDA analysis. 
# This function is useful because it helps us construct a by-group EDA analysis function. 

panel.bygroup.EDA <- function(dataframe, panel.time, y, x, groupvar) {
                       library(nlme)
                       attach(dataframe)
                       missing_count <- function(vector) {
                         mc <- sum(is.na(vector))
                         mc
                       }
                       by_avg <- as.matrix(gsummary(dataframe[, c(panel.time, y, x)], groups = groupvar, FUN=mean))
                       by_std <- as.matrix(gsummary(dataframe[, c(panel.time, y, x)], groups = groupvar, FUN=sd))
                       by_med <- as.matrix(gsummary(dataframe[, c(panel.time, y, x)], groups = groupvar, FUN=median))
                       by_min <- as.matrix(gsummary(dataframe[, c(panel.time, y, x)], groups = groupvar, FUN=min))
                       by_max <- as.matrix(gsummary(dataframe[, c(panel.time, y, x)], groups = groupvar, FUN=max))
                       by_mis <- as.matrix(gsummary(dataframe[, c(panel.time, y, x)], groups = groupvar, FUN=missing_count))
                       names(dimnames(by_avg)) <- list("", "\nMean value summary by the time periods")
                       names(dimnames(by_std)) <- list("", "\nStandard deviation value summary by the time periods")
                       names(dimnames(by_med)) <- list("", "\nMedian value summary by the time periods")
                       names(dimnames(by_min)) <- list("", "\nMinimum value summary by the time periods")
                       names(dimnames(by_max)) <- list("", "\nMaximum value summary by the time periods")
                       names(dimnames(by_mis)) <- list("", "\nMissing value count by the time periods")              
                       print(by_avg)
                       print(by_std)
                       print(by_med)
                       print(by_min)
                       print(by_max)
                       print(by_mis)
                       detach(dataframe)
                     }
panel.bygroup.EDA(Medicare, "YEAR", "CCPD", "NUM_DCHG", groupvar=YEAR)

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
proc.missing <- function(dataframe) {
                  sapply(dataframe, function(x) sum(is.na(x)))   
                }

proc.freq.allfactors(Medicare)
proc.missing(Medicare)

# We now do some graphical analysis, starting with boxplots. 
# The box plot (or box and whisker diagram) is a standardized way of displaying the distribution of data based on the five number summary (minimum, first quartile, median, third quartile, and maximum).  

dev.new()
attach(Medicare)
par(mfrow=c(2,2))
boxplot(CCPD ~ YEAR, col=rainbow(length(unique(YEAR))), main='CCPD per year')
boxplot(NUM_DCHG ~ YEAR, col=rainbow(length(unique(YEAR))), main='Number of discharges per year')
boxplot(AVE_DAYS ~ YEAR, col=rainbow(length(unique(YEAR))), main='Average days per year')
boxplot(MED_REIB ~ YEAR, col=rainbow(length(unique(YEAR))), main='Medical reimbursement per year')
detach(Medicare)

# We now conduct a series of plots designed particularly for appropriate panel data analysis.
# These plots include multiple time series plots, scatter plots (with symbols), and added variable plots. 

# The multiple time series plot uses time as its horizontal axis and and then plot a continuous variable based on different levels of the subjects.
# This plot is very appropriate if you have a small T and a small number of subjects, and it helps us to see whether the variable has a substantial individual variation across time (heterogeneity).
# Here we can see that not only are overall claims increasing but also that claims increase for each state.

dev.new()
ggplot(data = Medicare, aes(x=YEAR, y=CCPD)) + geom_line(aes(color=NMSTATE)) + ggtitle('Multiple Time Series Plot')

# The scatter plots in panel data analysis is usually a plot of the response versus one of hte covariate, connecting observations over time. 
# Here we are plotting CCPD vs the number of discharges, and it demonstrates a positive overall relationship between the response and the covariate. 
# Like CCPD, we see a substantial state variation of different numbers of discharges. 
# The plot also suggests that the number of discharges lagged by one year may be an important predictor of CCPD.

dev.new()
ggplot(data = Medicare, aes(x=NUM_DCHG, y=CCPD)) + geom_line(aes(color=NMSTATE)) + ggtitle('Number of Discharges in Thousands')

# Now we do the added variable plot, which is also called a partial regression plot/adjusted variable plots, or individual coefficients plot.
# In applied statistics, a partial regression plot attempts to show the effect of adding another variable to a model already having one or more independent variables. 
# In panel data analysis, a convention is first to create two least squares models by running a regression using the individual fixed effect on the response variable and the time effect respectively.
# Once this is done, we create two residual terms based on the previous model and plot that against each other. 
# Here, we are seeing that the rate of increase of CCPD over time is approximately consistent among states, yet there exists important variations. 

Med1.lm = lm(CCPD ~ FSTATE, data=Medicare)
Med2.lm = lm(YEAR ~ FSTATE, data=Medicare)
Medicare$rCCPD=residuals(Med1.lm)
Medicare$rYEAR=residuals(Med2.lm)

dev.new()
plot(rCCPD ~ rYEAR, data=Medicare, ylab="", xlab="", xaxt="n", yaxt="n")
for (i in Medicare$FSTATE) {
  lines(rCCPD ~ rYEAR, data = subset(Medicare, STATE== i)) }
axis(2, at=seq(-6000, 4000, by=2000), las=1, font=10, cex=0.005, tck=0.01)
axis(2, at=seq(-6000, 4000, by=200), lab=F, tck=0.005)
axis(1, at=seq(-3,3, by=1), font=10, cex=0.005, tck=0.01)
axis(1, at=seq(-3,3, by=0.1), lab=F, tck=0.005)
mtext("Residuals from CCPD", side=2, line=-8, at=5000, font=12, cex=1, las=1)
mtext("Residuals from YEAR", side=1, line=3, at=0, font=12, cex=1)

# Lastly we examine the Trellis plot, which is very popular when it comes to panel data. 
# This graphical technique takes its name from a trellis which is a structure of open latticework. 
# When viewing a house or garden, one typically thinks of a trellis as being used to support creeping plants such as vines. 
# We will use this lattice structure and refer to a trellis plot as consisting of one or more panels arranged in a rectangular array. 
# Graphs that contain multiple versions of a basic graphical form, each version portraying a variation of the basic theme, promote comparisons and assessments of change. 
# By repeating a basic graphical form, we promote the process of communication.

GrpMedicare = groupedData(CCPD ~ YEAR| NMSTATE, data=Medicare)
dev.new()
plot(GrpMedicare, xlab="YEAR", ylab="CCPD", scale = list(x=list(draw=FALSE)), layout=c(18,3), main='Trellis Plot')


# III. Pooled OLS Model 



# IV. The One-way Fixed Effect Model and Random Effect Model 






# References:
# http://instruction.bus.wisc.edu/jfrees/jfreesbooks/Longitudinal%20and%20Panel%20Data/Book/BookAnalysisR/Chap2AnalysisR.txt
# http://www.physics.csbsju.edu/stats/box2.html