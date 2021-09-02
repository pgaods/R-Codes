# Univariate Regression Splines Example


#############################################


# I. Data Loading 

# We now study regression splines, which is another way of nonparametric regressions, which includes kernel methods and series methods.
# Series methods approximate an unknown function with a flexible parametric function, with the number of parameters treated similarly to the bandwidth in kernel regression. 
# Essentially, what we intend to do is to use a linear combination of transformed variables of the covariates to fit the regression function.
# The regression function will still be linear in the parameters but the regressors will be functions of the covariates (e.g. polynomials).
# Mathematically, if we consider the regression function as a vector in a vector space (of functions), then series estimation in general is equivalent to re-designating the bases of the functions.
# A common series approximation is a continuous piecewise polynomial function known as a spline.
# While splines can be of any polynomial order (e.g. linear, quadratic, cubic, etc.), common choices includes quadratic and cubic. 
# To impose smoothness it is common to constrain the spline function to have continuous derivatives up to the order of the spline.
# Thus a quadratic spline is typically constrained to have a continuous first derivative, and a cubic spline is typically constrained to have a continuous first and second derivative. 
# The construction of splines require using 'knots', which are the analogs of the bandwidth in nonparametric regressions.
# The more knots we used, the smoother the fitting curve (the lower the bias), the more likely we are overfitting the data.
# In comparison, the less knots we used, the rougher the fitting curve (the higher the bias), the more likely we are underfitting the data.
# For regression splines in practice, it's hard to choose the number of knots and the location of knots, and a common practice is to specify a grid of values of knots on the support of the explanatory variable x.
# However, a smoothing spline has a knot at each data point, but introduces a penalty for lack of smoothness. 
# If the penalty is zero you get a function that interpolates the data, whereas if the penalty is infinite you get a straight line fitted by ordinary least squares.

mydata <- read.table("http://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
class(mydata)
names(mydata)
str(mydata)
summary(mydata)
head(mydata)
PackageTest <- function(x){
                 if (!require(x,character.only = TRUE)){
                   install.packages(x,dep=TRUE)
                   if(!require(x,character.only = TRUE)) stop("Package not found")
                 }
               }
PackageTest("plyr")
PackageTest("ggplot2")
PackageTest("splines")
PackageTest("gridExtra")

# We use the cohort population dataset as an example to compare different splines' performance vs the least square fit.
# Specifically, we focus on studying regression splines, natural cubic splines and smoothing splines.
# We first do some explorative data analyses to see the plot and correlation. 
# The goal is to use the variable 'age' to predict the variable population. 

dev.new()
plot(mydata$age, mydata$pop)
cor(mydata$age, mydata$pop)


# II. Regression Splines and B-Splines

# We first start with the regression splines (characterized by fixed knots), which requires to select the order of the spline, the number of knots and their placement.
# One simple approach is to parameterize a family of splines by the number of basis functions or degrees of freedom, and have the observations determine the positions of the knots.
# For example, the command bs(x,df=7) in R generates a basis matrix of cubic-spline functions evaluated at the n observations in x, with the 7-3 = 4 interior knots at the appropriate percentilesof x (20, 40, 60 and 80th). 
# For another example, bs(x, degree=1, knots = c(0.2, 0.4, 0.6)) generates a basis for linear splines, with three interior knots, and returns an n-by-4 matrix.
# Since the space of spline functions of a particular order and knot sequence is a vector space, there are many equivalent bases for representing them (just as there are for ordinary polynomials).
# While the truncated power basis is conceptually simple, it is not too attractive numerically, as powers of large numbers can lead to severe rounding problems. 
# The B-spline basis, which is a special and convenient type of regression splines, allows for efficient computations even when the number of knots is large.

knots1 <- seq(25,75, by=25) # less knots
knots2 <- seq(5,95, by=5) # more knots

B.spline <- lm(pop ~ bs(age, knots=knots1, degree=3), data=mydata) # cubic spline
B.spline
cubic.spline1 <- mutate(mydata, smooth=fitted(B.spline)) # combining x,y and yhat

B.spline <- lm(pop ~ bs(age, knots=knots1, degree=2), data=mydata) # quadratic spline
B.spline
quadratic.spline1 <- mutate(mydata, smooth=fitted(B.spline)) # combining x,y and yhat

B.spline <- lm(pop ~ bs(age, knots=knots1, degree=1), data=mydata) # linear spline
B.spline
linear.spline1 <- mutate(mydata, smooth=fitted(B.spline)) # combining x,y and yhat

B.spline <- lm(pop ~ bs(age, knots=knots2, degree=3), data=mydata) # cubic spline
B.spline
names(B.spline)
cubic.spline2 <- mutate(mydata, smooth=fitted(B.spline)) # combining x,y and yhat

B.spline <- lm(pop ~ bs(age, knots=knots2, degree=2), data=mydata) # quadratic spline
B.spline
quadratic.spline2 <- mutate(mydata, smooth=fitted(B.spline)) # combining x,y and yhat

B.spline <- lm(pop ~ bs(age, knots=knots2, degree=1), data=mydata) # linear spline
B.spline
linear.spline2 <- mutate(mydata, smooth=fitted(B.spline)) # combining x,y and yhat

plot1 <- ggplot(cubic.spline1, aes(age, pop)) + geom_point(color='red') + geom_line(aes(age, smooth)) + ggtitle("A Regression Spline (cubic)")
plot2 <- ggplot(quadratic.spline1, aes(age, pop)) + geom_point(color='orange') + geom_line(aes(age, smooth)) + ggtitle("A Regression Spline (quadratic)")
plot3 <- ggplot(linear.spline1, aes(age, pop)) + geom_point(color='yellow') + geom_line(aes(age, smooth)) + ggtitle("A Regression Spline (linear)")
plot4 <- ggplot(cubic.spline2, aes(age, pop)) + geom_point(color='#0000FF') + geom_line(aes(age, smooth)) + ggtitle("A Regression Spline (cubic and more knots)")
plot5 <- ggplot(quadratic.spline2, aes(age, pop)) + geom_point(color='#1E90FF') + geom_line(aes(age, smooth)) + ggtitle("A Regression Spline (quadratic and more knots)")
plot6 <- ggplot(linear.spline2, aes(age, pop)) + geom_point(color='#228B22') + geom_line(aes(age, smooth)) + ggtitle("A Regression Spline (linear and more knots)")
dev.new()
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=2, ncol=3) # putting all pictures together

class(plot1) # "gg" "ggplot"
class(B.spline) # "lm"


# III. Natural Cubic Splines

# Sometimes we have little information at the extremes of the range. 
# Natural cubic splines, which are constrained to be linear outside the range of the data, provide a useful tool in those circumstances.
# Note that requiring linearity outside the range of the data imposes additional smoothness constraints inside the range.

natural.cubic.spline <- lm(pop ~ ns(age, knots=knots1), data=mydata) # natural cubic spline with less knots
natural.cubic.spline
natural.cubic.spline1 <- mutate(mydata, smooth=fitted(natural.cubic.spline)) 

natural.cubic.spline <- lm(pop ~ ns(age, knots=knots2), data=mydata) # natural cubic spline with more knots
natural.cubic.spline
natural.cubic.spline2 <- mutate(mydata, smooth=fitted(natural.cubic.spline)) 

plot7 <- ggplot(natural.cubic.spline1, aes(age, pop)) + geom_point(color='red') + geom_line(aes(age, smooth)) + ggtitle("Natural Cubic Spline with Less Knots")
plot8 <- ggplot(natural.cubic.spline2, aes(age, pop)) + geom_point(color='purple') + geom_line(aes(age, smooth)) + ggtitle("Natural Cubic Spline with More Knots")
dev.new()
grid.arrange(plot7, plot8, nrow=2, ncol=1) # putting all pictures together


# IV. Smoothing Splines

# A smoothing spline has a knot at each data point, but introduces a penalty for lack of smoothness. 
# If the penalty is zero you get a function that interpolates the data, whereas if the penalty is infinite you get a straight line fitted by ordinary least squares.
# Usually a nice compromise can be found somewhere in between, as we usually focus on splines of odd degree, particularly on cubic splines which have some nice properties.

smoothing.spline <- smooth.spline(x=mydata$age, y=mydata$pop)
smoothing.spline
class(smoothing.spline) # "smooth.spline"
names(smoothing.spline)


# References:
# http://people.stat.sfu.ca/~cschwarz/Consulting/Trinity/Phase2/TrinityWorkshop/Workshop-handouts/TW-04-Intro-splines.pdf
# http://www.ssc.wisc.edu/~bhansen/econometrics/EconometricsKindle.pdf
# http://data.princeton.edu/eco572/smoothing2.html
# http://data.princeton.edu/eco572/smoothing.pdf
# http://web.mit.edu/hyperbook/Patrikalakis-Maekawa-Cho/node16.html
# https://www.math.ntnu.no/emner/TMA4215/2008h/cubicsplines.pdf
# http://www.stat.cmu.edu/~cshalizi/402/lectures/11-splines/lecture-11 .pdf
# http://www.uio.no/studier/emner/matnat/ifi/INF-MAT5340/v07/undervisningsmateriale/
# http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf