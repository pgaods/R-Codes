# Univariate Local Linear and Local Polynomial Regressions Example


#############################################


# I. Data Loading and Exploratory Analyses

# There are three types of univariate nonparametric regressions that we aim to cover in this module.
# They are local constant regressions (Nadaraya-Watson estimator), local linear regressions, and local polynomial regressions.
# Mathematically, the focus is to estimate the CEF m(x) nonparametrically, where we assume y=m(x)+e
# We use the dataset 'faithful' that indicate geyser eruptions and waiting time.

data(faithful)
mydata <- faithful
class(mydata)
names(mydata)
str(mydata)
summary(mydata)

PackageTest <- function(x){
                 if (!require(x,character.only = TRUE)){
                   install.packages(x,dep=TRUE)
                   if(!require(x,character.only = TRUE)) stop("Package not found")
                 }
               }
PackageTest("plyr")
PackageTest("KernSmooth")
proc.freq <- function(var, dataset){
             y=count(dataset, var)
             y$percent <- y$freq/sum(y$freq)
             return(y)
             }
proc.freq('mydata$eruptions', mydata)
proc.freq('mydata$waiting', mydata)


# II. The Nadaraya-Watson Estimator

# The Nadaraya-Watson estimator (NW) essentially uses the rolling weighted mean for estimation, with weights defined by the kernel function.
# It's also known as the local mean estimator as it solves the minization problem of a locally weighted mean (using only the response variable).
# The most important aspect of this estimator is the choice of bandwidth and the choice of kernels.
# The optimal choice under some regularity conditions is the Epanechnikov kernel (it's smooth on a compact set and rapid computation is available).
# The choice of bandwidth is however critical to the performance of the estimator and far more important than the choice of kernel.
# In practice we typically observe that the Nadaraya–Watson has a poorer behavior near the edges of the region where we observe the data and in regions where we lack more data. 
# In R, the ksmooth() function computes the Nadaraya-Watson estimator, with a limited possible kernel choice (box or normal). 
# The syntax for the function is ksmooth(x, y, kernel = c("box", "normal"), bandwidth = 0.5, range.x = range(x), n.points = max(100L, length(x)), x.points).

attach(mydata)
par(mfrow=c(2,2))
plot(eruptions,waiting,main="h=0.0001",xlab="Duration", ylab="Waiting time")
lines(ksmooth(x=eruptions,y=waiting,"normal",0.1),lwd=2,col="red")
plot(eruptions,waiting,main="h=0.1",xlab="Duration", ylab="Waiting time")
lines(ksmooth(x=eruptions,y=waiting,"normal",0.1),lwd=2,col="red")
plot(eruptions,waiting,main="h=0.5",xlab="Duration", ylab="Waiting time")
lines(ksmooth(eruptions,waiting,"normal",0.5),lwd=2,col="red")
plot(eruptions,waiting,main="h=2",xlab="Duration", ylab="Waiting time")
lines(ksmooth(eruptions,waiting,"normal",2),lwd=2,col="red")

# As we can see, large h corresponds to oversmoothing, while small h corresponds to undersmoothing.
# The bandwidth selection depends on whether we are doing univariate or multivariate analysis.
# In univariate regressions, the optimal rate for the bandwidth is h = C*n^(-1/5) with mean-squared convergence O(n^(-2/5)), where C is a well-defined known constant.
# In the multiple regressor case, the optimal bandwidths are h = C*n^(-1/(q+4)) with convergence rate O(n^(-2/(q+4))), where q is the dimension of the covariates.
# In practice, we specify a grid of values of h and pick the best (visualizing can be helpful too).


# III. The Local Linear Regression

# Instead of taking a rolling weighted mean, we can consider a rolling mean regression, with the weights de?ned by the kernel function.
# This is exactly the local linear regression, which is a generalization of the local constant estimator.

library(KernSmooth) 
dev.new()
sequence <- seq(from=0.01, to=range(eruptions)[2], by=(range(eruptions)[2]-0.01)/3)
sequence
par(mfrow=c(2,2))
for(i in sequence){
  fitlocpol<-locpoly(x=eruptions,y=waiting,bandwidth=i) 
  plot(eruptions,waiting,xlab="Duration",ylab="Waiting time",main=paste("h=", i))
  lines(fitlocpol,type="l",lwd=2,col="green")
  }


# IV. The Local Polynomial Regression

# Local polynomial regression extends the ideas above to the context where we can have further polynomial terms in the regression.

library(KernSmooth)
dev.new()
a=2
b=a*a-1
order=3 # local polynomial order
sequence <- seq(from=0.01, to=range(eruptions)[2], by=(range(eruptions)[2]-0.01)/b)
sequence
par(mfrow=c(a,a))
for(i in sequence){
  fitlocpol<-locpoly(x=eruptions,y=waiting,bandwidth=i, degree=order) 
  plot(eruptions,waiting,xlab="Duration",ylab="Waiting time",main=paste("h=", i), sub=paste("Polynomial order=", order))
  lines(fitlocpol,type="l",lwd=2,col="blue")
  }
detach(mydata)


# References:
# http://www.ssc.wisc.edu/~bhansen/econometrics/EconometricsKindle.pdf
# http://stat.epfl.ch/files/content/sites/stat/files/shared/Applied%20Statistics/notes3.pdf
# http://www.ssc.wisc.edu/~bhansen/718/NonParametrics2.pdf
# http://athena.sas.upenn.edu/petra/class721/nonpar3.pdf


