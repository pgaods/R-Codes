# Shrinkage Methods Example


#############################################


# I. Data Loading

# The prostate cancer dataset measures the correlation between the level of a prostate-specific antigen and some covariates. 
# The covariates are:
# 1) lcavol: log-cancer volume
# 2) lweight: log-prostate weight
# 3) age: age of patient
# 4) lbhp: log-amount of benign hyperplasia
# 5) svi: seminal vesicle invasion
# 6) lcp: log-capsular penetration
# 7) gleason: Gleason Score, check http://en.wikipedia.org/wiki/Gleason_Grading_System
# 8) pgg45: percent of Gleason scores 4 or 5
# The response variable is lpsa, the log-psa.
# In addition, there is a flag variable called 'train' that indicates whether the data should be used as a training or a test dataset. 

url <- "http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data"
pcancer <- read.table(url, header=TRUE)
names(pcancer)
class(pcancer)
head(pcancer)
tail(pcancer)

PackageTest <- function(x){
                 if (!require(x,character.only = TRUE)){
                   install.packages(x,dep=TRUE)
                   if(!require(x,character.only = TRUE)) stop("Package not found")
                 }
               }
PackageTest("plyr")
PackageTest("car")
PackageTest("sandwich")


# II. Explorative Data Analyses

str(pcancer)
summary(pcancer)

train <- pcancer[which(pcancer$train),1:9] # getting the training dataset
test <- pcancer[-which(pcancer$train),1:9] # getting the test dataset
piechart <- pie(table(pcancer$train), paste(pcancer$train, table(pcancer$train)), col=c('green', 'yellow'), main='count of records for train vs. test') # use piechart to visualize training vs. test datasets
dev.new()
plot(train) # visualizing the data
cor(train) # getting the correlation matrix

# The which() function gives the TRUE indices of a logical object, allowing for array indices.
# For example, which(letters=="b") gives 2, and which(LETTERS=="D") gives us 4.
# The pie() function creates piecharts.
# The paste() function concatenates vectors after converting to character.
# For example, paste("A", 1:6, sep = "") resolves to "A1" "A2" "A3" "A4" "A5" "A6".
# For another example, paste("Today is", date()) resolves to "Today is Sat Feb 13 19:25:22 2016".

library(plyr)
proc.freq <- function(var, dataset){
             y=count(dataset, var)
             y$percent <- y$freq/sum(y$freq)
             return(y)
             }
proc.freq('train', pcancer)
proc.missing <- function(dataframe) {
                  sapply(dataframe, function(x) sum(is.na(x)))   
                }
proc.missing(train) # no missing values

# III. OLS

# The lm() function gives the standard OLS solution in finite samples under homoscedasticity.
# The default setting for lm() has an intercept term.

model.ls <- lm(lpsa ~ ., data=train) # this adds all the covariates
model.ls
names(model.ls)
formula(model.ls)
anova(model.ls)
summary(model.ls) # doing a summary() function over the result gives hypotheses testing results
BIC(model.ls) # this calculates the BIC value (the smaller the better)
AIC(model.ls) # this calculates the AIC value (the smaller the better)

# We now do a brief visual model diagnostic (and more than likely standard assumptions of OLS are violated in the finite sample):
# Specifically, we plot the residuals itself, and residuals against fitted values.
# We also want to look at outliers and influential statistics. 
# Outliers are observations for which the response y is unusual given the predictor x. 
# In contrast, observations with high leverage high leverage have an unusual value for x.
# Recall that observations whose studentized residuals are greater than 3 in absolute value are possible outliers.

par(mfrow=c(1,2))
plot(model.ls$resid, main="Run chart of residuals")
lines(lowess(model.ls$resid), col="Red") # the lowess() function (different from the loess() function) performs the computations for the LOWESS (locally weighted scatterplot smoothing) smoother which uses the locally-weighted polynomial regression  
grid() # adding grid
plot(model.ls$resid, model.ls$fitted, main="Residuals vs. Fitted Value")
lines(lowess(model.ls$resid, model.ls$fitted), col="Purple")
grid() # adding grid

dev.new()
qqPlot(model.ls, main="QQ Plot") # this function is only in the 'car' library
leveragePlots(model.ls) # leverage plots 
ncvTest(model.ls) # Breusch-Pagan test for heteroscedasticity
vif(model.ls) # testing multicollinearity (VIF>5 means highly multicollinear)
durbinWatsonTest(model.ls) # autocorrelation test (small p-value indicates autocorrelation)

# We now relax the assumption of homoskedasticity.
# The key for heteroskadasticity is the estimated covariance matrix. The algebra of OLS does not change.
# In time series regressions, if the error terms are correlated, then the covariance matrix is non-diagonal and can only be estimated directly on introducing further assumptions.

library(sandwich)
vcovHC(model.ls) # heteroskedasticity consistent (HC) covariance matrix estimation
vcovHAC(model.ls) # heteroskedasticity and autocorrelation consistent (HAC) covariance matrix estimation
NeweyWest(model.ls) # Newey-West HAC estimation, which uses the Bartlett kernel with nonparametric bandwidth selection
kernHAC(model.ls) # Andrew's estimator using a quadratic spectral kernel with parametric bandwidth selection


# IV. Stepwise Algorithm for Variable Selection

# Forwardstepwise selection starts with the intercept, and then sequentially adds into the model the predictor that most improves the fit.
# Forward-stepwise selection is a greedy algorithm.
# Computationally speaking, if the number of variables are bigger than the sample size, forward selection can still be applied. 
# Statistically speaking, a price is paid in variance for selecting the best subset of each size.
# Forward stepwise is a more constrained search, and will have lower variance, but perhaps more bias.
# Backward-stepwise selection starts with the full model, and sequentially deletes the predictor that has the least impact on the fit. 
# The candidate for dropping is the variable with the smallest z-score.
# Backward selection can only be used when the sample size is way bigger than the number of covariates, while forward stepwise can always be used.

model.backward <- step(model.ls) # choose a model by AIC in a stepwise algorithm (default is backward)
model.backward
model.forward <- step(model.ls, direction='forward')
model.forward


# V. Ridge Regression

PackageTest("MASS")
library(MASS)
model.ridge <- lm.ridge(lpsa ~ ., data=train, lambda = 0.5) # the lamda is the tuning parameter 
model.ridge # this gives the matrix of coefficients
model.ridge2 <- lm.ridge(lpsa ~ ., data=train, lambda = seq(0,10,0.5)) # different tuning parameter yields different coefficients
model.ridge2 
plot(model.ridge2)
optimal.lambda <- seq(0,10,0.5)[which.min(model.ridge2$GCV)] # optimal tuning parameter
optimal.lambda


# VI. Lasso

PackageTest("lars") # the lars is the oldest package for lasso
library(lars)
x <- as.matrix(train[,1:8]) # getting the covariates
y <- as.numeric(train[,9]) # getting the response variable
model.lasso <- lars(x, y, type="lasso") # these are all variants of lasso, providing the entire sequence of coefficients and fits, starting from zero, to the OLS fit
model.lasso # the default setting for lasso is that each variable is standardized to unit L2 norm
names(model.lasso)
model.lasso$beta # this gives all the coefficients' solution path
model.lasso$entry # listing step number
model.lasso$lambda # listing all the lambda values for each solution

# There is more recent package called 'glmnet' to do lasso. 
# The function glmnet() in this package fits a generalized linear model via penalized maximum likelihood. 
# The regularization path is computed for the lasso or elastic net penalty at a grid of values for the regularization parameter lambda. 
# The function can deal with all shapes of data, including large sparse data matrices.
# It fits linear, logistic, poisson, multinomial, and Cox regression models. 
# The sequence of models implied by lambda is fit by coordinate descent. 
# For family="gaussian", this is the lasso sequence when alpha=1, otherwise when alpha is not 1, it is the elastic net sequence. 
# For other families, this is a lasso or elastic net regularization path for fitting the generalized linear regression paths by maximizing the appropriate penalized log-likelihood (partial likelihood for the "cox" model).
# Note also that for "gaussian", glmnet standardizes y to have unit variance before computing its lambda sequence (and then unstandardizes the resulting coefficients).
# The coefficients for any predictor variables with zero variance are set to zero for all values of lambda. 
# The latest two features in glmnet are the family="mgaussian" family and the type.multinomial="grouped" option for multinomial fitting. 
# The 'mgaussian' option allows a multi-response gaussian model to be fit, using a "grouped lasso" penalty on the coefficients for each variable.
# The grouped multinomial allows the same penalty for the family="multinomial" model, which is also multi-responsed.

PackageTest("glmnet")
library(glmnet)
trainX <- as.matrix(pcancer[which(pcancer$train),1:8]) # getting the covariates
trainY <- as.matrix(pcancer[which(pcancer$train),9]) # getting the response 
testX <- as.matrix(pcancer[-which(pcancer$train),1:8])
testY <- as.matrix(pcancer[-which(pcancer$train),9])
model.lasso2 <- glmnet(x=trainX, y=trainY, alpha=1, family='gaussian') # the alpha is the elastic net mixing parameter with alpha between 0 and 1 
names(model.lasso2)
model.lasso2
model.lasso2$df # this gives the number of nonzero coefficients for each value of lambda
coef(model.lasso2, s=0.001305)  # s is the lambda value
pred <- predict(model.lasso2, testX, s=0.001305) # s is the lambda value
difftest <-pred-testY
dev.new()
plot(difftest, col='blue', main='closer to 0, the better', pch=15) # pch stands for plotting symbol
abline(h=0, col='gold', ) # h=0 sets the horizontal line

# The ordinary lasso does not address the uncertainty of parameter estimation.
# For inference using the lasso estimator, there are mainly two approaches.
# First, Tibshirani (1996) suggested the bootstrap for the estimation of standard errors and derived an approximate closed form estimate.
# Second, Fan and Li (2001) derived the sandwich formula in the likelihood setting as an estimator for the covariance of the estimates.
# The "Bayesian lasso" of Park and Casella (2008) provides valid standard errors for the regression parameters and provides more stable point estimates by using the posterior median.
# The lasso estimate is equivalent to the mode of the posterior distribution under a normal likelihood and an independent Laplace (double exponential) prior.


# References:
# http://pages.stat.wisc.edu/~gvludwig/fall_2012/handout10.R
# http://www.stat.wisc.edu/~gvludwig/fall_2012/handout15.R
# https://onlinecourses.science.psu.edu/stat857/node/155
# https://onlinecourses.science.psu.edu/stat857/node/156
# http://web.stanford.edu/~hastie/Papers/LARS/LeastAngle_2002.pdf
# http://statweb.stanford.edu/~tibs/ElemStatLearn/
# http://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot

