# Probit Regression for Binary Responses Example


#############################################


# I. Data Loading 

# We use a university admission dataset as an example to study the R code for logistic regression for binary response.
# Here, a researcher is interested in how variables, such as GRE score, GPA, and prestige of the undergraduate institution ('rank'), affects admission into graduate school. 
# In the data, the response variable, admit/reject, is a binary (nominal) variable (coded as 1 or 0).
# Here, the variable for university prestiage ('rank') takes on the values 1 through 4 (institutions with a rank of 1 have the highest prestige, while those with a rank of 4 have the lowest). 
# To perform a complete probit regression analysis, we need to load additional libraries 'aod', 'ggplot2' and 'Rcpp' to complete analyses beyond building models.

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv") # note that the slash '/' is always the backward way
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
PackageTest("aod")
PackageTest("ggplot2")

# Since the variable 'rank' is categorical (factor), we need to ensure that R recognizes it as a factor.
# This is fairly important, as recognizing categorical variables will directly affect the estimation result and its interpretation. 

mydata$rank <- factor(mydata$rank) # this variable 'rank' is a factor now
levels(mydata$rank) # 1 2 3 4


# II. Explorative Data Analyses

# We first do a frequency on the categorical variables (both independent and dependent variables).
# We then examine the continuous variables' correlation and plot.

library(plyr)
proc.freq <- function(var, dataset){
             y=count(dataset, var)
             y$percent <- y$freq/sum(y$freq)
             return(y)
             }
proc.freq('admit', mydata)
proc.freq('rank', mydata)
cor(mydata$gre, mydata$gpa)
plot(mydata$gre, mydata$gpa, col='brown')


# III. The Probit Model 

myprobit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial"(link="probit")) # binomial b/c of binary response
summary(myprobit)
Information.criterion <- c(AIC(myprobit), BIC(myprobit))
Information.criterion

# The probit regression coefficients give the change in the z-score or probit index for a one unit change in the predictor.
# Specifically for this example, for a one unit increase in gre, the z-score increases by 0.001376.
# The indicator variables for 'rank' have a slightly different interpretation.
# For example, having attended an undergraduate institution of 'rank' of 2, versus an institution with a 'rank' of 1 (the baseline group), decreases the z-score by 0.415

# We can use the confint() function to obtain confidence intervals (CIs) for the coefficient estimates.
# Note that for probit models, confidence intervals are based on the profile(d) log-likelihood function, which are not necessarily symmetric.
# To understand profiled likelihood function, consider a likelihood function L(theta), where theta=(beta, gamma), with beta being the parameter of interest, and gamma is considered a nuisance parameter.
# The profile likelihood for beta in this case is defined as max(L(beta, gamma.hat)) where gamma.hat=argmax(L(beta, gamma)).
# So in a nutshell, for each value of beta, the profile likelihood function is the maximum of the likelihood function over the remaining nuisance parameters.

confint(object=myprobit, level=0.99) # profile likelihood CI
confint.default(myprobit, level=0.95) # default CI

# To do hypotheses testings, one can use the wald.test() function in the 'aod' library to do joint testing.
# For example, one can use this function to do joint testing on 'rank', and it can be shown in this example that the overall effect of 'rank' is significant.
# Specifically, the chi-squared test statistic of 21.4 with three degrees of freedom is associated with a p-value of less than 0.001 indicating that the overall effect of rank is statistically significant.
# More generally, we can test additional hypotheses about the differences in the coefficients (more generally, linear hypotheses).
# For example, the object wt2 shows we can test that the coefficient for rank=2 is equal to the coefficient for rank=3, and the result is significant. 

wt1 <- wald.test(b=coef(myprobit), Sigma=vcov(myprobit), Terms=4:6) # the optional argument 'Terms' to be 4:6 refers to the variable rank2-rank4
wt1
Restricted <- cbind(0,0,0,1,-1,0)
wt2 <- wald.test(b=coef(myprobit), Sigma=vcov(myprobit), L=Restricted) # this tests the general form of linear hypothesis Rbeta=0, where R and beta are both column vectors
wt2


# IV. Model Diagnostics

# Traditionally, model diagnostics require methods involving checking residuals, outliers and graphical displays of certain variables.
# From the machine learning perspective, looking at prediction accuracy is also an important aspect of model diagnostics and a further improvement on model building.
# We first go over traditional way of model diagnostics, and then move onto testing the predictability of the model.
# To start with, the most classic check is the plotting of residuals and fitted values, which can easily be achieved through the plot() function.
# However, in the GLM context, the residuals (the object in the glm() function) are the working reiduals, which are essentially in the final iteration of hte IWLS fit.
# The fitted values are the fitted mean values, which is obtained by transforming the linear predictors by the inverse of the link function.

par(mfrow=c(2,2)) # arranging all graphs to be on one picture
plot(myprobit) # The quantile-quantile (qq) plot is a graphical technique for determining if two data sets come from populations with a common distribution
plot(residuals(myprobit,type="pearson"),main="pearson residual plot") # Pearson residuals vs. observations

# We now try to calculate the predicted probability, and to do so, we first generate a test dataset.
# To do the prediction, we can invoke the predict() function just like the OLS cases on the test data, with the argument 'type' set to be "response".

testdata <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
testdata
testdata$predicted <- predict(myprobit, newdata=testdata, type="response")
testdata # the predicted probability of being accepted into a graduate program is 0.5166 for students from the highest prestige undergraduate institutions (rank=1)


# References:
# http://www.ats.ucla.edu/stat/r/dae/probit.htm
# http://data.princeton.edu/wws509/notes/c3s8.html
