# Poisson Regression Example


#############################################


# I. Data Loading 

# Poisson regression is used to model count variables.
# Suppose we have a dataset that records the number of awards earned by students at one high school. 
# Predictors of the number of awards earned include the type of program in which the student was enrolled (e.g., vocational, general or academic) and the score on their final exam in math.
# Here, a researcher is interested in how the factors above affects the number of awards earned by students at one high school. 
# In this example, 'num_awards' is the outcome variable and indicates the number of awards earned by students at a high school in a year.
# 'Math' is a continuous predictor variable and represents students’ scores on their math final exam.
# 'Prog' is a categorical predictor variable with three levels indicating the type of program in which the students were enrolled. 
# It is coded as 1 = “General”, 2 = “Academic” and 3 = “Vocational”. 
# As usual, we first load the data, and after massaging the data and EDA, we perform regression analysis on the dataset.

PackageTest <- function(x){
                 if (!require(x,character.only = TRUE)){
                   install.packages(x,dep=TRUE)
                   if(!require(x,character.only = TRUE)) stop("Package not found")
                 }
               }
PackageTest("plyr")
PackageTest("aod")
PackageTest("sandwich")
PackageTest("msm")

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
class(mydata)
names(mydata)

# Since the variable 'prog' is categorical (factor), we need to ensure that R recognizes it as a factor.
# This is fairly important, as recognizing categorical variables will directly affect the estimation result and its interpretation. 

mydata <- within(mydata, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)
})
str(mydata)
summary(mydata)
head(mydata)
tail(mydata)

# Each variable has 200 valid observations and their distributions seem quite reasonable. 
# The unconditional mean and variance of our outcome variable are not extremely different. 
# Our model assumes that these values, conditioned on the predictor variables, will be equal (or at least roughly so).
# This suggests that Poisson regression might be very appropriate given that we may not need to deal with overdispersion. 
# In contrast, negative binomial regression can be used for over-dispersed count data, that is when the conditional variance exceeds the conditional mean. 
# It can be considered as a generalization of Poisson regression since it has the same mean structure as Poisson regression and it has an extra parameter to model the over-dispersion. 
# If the conditional distribution of the outcome variable is over-dispersed, the confidence intervals for Negative binomial regression are likely to be narrower as compared to those from a Poisson regression.
# In this module, we focus on studying Poisson regression.


# II. Explorative Data Analyses

library(plyr)
proc.freq <- function(var, dataset){
             y=count(dataset, var)
             y$percent <- y$freq/sum(y$freq)
             return(y)
             }
proc.freq('num_awards', mydata)
proc.freq('prog', mydata)

# We can use the tapply() function to display the summary statistics by program type (by-group analysis). 
# The table below shows the average numbers of awards by program type and seems to suggest that program type is a good candidate for predicting the number of awards, our outcome variable, because the mean value of the outcome appears to vary by prog. 
# Additionally, the means and variances within each level of prog–the conditional means and variances–are similar. 
# A conditional histogram separated out by program type is plotted to show the distribution.

with(mydata, tapply(num_awards, prog, function(x) {
     sprintf("Mean (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
library(ggplot2)
dev.new()
ggplot(mydata, aes(num_awards, fill = prog)) + geom_histogram(binwidth=0.5, position="dodge")


# III. The Poisson Regression Model and Diagnostics

mymodel <- glm(num_awards ~ prog + math, data = mydata, family = "poisson")
summary(mymodel)
Information.criterion <- c(AIC(mymodel), BIC(mymodel))
Information.criterion
head(mymodel$fitted.values)
head(mymodel$linear.predictors)

# Cameron and Trivedi (2009) recommended using robust standard errors for the parameter estimates to control for mild violation of the distribution assumption that the variance equals the mean. 
# We use R package 'sandwich' below to obtain the robust standard errors and calculated the p-values accordingly. 
# Together with the p-values, we have also calculated the 95% confidence interval using the parameter estimates and their robust standard errors.

cov.m <- vcovHC(mymodel, type="HC0")
std.err <- sqrt(diag(cov.m))
r.est <- cbind(Estimate= coef(mymodel), "Robust SE" = std.err, "Pr(>|z|)" = 2 * pnorm(abs(coef(mymodel)/std.err), lower.tail=FALSE), LL = coef(mymodel) - 1.96 * std.err, UL = coef(mymodel) + 1.96 * std.err)
r.est

# The output begins with echoing the function call, and the information on deviance residuals is displayed next. 
# Deviance residuals are approximately normally distributed if the model is specified correctly.
# In our example, it shows a little bit of skeweness since median is not quite zero.
# The Poisson regression coefficients give the change in the expected log count for a one unit increase in the predictor variable (continuous). 
# The coefficient for math is 0.07, which means that the expected log count for a one-unit increase in math is 0.07. 
# The indicator variable 'progAcademic' compares between prog=“Academic” and prog=“General”, the expected log count for prog=“Academic” increases by about 1.1. 
# The indicator variable 'prog.Vocational' is the expected difference in log count between prog=“Vocational” and the reference group (prog=“General”).
# The object mymodel$fitted.values gives the predicted probabilities, and the object mylogit$linear.predictors gives the value eta=Xbeta in the GLM framework.

# We can use the residual deviance to perform a goodness of fit test for the overall model. 
# The residual deviance is the difference between the deviance of the current model and the maximum deviance of the ideal model where the predicted values are identical to the observed.
# Therefore, if the residual difference is small enough, the goodness of fit test will not be significant, indicating that the model fits the data. 
# We conclude that the model fits reasonably well because the goodness-of-fit chi-squared test is not statistically significant. 
# If the test had been statistically significant, it would indicate that the data do not fit the model well. 
# In that situation, we may try to determine if there are omitted predictor variables, if our linearity assumption holds and/or if there is an issue of over-dispersion.
# We can also use the confint() function to obtain confidence intervals (CIs) for the coefficient estimates.
# The default method of confint() assumes asymptotic normality, and needs suitable coef() and vcov() methods to be available.

with(mymodel, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail=FALSE)))
confint.default(mymodel, level=0.95) # default Wald confidence limit, based on asymptotic normality

# With the multiplicative Poisson model, the exponents of coefficients are equal to the incidence rate ratio (relative risk). 
# These baseline relative risks give values relative to named covariates for the whole population. 
# You can define relative risks for a sub-population by multiplying that sub-population's baseline relative risk with the relative risks due to other covariate groupings.
# For example, we can compute the relative risk of dying from lung cancer if you are a smoker who has lived in a high radon area. 
# Sometimes, we might want to present the regression results as incident rate ratios and their standard errors, together with the confidence interval.
# To compute the standard error for the incident rate ratios, we will use the Delta method.

s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), coef(mymodel), cov.m)
rexp.est <- exp(r.est[, -3]) ## exponentiate old estimates dropping the p values
rexp.est[, "Robust SE"] <- s ## replacing standard errors with estimates for exponentiated coefficients
rexp.est

# The output above indicates that the incident rate for prog=“Academic” is 2.96 times the incident rate for the reference group (prog=“General”). 
# Likewise, the incident rate for prog=“Vocational” is 1.45 times the incident rate for the reference group holding the other variables at constant. 
# The percent change in the incident rate of 'num_awards' is by 7% for every unit increase in math.

# Sometimes, we might want to look at the expected marginal means. 
# For example, what are the expected counts for each program type holding math score at its overall mean? 
# To answer this type of questions, we can make use of the predict() function. 
# First off, we will make a small data set to apply the predict function to it.

s.mean<- data.frame(math = mean(mydata$math), prog = factor(1:3, levels = 1:3, labels = levels(mydata$prog)))
s.mean
predict(mymodel, s.mean, type="response", se.fit=TRUE)

# In the output above, we see that the predicted number of events for level 1 of prog is about 0.21, holding 'math' at its mean. 
# The predicted number of events for level 2 of 'prog' is higher at 0.62, and the predicted number of events for level 3 of prog is about 0.31.

# We can also graph the predicted number of events with the commands below. 
# The graph indicates that the most awards are predicted for those in the academic program (prog=2), especially if the student has a high math score. 
# The lowest number of predicted awards is for those students in the general program (prog=1). 
# The graph overlays the lines of expected values onto the actual points, although a small amount of random noise was added vertically to lessen overplotting.

mydata$phat <- predict(mymodel, type="response") ## calculating and store predicted values
mydata2 <- mydata[with(mydata, order(prog, math)), ] ## ordering by 'program' and then by 'math'
dev.new()
ggplot(mydata2, aes(x = math, y = phat, color = prog)) +
       geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
       geom_line(size = 1) +
       labs(x = "Math Score", y = "Expected number of awards")


# An ROC curve is a standard technique for summarizing classifier performance over a range of trade-offs between true positive (TP) and false positive (FP) error rates.
# An ROC curve is a plot of sensitivity (the ability of the model to predict an event correctly) versus 1-specificity for the possible cut-off classification probability values.
# For logistic regression you can create a 2 × 2 classification table of predicted values from your model for your response if yhat is 0 or 1 versus the true value of y = 0 or 1.
# The position of the ROC on the graph reflects the accuracy of the diagnostic test, as the higher the area under the curve, the better prediction power the model has.

dev.new()
roc.analysis <- function (object, newdata = NULL, newplot = TRUE, ...) {
                  if (is.null(newdata)) {
                  sd <- object$fitted[object$y == 1]
                  sdc <- object$fitted[object$y == 0]
                  }
                  else {
                    sd <- predict(object, newdata, type = "response")[newdata$y==1]
                    sdc <- predict(object, newdata, type = "response")[newdata$y==0]
                  }
                  roc.plot(sd, sdc, newplot, ...)
                }  
                  roc.plot <- function (sd, sdc, newplot = TRUE, ...) {
                    sall <- sort(c(sd, sdc))
                    sens <- 0
                    specc <- 0
                    for (i in length(sall):1) {
                      sens <- c(sens, mean(sd >= sall[i], na.rm = T))
                      specc <- c(specc, mean(sdc >= sall[i], na.rm = T))
                    }
                    if (newplot) {
                      plot(specc, sens, xlim = c(0, 1), ylim = c(0, 1), type = "l", xlab = "1-specificity", ylab = "sensitivity", ...)
                      abline(0, 1)
                    }
                    else lines(specc, sens, ...)
                    npoints <- length(sens)
                    area <- sum(0.5 * (sens[-1] + sens[-npoints]) * (specc[-1] - specc[-npoints]))
                    lift <- (sens - specc)[-1]
                    cutoff <- sall[lift == max(lift)][1]
                    sensopt <- sens[-1][lift == max(lift)][1]
                    specopt <- 1 - specc[-1][lift == max(lift)][1]
                    list(area = area, cutoff = cutoff, sensopt = sensopt, specopt = specopt)
                 }
roc.analysis(mymodel) # The area under the curve is c = 0.6928 which indicates good predictive power of the model


# References:
# https://stats.idre.ucla.edu/r/dae/poisson-regression/
# http://cameron.econ.ucdavis.edu/racd/count.html
# Cameron, A. C. and Trivedi, P. K. 2009. Microeconometrics Using Stata. College Station, TX: Stata Press.