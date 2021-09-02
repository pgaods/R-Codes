# Logistic Regression for Binary Responses Example


#############################################


# I. Data Loading 

# We use a university admission dataset as an example to study the R code for logistic regression for binary response.
# Here, a researcher is interested in how variables, such as GRE score, GPA, and prestige of the undergraduate institution ('rank'), affects admission into graduate school. 
# In the data, the response variable, admit/reject, is a binary variable (coded as 1 or 0).
# Here, the variable for university prestiage ('rank') takes on the values 1 through 4 (institutions with a rank of 1 have the highest prestige, while those with a rank of 4 have the lowest). 
# To perform a complete logistic regression analysis, we need to load additional libraries 'aod', 'ggplot2' and 'Rcpp' to complete analyses beyond building models.

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
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
PackageTest("Rcpp")

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
plot(mydata$gre, mydata$gpa, col='blue')


# III. The Logit Model 

mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial") # binomial b/c of binary response
summary(mylogit)
Information.criterion <- c(AIC(mylogit), BIC(mylogit))
Information.criterion
head(mylogit$fitted.values)
head(mylogit$linear.predictors)

# The logistic regression coefficients give the change in the log odds of the outcome for a one unit increase in the predictor variable. 
# In this example, for every one unit change in 'gre', the log odds of 'admission' (versus 'non-admission') increases by 0.002.
# Similarly, a one unit increase in 'gpa', the log odds of being admitted to graduate school increases by 0.804.
# The indicator variables for 'rank' (categorical variable) have a slightly different interpretation for logistic regression.
# For example, having attended an undergraduate institution with rank of 2, versus an institution with a rank of 1, changes the log odds of admission by -0.675.
# Meanwhile, the null deviance shows how well the response variable is predicted by a model that includes only the intercept (grand mean).
# The object mylogit$fitted.values gives the predicted probabilities, and the object mylogit$linear.predictors gives the log-odds ratio, which is also the value eta=Xbeta.

# We can use the confint() function to obtain confidence intervals (CIs) for the coefficient estimates.
# Note that for logistic models, confidence intervals are based on the profile(d) log-likelihood function.
# To understand profiled likelihood function, consider a likelihood function L(theta), where theta=(beta, gamma), with beta being the parameter of interest, and gamma is considered a nuisance parameter.
# The profile likelihood for beta in this case is defined as max(L(beta, gamma.hat)) where gamma.hat=argmax(L(beta, gamma)).
# So in a nutshell, for each value of beta, the profile likelihood function is the maximum of the likelihood function over the remaining nuisance parameters.

confint(object=mylogit, level=0.99) # profile likelihood CI, which is more suitable for small sample size
confint.default(mylogit, level=0.95) # default Wald confidence limit, based on asymptotic normality

# Alternativel, one can also exponentiate the coefficients and interpret them as odds-ratios, which is easier to interpret than the log-odds ratio previously.
# Basically, we can use the same logic to get odds ratios and their confidence intervals, by exponentiating the confidence intervals from before.

exp(cbind(odds.ratio = coef(mylogit), confint(mylogit))) # for one unit increase in 'gpa', the odds of being admitted increase by a factor of 2.23
exp(cbind(odds.ratio = coef(mylogit), confint.default(mylogit)))

# To do hypotheses testings, one can use the wald.test() function in the 'aod' library to do joint testing.
# For example, one can use this function to do joint testing on 'rank', and it can be shown in this example that the overall effect of 'rank' is significant.
# More generally, we can test additional hypotheses about the differences or any linear combinations in the coefficients.
# For example, the object wt2 shows we can test that the coefficient for rank=2 is equal to the coefficient for rank=3.

wt1 <- wald.test(b=coef(mylogit), Sigma=vcov(mylogit), Terms=4:6) # the optional argument 'Terms' to be 4:6 refers to the variable rank2-rank4
wt1
R <- cbind(0,0,0,1,-1,0)
wt2 <- wald.test(b=coef(mylogit), Sigma=vcov(mylogit), L=R) # this tests the general form of linear hypothesis Rbeta=0, where R and beta are both column vectors
wt2


# IV. Model Diagnostics

# Traditionally, model diagnostics require methods involving checking residuals, outliers and graphical displays of certain variables.
# From the machine learning perspective, looking at prediction accuracy is also an important aspect of model diagnostics and a further improvement on model building.
# We first go over traditional way of model diagnostics, and then move onto testing the predictability of the model.
# To start with, the most classic check is the plotting of residuals and fitted values, which can easily be achieved through the plot() function.
# However, in the GLM context, the residuals (the object in the glm() function) are the working reiduals, which are essentially in the final iteration of hte IWLS fit.
# The fitted values are the fitted mean values, which is obtained by transforming the linear predictors by the inverse of the link function.
# In the meantime, plotting the Receiver Operating Characteristic Curve (ROC) and plotting the residuals against the index will help too.
# An ROC curve is a standard technique for summarizing classifier performance over a range of trade-offs between true positive (TP) and false positive (FP) error rates.
# An ROC curve is a plot of sensitivity (the ability of the model to predict an event correctly) versus 1-specificity for the possible cut-off classification probability values.
# For logistic regression you can create a 2 × 2 classification table of predicted values from your model for your response if yhat is 0 or 1 versus the true value of y = 0 or 1.
# The position of the ROC on the graph reflects the accuracy of the diagnostic test, as the higher the area under the curve, the better prediction power the model has.

par(mfrow=c(2,2)) # arranging all graphs to be on one picture
plot(mylogit) # The quantile-quantile (qq) plot is a graphical technique for determining if two data sets come from populations with a common distribution
plot(residuals(mylogit,type="pearson"),main="pearson residual plot") # Pearson residuals vs. observations
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
roc.analysis(mylogit) # The area under the curve is c = 0.6928 which indicates good predictive power of the model

# We now try to calculate the predicted probability, and to do so, we test the model both on the on the original data and on a test dataset.
# To do the prediction, we can invoke the predict() function just like the OLS.
# Note that unlike OLS, in the logit models with binary responses, the predict() function allows for either predicted log-odds or predicted probabitlies (determined by the 'type' argument).
# In other words, in the GLM context, the predicted values are not necessarily always the fitted values.
# This is because the default for predict.glm() is to return predictions on the scale of the linear predictor (type=link). 
# To get the fitted values we want to apply the inverse of the link function to those values.
# In a nutshell, the fitted() function gives the same result as the predict() function with the 'type' set to be "response" (probabilities)
# In contrast, predict() with 'type' set to be 'link' only gives us the linear predictor eta=Xbeta (log-odds in this case).

fitted <- fitted(mylogit) # this gives predicted probabilities
prd1 <- predict(mylogit, type="link") # this gives the linear predictor eta=Xbeta
prd2 <- predict(mylogit, type="response") # this is the same as fitted() which gives the predicted probabilities
head(fitted)
head(prd1)
head(prd2)
testdata <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
testdata
predicted.probability <- predict(mylogit, newdata=testdata, type="response") # this gives the predicted probabilities
predicted.logodds <- predict(mylogit, newdata=testdata, type="link") # this gives the linear predictor eta=Xbeta
testdata <- cbind(testdata, predicted.probability, predicted.logodds) 
testdata

# The predicted probability of being accepted into a graduate program is 0.5166 for students from the highest prestige undergraduate institutions (rank=1).
# The predicted log-odds for a student who comes from the highest prestige undergraduate institutions (rank=1) with average gre and gpa of being accepted into a graduate program is 0.06643085. 



# References:
# http://statistics.ats.ucla.edu/stat/r/dae/logit.htm
# https://onlinecourses.science.psu.edu/stat504/node/161
# http://www.math.umt.edu/patterson/ProfileLikelihoodCI.pdf
# http://www.statmethods.net/advstats/glm.html
# https://onlinecourses.science.psu.edu/stat504/node/216
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html
# http://data.princeton.edu/wws509/notes/c3s8.html
# http://www.theanalysisfactor.com/r-glm-model-fit/
# https://onlinecourses.science.psu.edu/stat504/node/163
# http://www.r-bloggers.com/example-9-14-confidence-intervals-for-logistic-regression-models/
# http://stats.stackexchange.com/questions/34076/finding-the-fitted-and-predicted-values-for-a-statistical-model
