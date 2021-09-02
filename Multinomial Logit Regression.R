# Multinomial Logic Regression Example


#############################################

sink("blah.txt")
# I. Data Loading and Overview

# When analyzing a polytomous response, it's important to note whether the response is ordinal (consisting of ordered categories) or nominal (consisting of unordered categories). 
# For binary logistic model this question does not arise, but for multinomial response the question certainly matters.
# Some types of models are appropriate only for ordinal responses (e.g. cumulative logits models, adjacent categories models, and coninuation ratios models).
# Other models may be used whether the response is ordinal or nominal (e.g. baseline logit model, and conditional logit model).
# Strictly speaking, if the response is ordinal, we do not necessarily have to take the ordering into account, but very rarely this information is ignored.
# Ordinality in the response is a vital information, and neglecting it almost always will lead to sub-optimal models.
# If the response variable is polytomous and all the potential predictors are discrete as well, we could describe the multi-way contingency table by a loglinear model. 
# However, if you are analyzing a set of categorical variables, and one of them is clearly a "response" while the others are 'predictors', it's recommended that logistic models be used. 
# In particular, fitting a loglinear model in this setting could have two disadvantages.
# Firstly, the loglinear model has many more parameters, and many of them are not of interest. 
# The essential difference is that the loglinear model describes the joint distribution of all the variables, whereas the logit model describes only the conditional distribution of the response given the predictors. 
# Secondly, loglinear models may often be more complicated to interpret, as the effect of a predictor x on the response y is described by the xy association.
# In contrast, multinomial logistic regression is used to model nominal outcome variables, in which the log odds of the outcomes are modeled as a linear combination of the predictor variables, so the parameters are easy to interpret.

# From now on we focus on multinomial logit model, which treat the response variable as nominal. 
# In the current example, we are using a dataset that describes whether high school students make program choices among general program, vocational program and academic program given their test scores and socioeconomic background etc..
# The discrete variables in the dataset we have include: id, female(gender), ses (sociaeconomic status), schtyp (school type), prog(program type, the response variable), honors, and cid(class ID). 
# The continuous variables we have include: test scores, such as read, write, math, science, socst, and awards.
# For simplicity and convenience, we will drop the class ID variable in the model for model parsimony in our example.

PackageTest <- function(x){
                 if (!require(x,character.only = TRUE)){
                   install.packages(x,dep=TRUE)
                   if(!require(x,character.only = TRUE)) stop("Package not found")
                 }
               }
PackageTest('foreign')
PackageTest('nnet')
PackageTest('plyr')
PackageTest('aod')

mydata <- read.dta("http://www.ats.ucla.edu/stat/data/hsbdemo.dta")
head(mydata)
str(mydata)
summary(mydata)
class(mydata)
names(mydata)
names(mydata)[names(mydata) == 'female'] <- 'gender' # renaming variable
mydata$cid <- NULL # drop the class ID variable


# II. Exploratory Data Analyses

# We first do a frequency analysis on the categorical variables and draw a contingency table.
# We then examine the continuous variables' correlation and plot.

with(mydata, table(ses, prog)) # contingency table
library(plyr)
proc.freq <- function(var, dataset){
             y=count(dataset, var)
             y$percent <- y$freq/sum(y$freq)
             return(y)
             }
proc.freq('prog', mydata)
proc.freq('ses', mydata)
proc.freq('honors', mydata)
proc.freq('gender', mydata)
proc.freq('schtyp', mydata)
cor(mydata$write, mydata$read)
plot(mydata$write, mydata$read, col='brown')


# III. The Multinomial Logit Model 

# we use the multinom() function from the 'nnet' package to estimate a multinomial logistic regression model.
# There are other functions in other R packages capable of multinomial regression, but they require data reshaping, which could be complicated in certain scenarios.
# Before modeling, it is important to choose a refernce group for our outcome, as we can choose the level of our outcome that we wish to use as our baseline for the response.
# The baseline choice is generally arbitrary, though in many applied statistics setting, it may be natural to choose one over the other for the purpose of interpretation.
# It is also important to know that the indepent variables include many factors (categorical variables).
# For convenience, we choose their baseline too by using the factor() function, so that we avoid the dummy variable trap (colinearity) by choosing a common baseline for each dummy flag.

mydata$ses <- factor(mydata$ses, level=c("middle","low","high")) # reassigning baseline dummy variables for the independent variable (so only 'low' and 'high' will show up in the regression coefficients later)
mydata$prog2 <- relevel(mydata$prog, ref = "academic") # setting 'academic' as a baseline
mylogit <- multinom(prog2 ~ ses + gender + schtyp + read + write + math + science + socst + honors + awards, data = mydata)
summary(mylogit) # regression coefficients
z <- summary(mylogit)$coefficients/summary(mylogit)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1))*2
pvalue # getting the p-values
beta <- coef(mylogit) # this is a matrix
head(mylogit$fitted.values) # this gives the predicted probabilities given the predictors

# This model-running output includes some iteration history and includes the final negative log-likelihood 154.050319. 
# This value multiplied by two is then seen in the model summary as the residual deviance and it can be used in comparisons of nested models.
# To interpret the parameters, we need to first distinguish whether the explanatory variable is categorical or continous.
# A one-unit increase in the variable 'write' is associated with the decrease in the log odds of being in general program vs. academic program in the amount of 0.05433675.
# A one-unit increase in the variable 'write' is associated with the decrease in the log odds of being in vocational program vs. academic program in the amount of 0.05149545.
# The log odds of being in general program vs. in academic program will decrease by 0.6822449  if moving from ses="middle" to ses="high". 
# The log odds of being in general program vs. in academic program will increase by 0.2810933 if moving from ses="middle" to ses="low". 

# The ratio of the probability of choosing one outcome category over the probability of choosing the baseline category is often called the relative risk (also known as odds as we have just used to described the regression parameters).
# The relative risk is the right-hand side linear equation exponentiated, leading to the fact that the exponentiated regression coefficients are relative risk ratios for a unit change in the predictor variable.
# We can also compute the confidence interval for the parameter estimates using the confint() function 

rr <- exp(coef(mylogit)) # calculating relative risk
rr # this is a matrix

# The relative risk ratio for a one-unit increase in the variable 'math' is 0.9047077 for being in general program vs. academic program. 
# The relative risk ratio switching from middle class to upper class is 0.2941023 for being in vocational program vs. academic program. 

alpha <- 0.95
CI <- confint(mylogit, level=alpha)
CI
class(CI) # array
dimnames(CI)
dim(CI) # 12 2 2
CI.general <- paste("(", as.character(round(CI[1:12], digits=3)), ",", as.character(round(CI[13:24], digits=3)), ")")
CI.vocation <- paste("(", as.character(round(CI[25:36], digits=3)), ",", as.character(round(CI[37:48]), digits=3), ")")
pvalue.general <- round(pvalue[1, ], digits=3)
pvalue.vocation <- round(pvalue[2, ], digits=3)
coef.general=round(beta[1, ], digits=3)
coef.vocation=round(beta[2, ], digits=3)
t1 <- data.frame(coef.general, CI.general, pvalue.general)
t2 <- data.frame(coef.vocation, CI.vocation, pvalue.vocation)
t1$significance <- ifelse(as.numeric(t1$pvalue) <= 1-alpha, yes="***", "")
t2$significance <- ifelse(as.numeric(t2$pvalue) <= 1-alpha, yes="***", "")
t1  
t2

# To do further hypotheses testings, one can use the wald.test() function in the 'aod' library to do joint testing.
# More generally, we can test additional hypotheses about the differences or any linear combinations in the coefficients.
# For example, the object wt2 shows we can test that the coefficient for rank=2 is equal to the coefficient for rank=3.

wt1 <- wald.test(b=c((coef(mylogit)[1,]), coef(mylogit)[2, ]), Sigma=vcov(mylogit), Terms=6:8) # the optional argument 'Terms' to be 6:8 refers to the variables 'read', 'write', and 'math' here
wt1
wt2 <- wald.test(b=(coef(mylogit)[1,]), Sigma=vcov(mylogit)[1:12, 1:12], Terms=6:8) 
wt2
R <- cbind(0,0,0,0,0,0,0,0,0,0,1,-1)
wt3 <- wald.test(b=(coef(mylogit)[1,]), Sigma=vcov(mylogit)[1:12, 1:12], L=R) # this tests the general form of linear hypothesis Rbeta=0, where R and beta are both column vectors
wt3 # when L is given, it must have the same number of columns as the length of b, and the same number of rows as the number of linear combinations of coefficients

# wt1 tests parameters of 'read'='write'='math'=0, and w2 gives the same test.
# wt3 tests write=math, and the the result is to fail to reject the null hypotheses, even though their coefficients seem very far apart (this is b/c both of them have p-value far greater than 0.05).


# IV. Model Diagnostics

# Traditionally, model diagnostics require methods involving checking residuals, outliers and graphical displays of certain variables, but given the dimensionality, some traditional methods may be hard to program.
# From the machine learning perspective, looking at prediction accuracy is also an important aspect of model diagnostics and a further improvement on model building.
# From now on we focus on trying to calculate the predicted probability and class.
# Unlike the case for the binary logit model, the function predict() here for multinomial logit works a little bit different.
# In logit models with binary responses, the predict() function allows for predicted log-odds (response='link') or predicted probabitlies (response='response').
# In multinomial logit models, the predict() function allows for predicted category (response='class') or predicted probabilities (response='probs').
# However, in the multinomial logit context, the fitted() function can be used to calculate predicted probabilities for each of our outcome levels, just like in the binary logit case.
# This is because in the GLM context, the fitted values always refer to the predicted probabilities, whether the response variable is binary or not for logit models.

predicted.prob1 <- fitted(mylogit) # predicted probability for each level given the predictor matrix X
head(predicted.prob1)
predicted.prob2 <- predict(mylogit, newdata=mydata, "probs")
head(predicted.prob2) # this gives the same result as predicted.prob1
predicted.class <- predict(mylogit, newdata=mydata, type="class")
head(predicted.class) # this predicts the classes instead of probabilities


# References:
# http://www.ats.ucla.edu/stat/r/dae/mlogit.htm
# https://onlinecourses.science.psu.edu/stat504/node/172
# http://data.princeton.edu/wws509/r/c6s2.html
# http://www.uni-kiel.de/psychologie/rexrepos/posts/regressionMultinom.html
# http://stats.stackexchange.com/questions/34076/finding-the-fitted-and-predicted-values-for-a-statistical-model
