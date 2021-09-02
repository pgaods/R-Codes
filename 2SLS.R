# 2SLS 

PackageTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
PackageTest('sem')
PackageTest('AER')

D <- read.csv("G:/PGZ Documents/Statistics Workshop/PDF textbooks/Machine Learning/The Elements of Statistical Learning/Datasets for Elements of Statistical Learning/mroz.txt", sep="")
class(D) # data.frame
str(D)
D1 <- D[D$LFP==1, ]
colnames(D1)[1] <- "gender" # femaile=1 and we only want females
colnames(D1)[2] <- "hours"
colnames(D1)[5] <- "age"
colnames(D1)[6] <- "education"
colnames(D1)[7] <- "wage"
colnames(D1)[11] <- "husband_edu"
colnames(D1)[15] <- "mom_edu"
colnames(D1)[16] <- "father_edu"
colnames(D1)[19] <- "experience"
D1$log_wage <- log(D1$wage)
D1$experience_sqr <- (D1$experience)^2
D2 <- D1[, c(1,2,5,6,7,11,15,16,19,20,21)]

attach(D2)
model.OLS <- lm(log_wage~experience+experience_sqr+education, data=D2)
summary(model.OLS) # over-identification using three instrumental variables as instruments (mom_edu, father_edu and husband_edu)
model.2SLS <- tsls(formula=log_wage~experience+experience_sqr+education, instruments=~mom_edu+father_edu+husband_edu+experience+experience_sqr, data=D2)
summary(model.2SLS) # 2SLS uses the efficient GMM weighting matrix under conditional homoscedasticity
model.IV <- ivreg(formula=log_wage~experience+experience_sqr+education, instruments=~mom_edu+experience+experience_sqr, data=D2)
summary(model.IV) # just-identification using mom_edu as the instrument
detach(D2)

# Note that education is endogenous and the rest of them are exogeneous.
# In the 'instruments' argument you need to list all exogeneous variables and instruments.

