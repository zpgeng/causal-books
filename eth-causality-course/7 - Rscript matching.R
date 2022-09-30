###################
# Right Heart Catheterization (RHC) dataset
###################
 
# For more info, see: http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.html

# R code adapted from: Jason A. Roy, University of Pennsylvania

# load packages
library(tableone)
library(Matching)

# read data
load(url("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.sav"))

# treatment variable is "swang1":
# The dataset pertains to day 1 of hospitalization, i.e., the "treatment" 
# variable swang1 is whether or not a patient received a RHC on the first day 
# in which the patient qualified for the study

# outcome variable is "death"
# for simplicity, we will use a subset of the available covariates 
# cat1: primary disease category
# meanbp1: mean blood pressure
ARF <- as.numeric(rhc$cat1=='ARF')
CHF <- as.numeric(rhc$cat1=='CHF')
Cirr <- as.numeric(rhc$cat1=='Cirrhosis')
colcan <- as.numeric(rhc$cat1=='Colon Cancer')
Coma <- as.numeric(rhc$cat1=='Coma')
lungcan <- as.numeric(rhc$cat1=='Lung Cancer')
MOSF <- as.numeric(rhc$cat1=='MOSF w/Malignancy')
sepsis <- as.numeric(rhc$cat1=='MOSF w/Sepsis')
female <- as.numeric(rhc$sex=='Female')
died <- as.numeric(rhc$death=='Yes')
age <- rhc$age
treatment <- as.numeric(rhc$swang1=='RHC')
meanbp1 <- rhc$meanbp1

# new dataset
mydata <- cbind(ARF, CHF, Cirr, colcan, Coma, lungcan, MOSF, sepsis,
                age, female, meanbp1, treatment, died)
mydata <- data.frame(mydata)

# covariates we will use (shorter list than you would use in practice)
# need to justify that this set satisfies the adjustment criterion
zvars <- c("ARF", "CHF", "Cirr", "colcan", "Coma", "lungcan", "MOSF", "sepsis",
           "age", "female", "meanbp1")

# look at a "table 1"
# stratify by treatment since we want to assess balance of covariates 
# between treated and control group
table1 <- CreateTableOne(vars=zvars, strata="treatment", data=mydata, test=FALSE)
## include standardized mean difference (SMD)
print(table1, smd=TRUE)


############################################
# do greedy matching on Mahalanobis distance
############################################
greedymatch <- Match(Tr=treatment, X=mydata[zvars], replace=FALSE)
matched <- mydata[unlist(greedymatch[c("index.treated","index.control")]), ]

# dimensions of orignal data and matched data
dim(mydata)
dim(matched)

# look at first matched pair
tr0 <- matched[matched$treatment==0,]
tr1 <- matched[matched$treatment==1,]
tr0[1,]
tr1[1,]

# get table 1 for matched data with standardized differences
matchedtab1 <- CreateTableOne(vars=zvars, strata ="treatment", 
                              data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

# outcome analysis
y_trt <- matched$died[matched$treatment==1]
y_con <- matched$died[matched$treatment==0]

mean(y_trt)
mean(y_con)
# proportion with died=1 larger in treatment group than in control group

tab <- table(y_trt, y_con)
print(tab)
# pairs of type 'always recover': 303
# pairs of type 'never recover': 973
# pairs of type 'helped': 395
# pairs of type 'hurt': 513

# McNemar test
mcnemar.test(tab)
# null hypothesis: p(helped) = p(hurt)


##########################
# propensity score matching
#########################

# fit a propensity score model with logistic regression
psmodel <- glm(treatment ~ ARF + CHF + Cirr + colcan + Coma + lungcan + MOSF +
               sepsis + age + female + meanbp1, family=binomial(), data=mydata)

# show coefficients etc
summary(psmodel)

# create propensity score
pscore <- psmodel$fitted.values

# check distribution of propensity scores for both treatment groups pre-matching
pscore_tr <- pscore[mydata$treatment==1]
pscore_contr <- pscore[mydata$treatment==0]

par(mfrow=c(1,2))
hist(pscore_tr, xlim=c(0,1), 
     main="Propensity scores of treatment group", 
     col="red", xlab="Propensity scores", cex.main=0.75)
summary(pscore_tr)
hist(pscore_contr, xlim=c(0,1), 
     main="Propensity scores of control group", 
     col="blue", xlab="Propensity scores", cex.main=0.75)
summary(pscore_contr)
# distributions should overlap
# positivity assumption would be violated if some propensity scores are 0 or 1
# this would imply that given certain values of z, treatment assignment would
# be deterministic

# do greedy matching on logit(PS) 
logit <- function(p) {log(p)-log(1-p)}
psmatch <- Match(Tr=mydata$treatment, X=logit(pscore), replace=FALSE)
matched <- mydata[unlist(psmatch[c("index.treated","index.control")]), ]

# look at first matched pair
tr0 <- matched[matched$treatment==0,]
tr1 <- matched[matched$treatment==1,]
tr0[1,]
tr1[1,]
pscores_matched <- t(psmatch$mdata$X)
# logit of propensity score
pscores_matched[1,]

# dimensions of orignal data and matched data
dim(mydata)
dim(matched)

# get standardized differences
matchedtab1 <- CreateTableOne(vars=zvars, strata ="treatment", 
                              data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

# outcome analysis
y_trt <- matched$died[matched$treatment==1]
y_con <- matched$died[matched$treatment==0]

mean(y_trt)
mean(y_con)
# proportion with died=1 larger in treatment group than in control group

tab <- table(y_trt, y_con)
print(tab)
# pairs of type 'always recover': 273
# pairs of type 'never recover': 971
# pairs of type 'helped': 425
# pairs of type 'hurt': 515

# McNemar test
mcnemar.test(tab)



