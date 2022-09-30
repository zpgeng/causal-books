# source("http://bioconductor.org/biocLite.R")
# biocLite("graph")
# biocLite("Rgraphviz")

library(gRain)
library(gRbase)
library(RBGL)
library(pcalg)
library(vcd)

# The code below is mostly based on Chapter 3 in
# Hojsgaard, Edwards, Lauritzen (2012). Graphical Models with R. Springer.

##########################
### A very simple example:
#########################

# define possible outcomes:
(yn <- c("yes","no"))
# define conditional probability tables (cptables):
#   marginal probability of disease:
d <- cptable(~disease, values=c(1,99), levels=yn)
#   conditional probability of positive test given disease:
t.d <- cptable(~test+disease, values=c(99,1,1,99), levels=yn)
#   (note that the test seems quite accurate)

# put cpt's together:
plist <- compileCPT(list(d, t.d))
# build GRAphical Independence Network:
pn <- grain(plist)
plot(pn)
# compile/fit Bayesian network:
pnc <- compile(pn, propagate=TRUE)
pnc$cpt

# one can now get all kinds of conditional/marginal/joint 
#   probabilities out:
querygrain(pnc, nodes=c("test", "disease"), type="joint")
querygrain(pnc, nodes=c("test", "disease"), type="conditional") 
   # this was test|disease
querygrain(pnc, nodes=c("test", "disease"), type="marginal")

# suppose that someone has a positive test:
pnc.ev <- setEvidence(pnc, "test", "yes")
# evidence can be retrieved with the getFinding() function
# probability of observing the evidence is obtained with pEvidence()
getFinding(pnc.ev)
# what is the probability of disease?
querygrain(pnc.ev, nodes="disease") 
# note this corresponds to first row of: 
querygrain(pnc, nodes=c("disease", "test"), type="conditional")
# the 50% might be lower than what you had expected, given the 
#   accurate test. Note though that it is still much higher 
#   than the probability of disease without a positive test:
querygrain(pnc, nodes="disease")

###########################
### A more complex example: 
###########################

### Shortness-of-breath (dyspnoea) may be due to tuberculosis, lung cancer or
### bronchitis, or none of them, or more than one of them. A recent visit to
### Asia increases the chances of tuberculosis, while smoking is known to be a 
### risk factor for both lung cancer and bronchitis. The results of a single 
### chest X-ray do not discriminate between lung cancer and tuberculosis, as
### neither does the presence or absence of dyspnoea.

dev.off()
# create dag directly (without specifying cond. distributions):
g <- list(~asia, ~tub|asia, ~smoke, ~lung|smoke, 
          ~bronc|smoke, ~either|lung:tub, ~xray|either, 
          ~dysp|bronc:either)
chestdag <- dagList(g)
plot(chestdag)

# check d-separation (conditional independence) relationships:
dsep("lung","bronc","smoke", chestdag)
dsep("tub", "lung", "either", chestdag)
dsep("tub", "lung", "xray", chestdag)
dsep("tub", "lung", NULL, chestdag) 
   # warning message here is not important
dsep("asia","smoke", c("either","lung"), chestdag)
dsep("tub", "lung", c("asia","xray","either",
                      "dysp","smoke","bronc"), chestdag)

# look at data simulated from this graph
data(chestSim500, package='gRbase')
?chestSim500
head(chestSim500)
names(chestSim500)

# we saw that "tub" and "lung" are d-separated given 
#   the empty set.
# check marginal independence of "tub" and "lung" in data:
tab <- table(chestSim500)
(tab <- margin.table(tab, c(2,4)))
mosaic(tab, shade=TRUE)

# fit data to DAG
simdagchest <- grain(chestdag, data=chestSim500)
simdagchest <- compile(simdagchest, propagate=T, smooth=.1)

# now we can get out all kinds of conditional/marginal/joint 
#   probability estimates:
querygrain(simdagchest, nodes=c("lung","bronc"),type="marginal")
# check with data:
summary(chestSim500)

# suppose we see someone who has been in asia, 
#   has bad xray and smokes
ev <- setEvidence(simdagchest, c("asia","xray","smoke"),
                  c("yes","yes","yes"))
getFinding(ev)
# what is the probability of lung cancer?
querygrain(ev, nodes="lung") 
# compare to marginal probability of lung cancer:
querygrain(simdagchest, "lung")

