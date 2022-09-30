# example:
n <- 1000

eps1 <- rnorm(n,0,1)
eps2 <- rnorm(n,0,1)
eps3 <- rnorm(n,0,1)
eps4 <- rnorm(n,0,1)

x2 <- eps2
x1 <- 3*x2 + eps1
x3 <- 2*x2 + 4*x1 + eps3
x4 <- 5*x3 + eps4

# we want to find the total average causal effect of x1 on x4.

# simulate from intervention distribution: 
# change generating mechanism of x1:
# do(x1=0):
x2 <- eps2
x1 <- rep(0,n)
x3 <- 2*x2 + 4*x1 + eps3
x4 <- 5*x3 + eps4
(m0 <- mean(x4))
# do(x1=1)
x2 <- eps2
x1 <- rep(1,n)
x3 <- 2*x2 + 4*x1 + eps3
x4 <- 5*x3 + eps4
(m1 <- mean(x4))
# total average causal effect:
m1-m0

# in linear SEM, one can multiply edges weights along directed paths, 
# and then sum over such paths. 

# path method oracle:
4*5

# path method sample version:
# regress each node on its parents to estimate edge weights
# then apply path method:
x2 <- eps2
x1 <- 3*x2 + eps1
x3 <- 2*x2 + 4*x1 + eps3
x4 <- 5*x3 + eps4
(r1 <- lm(x3~x1+x2)$coef)
(r2 <- lm(x4~x3)$coef)
r1[2]*r2[2]

# Can we obtain the total causal effect via regression?
# What variables should/shouldn't we adjust for?

# covariate adjustment:
# regenerate data:
x2 <- eps2
x1 <- 3*x2 + eps1
x3 <- 2*x2 + 4*x1 + eps3
x4 <- 5*x3 + eps4
# regressions:
lm(x4~x1)$coef       # S = empty set, invalid
lm(x4~x1+x2)$coef    # S = {x2}, valid
lm(x4~x1+x3)$coef    # S = {x3}, invalid
lm(x4~x1+x2+x3)$coef # S = {x2,x3}, invalid

# Later, we will discuss graphical criteria for identification of 
# causal effects via covariate adjustment 
