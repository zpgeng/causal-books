library(pcalg)
help(ges)

#==== First example

n <- 1000

set.seed(145)

eps1 <- rnorm(n,0,1)
eps2 <- rnorm(n,0,2)
eps3 <- rnorm(n,0,.7)
eps4 <- rnorm(n,0,1.5)
eps5 <- rnorm(n,0,1)

x1 <- eps1
x2 <- eps2
x3 <- x1 + x2 + eps3
x4 <- 2*x1 + eps4
x5 <- 3*x3 + eps5

data <- cbind(x1,x2,x3,x4,x5)

## Define the score (BIC)
score <- new("GaussL0penObsScore", data)

# choosing these options for phase and iterate yields the original algorithm
# proposed by Chickering; otherwise, a turning phase is included
ges.fit <- ges(score, phase = c("forward", "backward"), iterate = FALSE, 
               verbose = TRUE)

dev.off()
plot(ges.fit$essgraph)


#======= Example hidden variables I

set.seed(243)

n <- 1000

eps1 <- rnorm(n,0,1)
eps2 <- rnorm(n,0,2)
eps3 <- rnorm(n,0,1)
eps4 <- rnorm(n,0,1.5)
eps5 <- rnorm(n,0,.7)

x1 <- eps1
x5 <- eps5
x3 <- eps3
x2 <- x1+2*x5+eps2
x4 <- 2*x5+x3+eps4

# suppose x5 is unobserved/hidden

data <- cbind(x1,x2,x3,x4)

## Define the score (BIC)
score <- new("GaussL0penObsScore", data)

ges.fit <- ges(score)

dev.off()
plot(ges.fit$essgraph)

# there is no perfect map of the observed distribution!

#=========== Example hiddens II

set.seed(763)

n<-1000

eps1 <- rnorm(n,0,1)
eps2 <- rnorm(n,0,2)
eps3 <- rnorm(n,0,.7)
eps4 <- rnorm(n,0,1.5)
eps5 <- rnorm(n,0,1)

x2 <- eps2
x4 <- eps4
x1 <- x2+eps1
x3 <- x2+x4+eps3
x5 <- x4+eps5

# suppose x2 and x4 are hidden

data <- cbind(x1,x3,x5)

## Define the score (BIC)
score <- new("GaussL0penObsScore", data)

ges.fit <- ges(score)

dev.off()
plot(ges.fit$essgraph)

# there is a perfect map of the observed distribution,
# but it does *not* represent the causal structure

