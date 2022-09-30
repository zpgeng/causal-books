set.seed(1)
n <- 10000

# Example 1
eps <- matrix(rnorm(n*5,0,1),nrow=n, ncol=5)
x1 <- eps[,1]
x2 <- eps[,2]
x3 <- x2 + eps[,3]
x4 <- x1 + x2 + eps[,4]
x5 <- x3 + x4 + eps[,5]

# true causal effect of x4 on x5 is 1
coef(lm(x5~x4))[2]          #invalid
coef(lm(x5~x4+x2))[2]       #valid
coef(lm(x5~x4+x1+x2+x3))[2] #valid


# Example 2
eps <- matrix(rnorm(n*10,0,1),nrow=n,ncol=10)
x2 <- eps[,2]
x4 <- eps[,4]
x7 <- eps[,7]
x1 <- x2 + eps[,1]
x3 <- x2 + x4 + eps[,3]
x8 <- x1 + eps[,8]
x6 <- x1 + eps[,6]
x5 <- x4 + x6 + x7 + eps[,5]
x9 <- x6 + eps[,9]
x10 <- x5 + eps[,10]

# true causal effect of x1 on x5 is 1
coef(lm(x5~x1))[2]        #valid
coef(lm(x5~x1+x2))[2]     #valid
coef(lm(x5~x1+x2+x9))[2]  #invalid
coef(lm(x5~x1+x4))[2]     #valid
coef(lm(x5~x1+x4+x7))[2]  #valid

# is it a coincidence that the last 
# one seems most precise? 
# [see exercises]

