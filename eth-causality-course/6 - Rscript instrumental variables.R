set.seed(532)

simulate <- function(n,nsimul){
  res <- matrix(0, nrow=nsimul, ncol=3) 
  
  for (i in 1:nsimul){
     I <- rnorm(n,0,1)
     H <- rnorm(n,0,1)
     X <- alpha*I + gamma*H + rnorm(n,0,1)
     Y <- beta*X + delta*H + rnorm(n,0,1)

     # naive regression gives a biased estimate
     res[i,1] <- coef(lm(Y~X))[2]    #2.467

     # using sample covariances
     res[i,2] <- cov(I,Y)/cov(I,X)   #2.094

     # using two-stage least squares
     # regress X on I:
     Xtilde <- fitted(lm(X~I))
     res[i,3] <- coef(lm(Y~Xtilde))[2]
  }
  return(res)
}

alpha <- 2       # strength of IV
beta <- 2        # goal parameter
delta <- 1.5
gamma <- 2.5

res10 <- simulate(n=10,nsimul=100)
res100 <- simulate(n=100,nsimul=100)
res1000 <- simulate(n=1000,nsimul=100)
res10000 <- simulate(n=10000,nsimul=100)

par(mfrow=c(2,2))

plot(density(res10[,1]),xlim=c(1.5,2.8), main=paste("n=10, alpha=",alpha,sep=""), col="red")
lines(density(res10[,2]))
lines(density(res10[,3]), col="blue")
legend("topleft",c("regr","iv-cov", "iv-2s"),lty=c(1,1),col=c("red","black", "blue"))

plot(density(res100[,1]),xlim=c(1.5,2.8), main="n=100", col="red")
lines(density(res100[,2]))
lines(density(res100[,3]), col="blue")

plot(density(res1000[,1]),xlim=c(1.5,2.8), main="n=1000", col="red")
lines(density(res1000[,2]))
lines(density(res1000[,3]), col="blue")

plot(density(res10000[,1]),xlim=c(1.5,2.8), main="n=10'000", col="red")
lines(density(res10000[,2]))
lines(density(res10000[,3]), col="blue")

# Change value of alpha to change the 
# "strength" of the instrumental variable
# What is the effect on the estimates?
