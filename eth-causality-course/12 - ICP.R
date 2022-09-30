library(InvariantCausalPrediction)
set.seed(1)

# Generate data from two environments
p <- 4
n <- 1000
X <- matrix(rnorm(n*p),ncol=p)
ExpInd <- c(rep(1,n/2), rep(2,n/2))

X[,1] <- X[,1] + (ExpInd>1)* rnorm(n)
X[,2] <- X[,1] + X[,2] + (ExpInd>1)* rnorm(n)
X[,3] <- X[,2] + X[,3]
X[,4] <- -X[,3] + -X[,1] + X[,4]+ (ExpInd>1)* rnorm(n)

YY <- X[,3]
XX <- X[,-3]
XD <- as.data.frame(cbind(XX, YY))
colnames(XD) <- c("X1","X2","X3", "Y")

plot(XD, col=rgb(0.2,0.2,0.2,0.1), pch=20, cex=1.3, main = "Pooled data")
plot(XD[ExpInd==1,],col=rgb(0.2,0.2,0.5,0.1), pch=20, cex=1.3, main = "Env. 1")
plot(XD[ExpInd==2,],col=rgb(0.5,0.2,0.2,0.1), pch=20, cex=1.3, main = "Env. 2")

# Linear regression
linmod <- lm(Y~., data = XD)
summary(linmod)

# Invariant causal prediction
icp <- ICP(XX,YY,ExpInd)
summary(icp)

