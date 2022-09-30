set.seed(1)

### Linear models with Gaussian noise ###

# Model 1
n <- 1000

eps1 <- rnorm(n,0,1)
eps2 <- rnorm(n,0,.6)

x1 <- eps1
x2 <- 0.8*x1 + eps2

par(mfrow=c(1,2))
plot(x1, x2, xlim = c(-4, 4), ylim = c(-4,4), col = "darkblue", 
     main = "True structure: 1 -> 2")
fit1 <- lm(x2~x1)
abline(fit1, col = "red")

# Model 2
eps1 <- rnorm(n,0,.6)
eps2 <- rnorm(n,0,1)

x2 <- eps2
x1 <- 0.8*x2 + eps1

plot(x1, x2, xlim = c(-4, 4), ylim = c(-4,4), col = "darkred", 
     main = "True structure: 2 -> 1")
fit2 <- lm(x2~x1)
abline(fit2, col = "red")


### Linear models with uniform noise ###

# Model 1
n <- 1000

eps1 <- runif(n, -1.732051, 1.732051)
eps2 <- runif(n, -1.03923, 1.03923)

x1 <- eps1
x2 <- 0.8*x1 + eps2

par(mfrow=c(1,2))
plot(x1, x2, xlim = c(-4, 4), ylim = c(-4,4), col = "darkblue", 
     main = "True structure: 1 -> 2")
fit1 <- lm(x2~x1)
abline(fit1, col = "red")

# Model 2
eps1 <- runif(n, -1.03923, 1.03923)
eps2 <- runif(n, -1.732051, 1.732051)

x2 <- eps2
x1 <- 0.8*x2 + eps1

plot(x1, x2, xlim = c(-4, 4), ylim = c(-4,4), col = "darkred", 
     main = "True structure: 2 -> 1")
fit2 <- lm(x2~x1)
abline(fit2, col = "red")
