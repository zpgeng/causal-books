# Motivating example ICP

# Generate data - observational distribution
set.seed(145)
n <- 1000

eps_x1 <- rnorm(n,0,1)
eps_y <- rnorm(n,0,1)
eps_x2 <- rnorm(n,0,.1)

x1 <- eps_x1
y <- x1 + eps_y
x2 <- y + eps_x2

# Model 1: Predict y using its causal parent, x1
model1 <- lm(y~x1)
# (Training) MSE
mean(model1$residuals^2)

# Model 2: Predict y using its causal child, x2
model2 <- lm(y~x2)
# (Training) MSE
mean(model2$residuals^2)

par(mfrow=c(2,2))
plot(x1, y, col = "darkblue", main = "Observational data: y ~ x1")
abline(model1, col = "red")

plot(x2, y, col = "darkred",main = "Observational data: y ~ x2")
abline(model2, col = "red")


# Generate data - interventional distribution
eps_x1 <- rnorm(n,0,1)
eps_y <- rnorm(n,0,1)
eps_x2 <- rnorm(n,0,.1)

# Intervene on x1 and x2
x1_int <- eps_x1 + rnorm(n, -5, 2)
y <- x1_int + eps_y
x2_int <- y + eps_x2 + rnorm(n, -5, 2)

# Use model 1
predictions1 <- predict(model1, newdata = data.frame(x1=x1_int))
# MSE
mean((y-predictions1)^2)

# Use model 2
predictions2 <- predict(model2, newdata = data.frame(x1=x2_int))
# MSE
mean((y-predictions2)^2)

plot(x1_int, y, col = "darkblue", main = "Interventional data: y ~ x1")
abline(model1, col = "red")

plot(x2_int, y, col = "darkred", main = "Interventional data: y ~ x2")
abline(model2, col = "red")
