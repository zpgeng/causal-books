require(dHSIC)
library(mgcv)
set.seed(1)

### RESIT: Regression with subsequent independence test
n <- 500

## Linear model with uniform noise
eps_cause <- runif(n, -1.732051, 1.732051)
eps_effect <- runif(n, -1.03923, 1.03923)

C <- eps_cause
E <- 0.8*C + eps_effect

# fit model in forward direction
fit_causal <- lm(E~C)
residuals_causal <- fit_causal$residuals
# test whether residuals are independent of C
dhsic.test(residuals_causal, C)$p.value # 0.9380619

# fit model in backward direction
fit_anticausal <- lm(C~E)
residuals_anticausal <- fit_anticausal$residuals
# test whether residuals are independent of E
dhsic.test(residuals_anticausal, E)$p.value # 0.000999001

# plot model fits
par(mfrow=c(2,2))
plot(C, E,  col = "darkblue", main = "Linear regression: E ~ C")
abline(fit_causal, col = "red")

plot(E, C,  col = "darkred", main = "Linear regression: C ~ E")
abline(fit_anticausal, col = "red")

# plot residuals
plot(C, residuals_causal,  col = "darkblue", 
     main = "Residuals of regression of E on C vs. C",
     ylab = "Residuals of regression of E on C.")

plot(E, residuals_anticausal, col = "darkred",
     main = "Residuals of regression of C on E vs. E",
     ylab = "Residuals of regression of C on E.")


## Nonlinear model with Gaussian noise
set.seed(1)
n <- 500

eps_cause <- rnorm(n)
eps_effect <- rnorm(n)

C <- eps_cause
E <- C^3 + eps_effect

# fit model in forward direction
fit_causal <- gam(E~s(C))
residuals_causal <- fit_causal$residuals
# test whether residuals are independent of C
dhsic.test(residuals_causal, C)$p.value # 0.967033

# fit model in backward direction
fit_anticausal <- gam(C~s(E))
residuals_anticausal <- fit_anticausal$residuals
# test whether residuals are independent of E
dhsic.test(residuals_anticausal, E)$p.value # 0.000999001

# plot model fits
par(mfrow=c(2,2))
plot(C, E,  col = "darkblue", main = "Nonlinear regression: E ~ C", axes=FALSE)
par(new=TRUE)
plot(fit_causal, col = "red", rug=FALSE, xlab = "", ylab = "")

plot(E, C,  col = "darkred", main = "Nonlinear regression: C ~ E", axes=FALSE)
par(new=TRUE)
plot(fit_anticausal, col = "red", rug=FALSE, xlab = "", ylab = "")

# plot residuals
plot(C, residuals_causal,  col = "darkblue", 
     main = "Residuals of regression of E on C vs. C",
     ylab = "Residuals of regression of E on C.")

plot(E, residuals_anticausal, col = "darkred",
     main = "Residuals of regression of C on E vs. E",
     ylab = "Residuals of regression of C on E.")
