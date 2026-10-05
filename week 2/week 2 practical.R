y <- c(1, 2, 3)
x <- c(0, 1, 2)
X <- cbind(1, x)
# Initial values (as in notes)
mu <- y
eta <- log(mu)
z <- eta
W <- diag(mu)
# One iteration
beta1 <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% z)
eta1 <- X %*% beta1
mu1 <- exp(eta1)
z1 <- eta1 + (y - mu1) / mu1
W1 <- diag(as.numeric(mu1))
# Second iteration
beta2 <- solve(t(X) %*% W1 %*% X) %*% (t(X) %*% W1 %*% z1)
# Fit with glm for comparison
fit <- glm(y ~ x, family = poisson)
coef(fit); summary(fit)
# Compare beta2 to coef(fit)
