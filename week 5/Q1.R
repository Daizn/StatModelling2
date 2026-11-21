turtle<-read.table("week 5/turtle.txt", header=T)
plot(turtle$temp,turtle$female/(turtle$male+turtle$female),
     xlab="Temperature", ylab="Proportion of Females")
y = cbind(turtle$female, turtle$male)
fit1 <- glm(y ~ temp, data=turtle, family=binomial)
summary(fit1)
par(mfrow=c(1,2))
plot(fitted(fit1), resid(fit1, type="deviance"),
     xlab="Fitted", ylab="Deviance Residual", main="Linear fit")
plot(fitted(fit1), resid(fit1, type="pearson"),
     xlab="Fitted", ylab="Pearson Residual", main="Linear fit")
par(mfrow=c(1,1))
fit2 <- glm(y ~ poly(temp, 2), data=turtle, family=binomial)
summary(fit2)
anova(fit1, fit2, test="Chisq") 
par(mfrow=c(1,2))
plot(fitted(fit2), resid(fit2, type="deviance"),
     xlab="Fitted", ylab="Deviance Residual", main="Quadratic fit")
plot(fitted(fit2), resid(fit2, type="pearson"),
     xlab="Fitted", ylab="Pearson Residual", main="Quadratic fit")
par(mfrow=c(1,1))
tempstar <- seq(26, 30, 0.01)
newdata <- data.frame(temp = tempstar)
plot(turtle$temp, turtle$female/(turtle$male+turtle$female), pch=16,
     xlab="Temperature", ylab="Proportion", main="Fitted curves")
lines(tempstar, predict(fit1, newdata=newdata, type="response"), lty=1)
lines(tempstar, predict(fit2, newdata=newdata, type="response"), lty=2)
legend("topright", legend=c("Linear","Quadratic","Cubic", "Quartic"),
       lty=c(1,2,3,4), cex = 0.5)
