aids.data<-read.csv("week 6/aids.csv")
aids.data$qrt<-as.factor(aids.data$qrt)
aidsLog.glm=glm(cases~qrt+date,family=poisson,data=aids.data)
summary(aidsLog.glm)
aidsSqrt.glm=glm(cases~qrt+date,family=poisson(link="sqrt"),data=aids.data)
summary(aidsSqrt.glm)
par(mfrow=c(1,2))
Time<-1:length(aids.data$cases)
plot(Time,aids.data$cases,pch=19,main="log-link",axes=F,xlab="Quarters")
axis(1,at=seq(1,45,1),labels=c(rep(c(1,2,3,4),11),1),las=2,cex.axis=0.7)
axis(2)
points(Time,aidsLog.glm$fitted)
plot(Time,aids.data$cases,pch=19,main="sqrt-link",axes=F,xlab="Quarters")
axis(1,at=seq(1,45,1),labels=c(rep(c(1,2,3,4),11),1),las=2,cex.axis=0.7)
axis(2)
points(Time,aidsSqrt.glm$fitted)
par(mfrow=c(1,1))
summary(aids.glm.sq<-glm(cases~date, family=poisson(link=sqrt), data = aids.data))
1-pchisq(336.71-293.92,3)
plot(fitted(aidsSqrt.glm),
     rstandard(aidsSqrt.glm, type = "deviance"),
     xlab = "Fitted values",
     ylab = "Standardized deviance residuals",
     main = "St Deviance Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)
qqnorm(rstandard(aidsSqrt.glm, type = "deviance"), main = "QQ-plot of standardized deviance residuals")
qqline(rstandard(aidsSqrt.glm, type = "deviance"), col = "red")
plot(cooks.distance(aidsSqrt.glm),
     type = "h",
     main = "Cook’s Distances",
     xlab = "Observation",
     ylab = "Cook’s Distance")
abline(h = 4/length(aidsSqrt.glm$fitted.values), col = "red", lty = 2)
plot(hatvalues(aidsSqrt.glm),
     rstandard(aidsSqrt.glm, type = "deviance"),
     xlab = "Leverage",
     ylab = "Standardized deviance residuals",
     main = "Residuals vs Leverage")
abline(h = c(-2, 0, 2), lty = 2)
abline(v =2* 5/length(aidsSqrt.glm$fitted.values), col = "red", lty = 2)
       
