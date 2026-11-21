library(GLMsData)
library(MASS)
data(turbines)
plot(turbines$Hours, turbines$Fissures/turbines$Turbines,
     xlab = "Hours of use",
     ylab = "Proportion of turbines with fissures",
     pch = 21, # open circle
     bg = "white",
     cex = turbines$Turbines/20,
     xlim = c(0, 5000),
     ylim = c(0, 0.7))
text(turbines$Hours, turbines$Fissures/turbines$Turbines,
     labels = 1:nrow(turbines), pos = 3, cex = 0.8)
tr.logit <- glm( Fissures/Turbines ~ Hours, data=turbines,
                 family=binomial, weights=Turbines)
tr.probit <- update( tr.logit, family=binomial(link="probit") )
tr.cll <- update( tr.logit, family=binomial(link="cloglog") )

tr.array <- rbind( coef(tr.logit), coef(tr.probit), coef(tr.cll))
tr.array <- cbind( tr.array, c(deviance(tr.logit),
                               deviance(tr.probit), deviance(tr.cll)) )
colnames(tr.array) <- c("Intercept", "Hours","Residual dev.")
rownames(tr.array) <- c("Logit","Probit","Comp log-log")
tr.array
newHrs <- seq( 0, 5000, length=100)
newdf <- data.frame(Hours=newHrs)
newP.logit <- predict( tr.logit, newdata=newdf, type="response")
newP.probit <- predict( tr.probit, newdata=newdf, type="response")
newP.cll <- predict( tr.cll, newdata=newdf, type="response")
plot( Fissures/Turbines ~ Hours, data=turbines, pch=19, las=1,
      xlim=c(0, 5000), ylim=c(0, 0.7),
      xlab="Hours run", ylab="Proportion with fissures")
lines( newP.logit ~ newHrs, lty=1, lwd=2)
lines( newP.probit  ~ newHrs, lty=2, lwd=2)
lines( newP.cll ~ newHrs, lty=4, lwd=2)
legend("topleft", lwd=2, lty=c(1, 2, 4),
       legend=c("Logit","Probit","Comp. log-log"))


ED50s <- cbind("Logit" = dose.p(tr.logit),
               "Probit" = dose.p(tr.probit),
               "C-log-log" = dose.p(tr.cll))






