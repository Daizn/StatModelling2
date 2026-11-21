mental.data <- read.table("week 10/mental.txt", T)
mental.data$y <- as.factor(mental.data$y)
library(MASS)
polr1 <- polr(y ~ x1 + x2, data = mental.data)
summary(polr1)
polr2 <- polr(y ~ x1 * x2, data = mental.data)
summary(polr2)
plot(0:9,rowSums(predict(polr1, newdata=data.frame(x2=rep(1,10), x1=c(0:9)),
                         type="probs")[,3:4]), axes=F,lty=2, type="l",ylim=c(0,1),bty="L",ylab="P(Y > 2)",
     xlab="Life Events Index")
axis(2)
axis(1, at=0:9)
lines(0:9,rowSums(predict(polr1, newdata=data.frame(x2=rep(0,10), x1=c(0:9)),
                          type="probs")[,3:4]), lty=1)
text(2.5, .5, "SES=0")
text(3.5, .33, "SES=1")