#senility example
senil.data <- data.frame("s" = c(rep(1,14), rep(0,40)),
"x" = c(9,13,6,8,10,4,14,8,11,7,9,7,5,14,13,16,10,12,11,14,
15,18,7,16,9,9,11,13,15,13,10,11,6,17,14,19,9,11,14,10,
16,10,16,14,13,13,9,15,10,11,12,4,14,20))


glm.fit1 <- glm(s ~ x, family = binomial(link = "logit"), data = senil.data)

summary(glm.fit1)


plot(senil.data$x,senil.data$s,xlab="WAIS score",ylab="Symptoms, Yes=1, No=0",xlim=c(1,30))
lines(1:30,predict(glm.fit1,data.frame(x=seq(1,30,1)),type = "response"))

#budworms example
ldose <- rep(0:5,2)
numdead <- c(1,4,9,13,18,20,0,2,6,10,12,16)
sex <- c(rep("M",6),rep("F",6))
bud.data <- data.frame(numdead,numalive=20-numdead,ldose,sex)

glm.fit1 <- glm(cbind(numdead,numalive) ~ ldose + sex, family = binomial, data = bud.data)
summary(glm.fit1)


plot(ldose[sex=="M"],numdead[sex=="M"],xlim=c(-3,9),pch=2,
xlab="ldose",ylab="Mortality",lwd=2)
points(ldose[sex=="F"],numdead[sex=="F"],pch=3,lwd=2)
lines(seq(from =-3,to=9,length.out=20),predict(glm.fit1,data.frame(
ldose=seq(from =-3,to=9,length.out=20),sex="M"),type="response")*20,lwd=2)
lines(seq(from =-3,to=9,length.out=20),predict(glm.fit1,data.frame(
ldose=seq(from =-3,to=9,length.out=20),sex="F"),type="response")*20,lty=2,lwd=2)
legend("topleft",c("Males fitted", "Females fitted"),col=c(1,1),
lwd=c(2,2),lty=c(1,2))


# How big is the shift of the fitted curves in the additive model?
predict(glm.fit1,data.frame(ldose=0,sex="M"),type="response")

predict(glm.fit1,data.frame(ldose=0+glm.fit1$coef[3]/glm.fit1$coef[2],sex="F"),type="response")


predict(glm.fit1,data.frame(ldose=4,sex="M"),type="response")

predict(glm.fit1,data.frame(ldose=4+glm.fit1$coef[3]/glm.fit1$coef[2],sex="F"), type="response")

#null model to the additive model
Lambda <- summary(glm.fit1)$null.deviance - summary(glm.fit1)$deviance
1-pchisq(Lambda,2)


#Infant respiratory disease
resp.data <- data.frame("Resp"=c(77,19,47,48,16,31),
"Sex"=c(rep("M",3),rep("F",3)),"Feed"=c("Bottle","Both","Breast"))

glm.int <- glm(cbind(Resp,c(458,147,494,384,127,464)-Resp) ~ Feed*Sex, family = binomial, data = resp.data)

glm.add <- glm(cbind(Resp,c(458,147,494,384,127,464)-Resp) ~ Feed + Sex, family = binomial, data = resp.data)

summary(glm.add)

1-pchisq(glm.add$deviance-glm.int$deviance, df = 2)

drop1(glm.add, test="Chisq")

#CI for odds ratios

#profile likelihood CIs
confint(glm.add)

#asymptotic CIs
confint.default(glm.add)

#asymptotic CIs for odds ratios by hand
exp(summary(glm.add)$coef[2]+c(-1,1)*qnorm(0.975)*sqrt(vcov(glm.add)[2,2]))

exp(summary(glm.add)$coef[3]+c(-1,1)*qnorm(0.975)*sqrt(vcov(glm.add)[3,3]))

exp(summary(glm.add)$coef[4]+c(-1,1)*qnorm(0.975)*sqrt(vcov(glm.add)[4,4]))
