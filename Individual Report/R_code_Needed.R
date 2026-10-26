#data
crime <- data.frame(
  InsurY = c(42, 58, 5, 8, 838, 924, 3556, 4341),
  InsurN = c(11, 14, 1, 2, 166, 252, 608, 685),
  Attempt = factor(c("Yes","Yes","Yes","Yes","No","No","No","No"),
                   levels = c("No","Yes")),
  Victim = factor(c("Yes","Yes","No","No","Yes","Yes","No","No"),
                  levels = c("No","Yes")),
  Sex = factor(c("Male","Female","Male","Female",
                 "Male","Female","Male","Female"),
               levels = c("Female","Male"))
)

crime$n <- crime$InsurY + crime$InsurN
crime$prop <- crime$InsurY / crime$n

sum(crime$n)
sum(crime$InsurY) / sum(crime$n)

#rates
xtabs(n ~ Attempt, data = crime)
xtabs(InsurY ~ Attempt, data = crime) /
  xtabs(n ~ Attempt, data = crime)

xtabs(n ~ Victim, data = crime)
xtabs(InsurY ~ Victim, data = crime) /
  xtabs(n ~ Victim, data = crime)

xtabs(n ~ Sex, data = crime)
xtabs(InsurY ~ Sex, data = crime) /
  xtabs(n ~ Sex, data = crime)

#overlap
AV.tab <- xtabs(n ~ Attempt + Victim, data = crime)
AV.tab
AV.tab / rowSums(AV.tab)

#models
resp <- cbind(crime$InsurY, crime$InsurN)

model_full <- glm(resp ~ Attempt * Victim * Sex,
                  family = binomial, data = crime)

model_2way <- glm(resp ~ Attempt + Victim + Sex +
                    Attempt:Victim + Attempt:Sex + Victim:Sex,
                  family = binomial, data = crime)

model_no_AV <- glm(resp ~ Attempt + Victim + Sex +
                     Attempt:Sex + Victim:Sex,
                   family = binomial, data = crime)

model_no_AV_AS <- glm(resp ~ Attempt + Victim + Sex + Victim:Sex,
                      family = binomial, data = crime)

model_final <- glm(resp ~ Victim * Sex,
                   family = binomial, data = crime)

model_additive <- glm(resp ~ Victim + Sex,
                      family = binomial, data = crime)

model_main <- glm(resp ~ Attempt + Victim + Sex,
                  family = binomial, data = crime)

model_null <- glm(resp ~ 1,
                  family = binomial, data = crime)

#model selection
anova(model_2way, model_full, test = "Chisq")
anova(model_no_AV, model_2way, test = "Chisq")
anova(model_no_AV_AS, model_no_AV, test = "Chisq")
anova(model_final, model_no_AV_AS, test = "Chisq")
anova(model_additive, model_final, test = "Chisq")

#AIC
AIC(model_full, model_2way, model_no_AV,
    model_no_AV_AS, model_final, model_additive,
    model_main, model_null)

#final model vs null model
anova(model_null, model_final, test = "Chisq")

#main effects model fit
deviance(model_main)
df.residual(model_main)
1 - pchisq(deviance(model_main),
           df.residual(model_main))

#final model
summary(model_final)

#goodness of fit
deviance(model_final)
df.residual(model_final)
1 - pchisq(deviance(model_final),
           df.residual(model_final))

X2 <- sum(residuals(model_final, type = "pearson")^2)
X2
1 - pchisq(X2, df.residual(model_final))

phi <- X2 / df.residual(model_final)
phi

#residuals
range(rstandard(model_final))
range(residuals(model_final, type = "pearson"))

#odds ratios and 95% confidence intervals
exp(cbind(OR = coef(model_final),
          confint(model_final)))

#victim effect among men
crime$Sex_male <- relevel(crime$Sex, ref = "Male")

model_men <- glm(resp ~ Victim * Sex_male,
                 family = binomial, data = crime)

exp(cbind(OR = coef(model_men),
          confint(model_men)))

#sex effect among victims
crime$Victim_yes <- relevel(crime$Victim, ref = "Yes")

model_victims <- glm(resp ~ Victim_yes * Sex,
                     family = binomial, data = crime)

exp(cbind(OR = coef(model_victims),
          confint(model_victims)))

#fitted probabilities
newdata <- expand.grid(
  Victim = levels(crime$Victim),
  Sex = levels(crime$Sex)
)

newdata$Fitted <- predict(model_final, newdata = newdata,
                          type = "response")
newdata

#plots
crime$fitted <- fitted(model_final)

par(mfrow = c(1, 2))

plot(crime$fitted, crime$prop, pch = 19,
     xlim = c(0.77, 0.88), ylim = c(0.77, 0.88),
     xlab = "Fitted proportion insured",
     ylab = "Observed proportion insured",
     main = "(a) Observed against fitted")
abline(0, 1, lty = 2)

plot(fitted(model_final), rstandard(model_final), pch = 19,
     ylim = c(-3, 3),
     xlab = "Fitted values",
     ylab = "Standardised deviance residuals",
     main = "(b) Standardised residuals")
abline(h = c(-2, 0, 2), lty = c(2, 1, 2))