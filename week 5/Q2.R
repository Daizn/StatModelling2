library(GLMsData)
data(lungcap)
plot(Smoke[Gender=="M"]~FEV[Gender=="M"], data = lungcap, pch = 16,
     xlab = "FEV", ylab = "Smoking (Y=1, N=0)")
points(Smoke[Gender=="F"]~FEV[Gender=="F"], data = lungcap, col = "red")
legend("topleft", c("M", "F"), col = c(1,2), pch = c(16, 1))
glm1 <- glm(Smoke ~ FEV + Gender, data = lungcap, family = "binomial")
summary(glm1)
plot(fitted(glm1), resid(glm1, type="pearson"),
     xlab="Fitted", ylab="Pearson Residual")
library(arm)
binnedplot(x = fitted(glm1),
           y = resid(glm1),
           xlab = "Fitted probability")
glm2 <- glm(Smoke ~ FEV * Gender, data = lungcap, family = "binomial")
summary(glm2)
binnedplot(x = fitted(glm2),
           y = resid(glm2),
           xlab = "Fitted probability")
fev_seq <- seq(min(lungcap$FEV), max(lungcap$FEV), length.out = 100)
pred_data <- expand.grid(FEV = fev_seq, Gender = levels(lungcap$Gender))
pred_data$pred <- predict(glm2,
                          newdata = pred_data, type = "response")
plot(pred_data$FEV[pred_data$Gender=="M"], pred_data$pred[pred_data$Gender=="M"],
     type = "l", ylim = c(0, 1), xlab = "FEV", ylab = "Fitted probability")
lines(pred_data$FEV[pred_data$Gender=="F"],pred_data$pred[pred_data$Gender=="F"],
      col = 2, lty = 2)
points(lungcap$FEV[lungcap$Gender=="M"], lungcap$Smoke[lungcap$Gender=="M"],
       pch = 16)
points(lungcap$FEV[lungcap$Gender=="F"], lungcap$Smoke[lungcap$Gender=="F"],
       col = 2)
