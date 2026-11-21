cakes <- data.frame(
  Cake = factor(rep(c("Eclairs","Napoleons","Custard"), each=3),
                levels=c("Custard","Eclairs","Napoleons")),
  Bacteria = factor(rep(c("Low","Medium","High"), times=3),
                    levels=c("Low","Medium","High")),
  Count = c(92,37,46, 53,15,19, 75,19,12))

m0 <- glm(Count ~ 1, family=poisson, data=cakes)
m1 <- glm(Count ~ Cake, family=poisson, data=cakes)
m2 <- glm(Count ~ Cake + Bacteria, family=poisson, data=cakes)
m3 <- glm(Count ~ Cake * Bacteria, family=poisson, data=cakes)
anova(m0, m1, m2, m3, test="Chisq")
summary(m3)
logmu_tbl <- matrix(NA, nrow=3, ncol=3,
                    dimnames=list(levels(cakes$Cake), levels(cakes$Bacteria)))
for(i in rownames(logmu_tbl)){
  for(j in colnames(logmu_tbl)){
    logmu_tbl[i,j] <- predict(m3,
                              newdata=data.frame(Cake=i,Bacteria=j), type="link")}}
adj_logmu <- logmu_tbl - coef(m3)["(Intercept)"]
adj_logmu
dds_E_H <- (adj_logmu["Eclairs","High"] - adj_logmu["Eclairs","Low"]) -
  (adj_logmu["Custard","High"] - adj_logmu["Custard","Low"])
dds_E_H 
log_odds_H_vs_M <- logmu_tbl[,"High"] - logmu_tbl[,"Medium"]
odds_H_vs_M <- exp(log_odds_H_vs_M)
odds_H_vs_M
X <- as.data.frame(model.matrix(m3))
colnames(X)
m4 <- glm(Count ~ Cake + Bacteria +
            X$"CakeEclairs:BacteriaHigh" +
            X$"CakeNapoleons:BacteriaHigh",
          family = poisson, data = cakes)
anova(m3, m4, test = "Chisq")
newdat <- with(cakes, expand.grid(Cake = levels(Cake),
                                  Bacteria = levels(Bacteria)))
newdat$Observed <- cakes$Count
newdat$logObserved <- log(cakes$Count)
newdat$Fit_m4 <- predict(m4, newdata = newdat, type = "response")
newdat$LogFit_m4 <- predict(m4, newdata = newdat, type = "link")
newdat



