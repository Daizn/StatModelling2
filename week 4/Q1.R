vitc1.data <- data.frame("cold" = c(rep(1,335), rep(0,(411-335)),
                                    rep(1,302), rep(0,407-302)), "treat" = c(rep(0,411), rep(1,407)))
vitc1.glm=glm(cold~treat,family=binomial,data=vitc1.data)
summary(vitc1.glm)
confint.default(vitc1.glm)
vitc2.data <- data.frame("cold" = c(335,302), "nocold" = c(76,105),
                         "treat" = c(0,1))
vitc2.glm=glm(cbind(cold, nocold)~treat,family=binomial,data=vitc2.data)
summary(vitc2.glm)