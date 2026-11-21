library(MASS)
library(nnet)
data(housing, package="MASS")
house.mult <- multinom(Sat ~ Infl*Type*Cont, weights = Freq, data = housing)
summary(house.mult)
stepAIC(house.mult)
multinom(formula = Sat ~ Infl + Type + Cont, data = housing,
         weights = Freq)

summary(update(house.mult, ~ Infl + Type + Cont))
hnames <- lapply(housing[,-5], levels)
house.final <- update(house.mult, ~ Infl + Type + Cont)
pred <- predict(house.final, expand.grid(hnames[-1]), type="probs")
cbind(expand.grid(hnames[-1]), round(pred,2))
housing$HighSat <- as.numeric(housing$Sat == "High")
logit.mod <- glm(HighSat ~ Infl + Type + Cont,
                 family=binomial, weights=Freq, data=housing)
summary(logit.mod)