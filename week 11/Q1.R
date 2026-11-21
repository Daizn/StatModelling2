
library(readxl)
datacam<- read_excel("week 11/cameratrap.xlsx",sheet=1)
head(datacam)
library(lme4)
model1 <- glmer(Noseen ~ (1 | Camera),
                data = datacam, family = poisson())
summary(model1)
model2 <- glmer(Noseen ~ factor(Habitat) + (1 | Camera),
                data = datacam, family = poisson())
summary(model2)
model3 <- glm(Noseen ~ factor(Habitat),
              data = datacam, family = poisson())
summary(model3)
ranef(model2)
