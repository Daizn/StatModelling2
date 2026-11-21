library(lme4)
datadeer <- read_excel("week 11/DeerEcervi.xlsx", sheet = 1)
model1 <- glmer(Ecervi ~ factor(Gender) + Length + (1 | Region/Farm),
                data = datadeer, family = binomial())
summary(model1)
