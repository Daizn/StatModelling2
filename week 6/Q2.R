Atomic.data <- read.table("week 6/Atomic.txt", TRUE)
head(Atomic.data)
Atomic.data$dose <- as.factor(Atomic.data$dose)
Atomic.glm <- glm(Deaths ~ offset(log(AtRisk)) + tyears*dose,
                  family = poisson, data = Atomic.data)
summary(Atomic.glm)
drop1(Atomic.glm, test="Chisq")
summary(update(Atomic.glm, ~.-tyears:dose, data=Atomic.data))
summary(glm(formula = Deaths ~ tyears + dose + offset(log(AtRisk)),
            family = poisson, data = Atomic.data))
