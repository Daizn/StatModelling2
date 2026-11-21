students.data <- data.frame("alcohol" = c(rep("Yes",16), rep("No", 16)),
                            "cigarette" = rep(c(rep("Yes", 8), rep("No", 8)), 2),
                            "sex" = rep(c(rep("F", 2), rep("M", 2)), 8),
                            "race" = rep(c(rep("White", 4), rep("Other", 4)), 4),
                            "marijuana" = rep(c("Yes", "No"), 16),
                            "freq" = c(405, 268, 453, 228, 23, 23, 30, 19, 13, 218, 28, 201, 2, 19, 1,
                                       18, 1, 17, 1, 17, 0, 1, 1, 8, 1, 117, 1, 133, 0, 12, 0, 17))
m_full2 <- glm(freq ~ (alcohol + cigarette + sex + race + marijuana)^2, family = poisson, data = students.data)
summary(m_full2)
deviance(m_full2); df.residual(m_full2)
pchisq(deviance(m_full2), df = df.residual(m_full2), lower.tail = FALSE)
drop1(m_full2, test = "Chisq")
drop1(update(m_full2, . ~ . - cigarette:race), test = "Chisq")
drop1(update(m_full2, . ~ . - cigarette:race - sex:race),
      test = "Chisq")
drop1(update(m_full2, . ~ . - cigarette:race - sex:race - cigarette:sex),
      test = "Chisq")
drop1(update(m_full2, . ~ . - cigarette:race - sex:race - cigarette:sex
             - race:marijuana), test = "Chisq")
