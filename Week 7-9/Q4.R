marginal <- data.frame(
  Smoke = rep(c("Yes","No"), each = 2),
  Outcome = rep(c("Alive","Dead"), 2),
  Freq = c(443, 139, 502, 230))
m_marg <- glm(Freq ~ Smoke * Outcome, family = poisson, data = marginal)
summary(m_marg)
