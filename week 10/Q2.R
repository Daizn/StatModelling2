residence.data <- data.frame("freq" = c(425, 17, 80, 36, 10, 555, 74, 47, 7,
                                        34, 771, 33, 5, 14, 29, 452), "R16" = c(rep("NE", 4), rep("MW", 4), rep("S", 4),
                                                                                rep("W", 4)), "R2004" = rep(c("NE", "MW", "S", "W"), 4))
residence.data$symm <- as.factor(c("1,1", "1,2", "1,3", "1,4", "1,2", "2,2",
                                   "2,3", "2,4", "1,3", "2,3", "3,3", "3,4", "1,4", "2,4", "3,4", "4,4"))
glm(freq ~ symm, family = poisson, data = residence.data)
Reff <- as.factor(c(0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3))
glm(freq ~ symm + Reff, family = poisson, data = residence.data)
