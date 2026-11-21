library(BradleyTerry2)
data(baseball, package = "BradleyTerry2")
baseballModel1 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
                      data = baseball, id = "team")
summary(baseballModel1)
baseball$home.team <- data.frame(team = baseball$home.team, at.home = 1)
baseball$away.team <- data.frame(team = baseball$away.team, at.home = 0)
baseballModel2 <- update(baseballModel1, formula = ~ team + at.home)
anova(baseballModel1, baseballModel2)
