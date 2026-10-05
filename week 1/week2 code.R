#q1
if (FALSE) {
saltBP_data <- read.table('saltBP.txt', header = T)
saltBP_fit <- lm (BP ~ salt, data = saltBP_data)
summary(saltBP_fit)
plot(saltBP_fit$fitted, rstudent(saltBP_fit),
     xlab = 'Fitted', ylab = 'res')
abline(h=0, lwd= 2)
qqnorm(rstudent(saltBP_fit), main = NULL)
qqline(rstudent(saltBP_fit), lwd=2)
}

#q2
if (FALSE){
chw_data <- read.table('heightweight.txt', header = T)
chw_fit <- lm(Weight ~ Height, data = chw_data)
summary(chw_fit)
plot(chw_fit$fitted, rstudent(chw_fit),
     xlab = 'fitted', ylab = 'res')
abline(h=0, lwd = 2)
which_large = (1:nrow(chw_data))[abs(rstudent(chw_fit))>2]
text(chw_fit$fitted[which_large], rstudent(chw_fit)[which_large],
     labels = (1:nrow(chw_data))[which_large])
qqnorm(rstudent(chw_fit), main = NULL)
qqline(rstudent(chw_fit), lwd=2)
plot(chw_fit, 4)
abline(h=8/(30-2*2),lty = 2)
library(MASS)
boxcox(chw_fit, lambda = seq(-30,30, by = 0.01))
chw_bc_fit <- lm(1/Weight^(10) ~ Height, data = chw_data)
summary(chw_bc_fit)
plot(chw_bc_fit$fitted, rstudent(chw_bc_fit),
     xlab = 'fitted',ylab = 'res')
abline(h=0, lwd=2)
which_large <- (1:nrow(chw_data))[abs(rstudent(chw_bc_fit))>2]
text(chw_bc_fit$fitted[which_large], rstudent(chw_bc_fit)[which_large],
     labels = (1:nrow(chw_data))[which_large])
qqnorm(rstudent(chw_bc_fit),main=NULL)
qqline(rstudent(chw_bc_fit),lwd=2)
plot(chw_data$Height, (1/chw_data$Weight^10))
abline(chw_bc_fit)
plot(chw_bc_fit, 4)
abline(h=8/(30-2*2),lty=2)
}

#q3
errors_data <- read.table('errors.txt', header = T)
attach(errors_data)
full <- lm(Error ~ True * as.factor(Path)) 
summary(full)
plot(full$fitted, rstudent(full),
     xlab = 'fitted', ylab = 'res')
abline(h=0, lwd= 2)
qqnorm(rstudent(full), main = NULL)
qqline(rstudent(full), lwd=2)
anova(full)
add <- update(full, .~. - True:as.factor(Path))
summary(add)

# 1. Sort your data by 'True' first to prevent jagged, messy lines
order_idx <- order(True)
True_s   <- True[order_idx]
Error_s  <- Error[order_idx]
Path_s   <- Path[order_idx]

# 2. Plot points using the sorted data
plot(True_s, Error_s,
     xlim = range(True_s) + c(-5, 5),
     ylim = range(Error_s) + c(-5, 5),
     xlab = "True", ylab = "Error", 
     col = c("black", "red")[Path_s], pch = 16)

# 3. Calculate intervals (Suppress the warning using suppressWarnings)
conf_int <- suppressWarnings(predict(add, interval = "confidence"))
pred_int <- suppressWarnings(predict(add, interval = "prediction"))

# 4. Extract and align the intervals to the sorted data
conf_s <- conf_int[order_idx, ]
pred_s <- pred_int[order_idx, ]

# 5. Plot Fitted Lines
matlines(True_s[Path_s == 1], conf_s[Path_s == 1, 1], lty = 1, col = "black")
matlines(True_s[Path_s == 2], conf_s[Path_s == 2, 1], lty = 1, col = "red")

# 6. Plot Confidence Intervals (dashed lines)
matlines(True_s[Path_s == 1], conf_s[Path_s == 1, 2:3], lty = 2, col = "black")
matlines(True_s[Path_s == 2], conf_s[Path_s == 2, 2:3], lty = 2, col = "red")

# 7. Plot Prediction Intervals (dotted lines)
matlines(True_s[Path_s == 1], pred_s[Path_s == 1, 2:3], lty = 3, col = "black")
matlines(True_s[Path_s == 2], pred_s[Path_s == 2, 2:3], lty = 3, col = "red")
