#delete everything
rm(list=ls(all=TRUE))

# Install necessary packages
install.packages("lmtest")
install.packages("olsrr")
install.packages("psych")

library(lmtest)
library(olsrr)
library(psych)

# Read all three graphs
data1 <- read.csv("Downloads/Graph 1 v2.csv")
data2 <- read.csv("Downloads/Graph 2 v2.csv")
data3 <- read.csv("Downloads/no base case.csv")

# First two-way ANOVA table
res.aov1 <- aov(Results ~ Music.Pace, data = data1)
summary(res.aov1)

# Second
res.aov2 <- aov(Results ~ Music.Pace, data = data2)
summary(res.aov2)

# Third - model with all IV
res.aov3 <- aov(Results ~ Music.Pace + Text.Familiarity, data = data3)
summary(res.aov3)

# Create regression models (regression line of best fit) for all graphs
par(mfrow = c(1, 3))

model1 <- lm(Results ~ Music.Pace, data = data1)
plot(data1$Results, main = "Graph 1", xlab = "Base Case", ylab = "Results", pch = 19, col = "blue")
abline(model1, col = "red", lwd = 2)

model2 <- lm(Results ~ Music.Pace, data = data2)
plot(data2$Results, main = "Graph 2", xlab = "Base Case", ylab = "Results", pch = 19, col = "green")
abline(model2, col = "red", lwd = 2)

model3 <- lm(Results ~ Music.Pace + factor(Text.Familiarity), data = data3)
plot(data3$Music.Pace, data3$Results, main = "Graph 3", xlab = "Music Pace", ylab = "Results", pch = 19, col = ifelse(data3$Text.Familiarity == 0, "purple", "orange"))

intercept <- coef(model3)[1]
b1 <- coef(model3)[2]
b2 <- coef(model3)[3]
b3 <- coef(model3)[4]

y_vals_0 <- intercept + b1 * data3$Music.Pace + b2 * mean(data3$Music.Pace)
y_vals_1 <- (intercept + b3) + b1 * data3$Music.Pace + b2 * mean(data3$Music.Pace)

lines(data3$Music.Pace, y_vals_0, col = "purple", lwd = 2)
lines(data3$Music.Pace, y_vals_1, col = "orange", lwd = 2)

legend("topleft", legend = c("TF = 0", "TF = 1"), fill = c("purple", "orange"), title = "Text Familiarity")

par(mfrow = c(1, 1))

# Assumptions tests

# Correlation of pairs of variables
pairs.panels(data3)

# Durbin-Watson test
dwtest(mode3l)

# Residual plots
plot(model3)