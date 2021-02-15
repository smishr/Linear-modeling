### FARAWAY, LINEAR MODELS #####
### CHAPTER 4. DIAGNOSTICS #####
library(faraway)
library(tidyverse)
library(ellipse)

# 4.1 CHECKING ERROR ASSUMPTIONS (p. 58) #####
# ___.1 Constant variance (p. 58) ####
data(savings)
head(savings)
g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

# check for non-linearity
plot(fitted(g),
     residuals(g),
     xlab = "Fitted",
     ylab = "Residuals")
abline(h = 0)

# check for non-constant variance
plot(fitted(g),
     abs(residuals(g)),
     xlab = "Fitted",
     ylab = "|Residuals|")

summary(lm(abs(residuals(g)) ~ fitted(g))) # not sure what to look for here?

# example residual plots:
par(mfrow = c(3, 3))
for(i in 1:9) plot(1:50, rnorm(50)) # constant variance
for(i in 1:9) plot(1:50, (1:50) * rnorm(50)) # strong non-constant variance
for(i in 1:9) plot(1:50, sqrt((1:50)) * rnorm(50)) # mild non-constant variance
for(i in 1:9) plot(1:50, cos((1:50) * pi / 25) + rnorm(50)) # non-linearity
par(mfrow = c(1, 1))

# residual vs. predictor plots
plot(savings$pop15,
     residuals(g),
     xlab="Population under 15",
     ylab="Residuals")

plot(savings$pop75,
     residuals(g),
     xlab="Population over 75",
     ylab="Residuals")

# note two groups present in pop15 plot, test for variance between groups(NB:<35 & >35)
var.test(residuals(g)[savings$pop15 > 35],
         residuals(g)[savings$pop15 < 35]) # ratio significantly different from 1

# attempting transformation
data(gala)
gg <- lm(Species ~ Area + Elevation + Scruz + Nearest+ Adjacent, gala)
plot(fitted(gg),
     residuals(gg),
     xlab = "Fitted",
     ylab="Residuals")

# will a square root transformation give a constant variance?
gs <- lm(sqrt(Species) ~ Area + Elevation + Scruz + Nearest + Adjacent, gala)
plot(fitted(gs),
     residuals(gs),
     xlab="Fitted",
     ylab="Residuals",
     main="Square root Response")
# NB: square root transformations often appropriate for count response data

# ___.2 Normality (p. 64) ####
# ___.3 Correlated errors (p. 66) ####
# 4.2 FINDING UNUSUAL OBSERVATIONS (p. 69) #####
# ___.1 Leverage (p. 69) ####
# ___.2 Outliers (p. 71) ####
# ___.3 Influential observations (p. 75) ####
# 4.3 CHECKING THE STRUCTURE OF THE MODEL (p. 78) #####
# 4.4 EXERCISES (p. 80) #####







