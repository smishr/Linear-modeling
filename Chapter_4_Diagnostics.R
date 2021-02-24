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

fitted(g) # estimated savings rates for each country
residuals(g) # difference between fitted and actual savings rate for each country

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
# assess residuals for normality using a qq plot
qqnorm(residuals(g), ylab = "Residuals")
qqline(residuals(g))

hist(residuals(g)) # not really a suitable alternative for checking normality

# examples of variation in normality 
par(mfrow = c(3, 3))
for(i in 1:9) qqnorm(rnorm(50)) # generally normal
for(i in 1:9) qqnorm(exp(rnorm(50))) # lognormal/skewed 
for(i in 1:9) qqnorm(rcauchy(50)) # cauchy, long-tailed platykurtic
for(i in 1:9) qqnorm(runif(50)) # uniform, short-tailed leptokurtic
par(mfrow = c(1, 1))

hist(rnorm(50))
hist(exp(rnorm(50)))
hist(rcauchy(50))
hist(runif(50))

# formal test of normality
shapiro.test(residuals(g)) # Null = residuals are normally distributed

# ___.3 Correlated errors (p. 66) ####
# note that this section generally referes to serial data (time series)
data(airquality)
dim(airquality) # 153 observations of 6 variables
dim(na.omit(airquality)) # 111 observations of 6 variables
airquality

pairs(airquality, panel=panel.smooth)

# fit model excluding NA rows
g <- lm(Ozone ~ Solar.R + Wind + Temp, airquality, na.action = na.exclude)
summary(g)

# note that non-constant variance and non-linearity can be observed
plot(fitted(g), residuals(g), xlab="Fitted", ylab="Residuals")

#re-fit the model with log transformation of the solution
gl <- lm(log(Ozone) ~ Solar.R + Wind + Temp, airquality,na.action=na.exclude)

# note that variance and non-linearity problems are resolved
plot(fitted(gl), residuals(gl), xlab="Fitted", ylab="Residuals")

# check the residuals for correlation
plot(residuals(gl), ylab="Residuals")
abline(h=0)

# plot successive residuals
plot(residuals(gl)[-153], residuals(gl)[-1], xlab=expression(hat(epsilon)[i]),
     ylab=expression(hat(epsilon)[i + 1]))

# note how successive residuals were paired
residuals(gl)[-153] # get rid of last residual
residuals(gl)[-1] # get rid of first residual

# regression of successive residuals (without intercept - residuals have a mean of 0)
summary(lm(residuals(gl)[-1] ~ -1 + residuals(gl)[-153]))

# compute the Durbin-Watson statistic (p < 0.05 equals correlation)
# note that no evidence of correlation is observed
library(lmtest)
dwtest(Ozone ~ Solar.R + Wind + Temp, data = na.omit(airquality))

# 4.2 FINDING UNUSUAL OBSERVATIONS (p. 69) #####
# ___.1 Leverage (p. 69) ####
# ___.2 Outliers (p. 71) ####
# ___.3 Influential observations (p. 75) ####
# 4.3 CHECKING THE STRUCTURE OF THE MODEL (p. 78) #####
# 4.4 EXERCISES (p. 80) #####







