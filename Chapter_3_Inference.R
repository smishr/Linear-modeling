### FARAWAY, LINEAR MODELS #####
### CHAPTER 3. INFERENCE #####
library(faraway)
library(tidyverse)
library(ellipse)

# 3.1 HYPOTHESIS TESTS TO COMPARE MODELS (p. 28) #####
# no examples
# 3.2 TESTING EXAMPLES (p. 30) #####
# ___.1 'Savings' data exploration ####
data(savings)

dim(savings) # 50 observations of 5 variables
head(savings)
str(savings)

# sr = savings rate = aggregate personal savings divided by disposable income
# pop15 = percent population under 15
# pop75 = percent of population over 75
# dpi = disposable income per individual
# ddpi = percent rate of change in dpi

# ___.2 'Savings' model for all parameters ####
g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
summary(g)

# ___.3 Calculation of F stats and P values for the model ####
TSS <- sum((savings$sr - mean(savings$sr))^2) 
TSS # total sum of squares

RSS <- sum(g$residuals^2) 
RSS # residual sum of squares

DF_P <- summary(g)$df[1] - 1
DF_P # degrees of freedom on predictors

DF_R <- g$df.residual
DF_R # degrees of freedom on residuals

F_stat <- ((TSS - RSS) / DF_P)/(RSS / DF_R) # calculated with n and p degrees of freedom
F_stat # calculated with P and N degrees of freedom

Pval <- 1 - pf(F_stat, DF_P, DF_R)
Pval # equivalent of looking up a p value using F stat and degrees of freedom

# ___.4 Calculation of F stats and P values for predictors ####
# exercise to show that p values as determined by T and F statistics are same
g2 <- lm(sr ~ pop75 + dpi + ddpi, savings)
summary(g2)

RSS2 <- sum(g2$residuals^2) 
RSS2 # residual sum of squares

F_stat2 <- (RSS2 - RSS) / (RSS / DF_R)
F_stat2 # F stat calculated using 'general F-testing approach'

Pval <- 1 - pf(F_stat2, 1, DF_R)
Pval

# ___.5 Comparing nested models with ANOVA ####
# test if two models differ significantly from one another using anova
anova(g2, g)
# significantly different, keep pop15 in the model

g3 <- lm(sr ~ pop15 + dpi, data = savings)
summary(g3)
anova(g3, g)
# significantly different, keep predictors in the model

# ___.6 Testing a 'subspace' of predictors (NB: compounding predictors) ####
gr <- lm(sr ~ I(pop15 + pop75) + dpi + ddpi, savings) # use I() to evaluate contents rather than include in formula
summary(gr)
anova(gr, g)
# not significantly different, old and young people do not need to be considered separately

# ___.7 Testing fixed coefficients for specific predictors ####
gr <- lm(sr ~ pop15 + pop75 + dpi + offset(0.5 * ddpi), savings)
summary(gr)
anova(gr, g)
# not significantly different, can replace coefficient with fixed value

# direct computation of anova p value
summary(g)$coefficients # for reference of direct calls

T_stat <- (summary(g)$coefficients[5,1] - 0.5) / summary(g)$coefficients[5,2]
T_stat # use estimate and standard error

2 * pt(T_stat, DF_R) # equivalent to looking up p value from the student t distribution

# calculation of anova f statistic
T_stat^2

# 3.3 PERMUTATION TESTS (p. 36) #####
# ___.1 Run a simple 'Savings' model and observed T/F-stats, and p-values ####
# run a simplified model to produce interesting (NB: large) F-stats
g <- lm (sr ~ pop75 + dpi, savings)
summary(g) # note the  p-value

 # extract and view the observed F-stat (NB: derived from theoretical probability distribution)
gs <- summary(g)
gs$fstatistic[1] # equal to 2.6796

# ___.2 Run permutations on the solutions and determine p-value ####
# use sample() to run 4000 different permutations on sr
fstats <- numeric(4000) # create empty vector to store permutations

for (i in 1:4000){
  ge <- lm(sample(sr) ~ pop75 + dpi, savings)
  fstats[i] <- summary(ge)$fstatistic[1]
  }

# determine the p-value
length(fstats[fstats > 2.6796])/4000 # calculate proportion greater than the observer F-statistic

# ___.3 Run permutations on predictors and determine p-value ####
tstats <- numeric(4000)
for (i in 1:4000){
  ge <- lm(sr ~ sample(pop75) + dpi, savings)
  tstats[i] <- summary(ge)$coefficients[2,3] # pull t value for pop75
  }
mean(abs(tstats) > 1.6783)

tstats <- numeric(4000)
for (i in 1:4000){
  ge <- lm(sr ~ pop75 + sample(dpi), savings)
  tstats[i] <- summary(ge)$coefficients[3,3] # pull t value for dpi
}
mean(abs(tstats) > 0.337)

# 3.4 CONFIDENCE INTERVALS FOR b (p. 38) #####
# ___.1 'Savings' model for all parameters ####
g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
summary(g)

# ___.2 95% confidence intervals for pop75 ####
# an example of CI where p > 0.05 (NB: null hypothesis, b = 0)
qt(p = 0.975, df = 45) # upper tail of t distribution, 95 %
c(-1.69 - 2.01 * 1.08, -1.69 + 2.01 * 1.08)

# derived from model estimates and standard errors
summary(g)$coefficients[3,1] - qt(0.975, 45) * summary(g)$coefficients[3,2]
summary(g)$coefficients[3,1] + qt(0.975, 45) * summary(g)$coefficients[3,2]

# ___.3 95% confidence intervals for ddpi ####
# an example of CI where p < 0.05 (NB: null hypothesis, b != 0)
qt(p = 0.975, df = 45) # upper tail of t distribution, 95 %
c(0.41 - 2.01 * 0.196, 0.41 + 2.01 * 0.196) # note the positive CI is 50x the negative CI

# derived from model estimates and standard errors
summary(g)$coefficients[5,1] - qt(0.975, 45) * summary(g)$coefficients[5,2]
summary(g)$coefficients[5,1] + qt(0.975, 45) * summary(g)$coefficients[5,2]

# ___.4 Easy way to obtain all univariate intervals ####
confint(g)

# ___.5 Joint confidence intervals ####

# select predictors (2 = pop15, 3 = pop75, 4 = dpi, 5 = ddpi)
confint(g)
x <- 2
y <- 3

plot(ellipse(g, c(x, y)), type = "l", xlim = c(-4.5, 1), ylim = c(-4.5, 1)) # take linear model input directly, define the x, y axis
points(0, 0) # add origin (nb; null hypothesis)
points(coef(g)[x], coef(g)[y], pch = 18) # add coefficients
abline(v = confint(g)[x,], lty = 2) # add pop15 CI
abline(h = confint(g)[y,], lty = 2) # add pop75 CI

# ___.6 Correlation of predictors and coefficients ####
# correlation of the predictors
cor(savings$pop15, savings$pop75) # -0.91

# correlation of the coefficients
summary(g, corr = TRUE)$corr # 0.76, highly correlated predictors frequently leads to reversed coefficients

# 3.5 CONFIDENCE INTERVALS FOR PREDICTIONS (p. 41) #####
# ___.1 Predict the number of species for a new island with predictors ####
head(gala)

g <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
summary(g)

x0 <- c(1, 0.08, 93, 6.0, 12.0, 0.34) # new predictors
y0 <- sum(x0 * coef(g))
y0 # 33.92 species expected on the new island

# ___.2 Prediction of future mean response ####
# 95% CI for all islands with the preceding predictors

qt(0.975, 24) # 2.064; degrees of freedom = 24

x <- model.matrix(g)
x

xtxi <- solve(t(x) %*% x)
xtxi

bm <- sqrt(x0 %*% xtxi %*% x0) * 2.064 * 60.98 # residual error = 60.98
bm # 32.89

# calculate the interval
c(y0 - bm, y0 + bm)

# ___.3 Prediction of future observations ####
# 95% CI for a single new island with the preceding predictors
bm <- sqrt(1 + x0 %*% xtxi %*% x0) * 2.064 * 60.98 # only differs in including mean
bm # 130.09

# calculate the interval
c(y0 - bm, y0 + bm) # note that negative values exist

# ___.4 Using the predict() function ####
# present predictors in a data frame
x0 <- data.frame(Area = 0.08, Elevation = 93, Nearest = 6.0,
                 Scruz = 12, Adjacent = 0.34)

str(predict(g, x0, se = TRUE))


predict(g, x0, interval = "confidence")
predict(g, x0, interval = "prediction")

# ___.5 Consideration of quantitative extrapolation ####
grid <- seq(0, 100, 1)

# assemble a data frame of predictors where Nearest extends from 0 to 100
p <- predict(g, data.frame(Area = 0.08, Elevation = 93, Nearest = grid,
                           Scruz = 12, Adjacent = 0.34),
             se = T,
             interval="confidence")
p

# use matrix plot to plot grid sequence against fit, lower, and upper CI for all predictions
matplot(grid, p$fit, lty = c(1,2,2), type = 'l', xlab = "Nearest",
        ylab = "Species")

# add a 'rug' of actual 'nearest island' distances from the data
rug(gala$Nearest)

# 3.6 DESIGNED EXPERIMENTS (p. 44) #####
# 3.7 OBSERVATIONAL DATA (p. 48) #####
# 3.8 PRACTICAL DIFFICULTIES (p. 53) #####
# no examples...
# 3.9 EXERCISES (p. 55) #####
