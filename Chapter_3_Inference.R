### FARAWAY, LINEAR MODELS #####
### CHAPTER 3. INFERENCE #####

library(faraway)
library(tidyverse)

# 3.1 HYPOTHESIS TESTS TO COMPARE MODELS (p. 28) #####
# no examples
# 3.2 TESTING EXAMPLES (p. 30) #####
# ___.1 Savings data exploration ####
data(savings)

dim(savings) # 50 observations of 5 variables
head(savings)
str(savings)

# sr = savings rate = aggregate personal savings divided by disposable income
# pop15 = percent population under 15
# pop75 = percent of population over 75
# dpi = disposable income per individual
# ddpi = percent rate of change in dpi

# ___.2 Savings model with all parameters ####
g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
summary(g)

# ___.3 Calculation of F stats and P values ####
TSS <- sum((savings$sr - mean(savings$sr))^2) # total sum of squares
TSS

RSS <- sum(g$residuals^2) # residual sum of squares
RSS

DF_P <- summary(g)$df[1] - 1
DF_P

DF_R <- g$df.residual
DF_R

F_stat <- ((TSS - RSS) / DF_P)/(RSS / DF_R) # calculated with n and p degrees of freedom
F_stat

Pval <- 1 - pf(F_stat, DF_P, DF_R)
Pval

# ___.4 Individual predictors and P values ####
# goal is to show that p values as determined by T and F statistics are same
g2 <- lm(sr ~ pop75 + dpi + ddpi, data = savings)
summary(g2)

RSS2 <- sum(g2$residuals^2) # residual sum of squares
RSS2

F_stat2 <- (RSS2 - RSS)/(RSS / DF_R) # calculated with n and p degrees of freedom
F_stat2

Pval <- 1 - pf(F_stat2, 1, DF_R)
Pval

# ___.5 Comparing nested models with ANOVA ####
# test if the models differ significantly from one another using anova
anova(g2, g)
# significantly different, keep pop15 in the model

g3 <- lm(sr ~ pop15 + dpi, data = savings)
summary(g3)
anova(g3, g)
# significantly different, keep predictors in the model

# ___.6 Testing a subspace ####
gr <- lm(sr ~ I(pop15 + pop75) + dpi + ddpi, data = savings) # use I() to evaluate contents rather than include in formula
summary(gr)
anova(gr, g)
# not significantly different, old and young people do not need to be considered separately

# ___.7 Fixing coefficients ####
gr <- lm(sr ~ pop15 + pop75 + dpi + offset(0.5 * ddpi), savings)
summary(gr)
anova(gr, g)
# not significantly different, can 

# direct computation of anova p value
T_stat <- (summary(g)$coefficients[5,1] - 0.5) / summary(g)$coefficients[5,2]
T_stat

2 * pt(T_stat, DF_R)
# computation of anova f statistic
T_stat^2

# 3.3 PERMUTATION TESTS (p. 36) #####
# 3.4 CONFIDENCE INTERVALS FOR b (p. 38) #####
# 3.5 CONFIDENCE INTERVALS FOR PREDICTIONS (p. 41) #####
# 3.6 DESIGNED EXPERIMENTS (p. 44) #####
# 3.7 OBSERVATIONAL DATA (p. 48) #####
# 3.8 PRACTICAL DIFFICULTIES (p. 53) #####
# no examples...
# 3.9 EXERCISEs (p. 55) #####
