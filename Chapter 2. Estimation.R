##### FARAWAY, LINEAR MODELS #####
##### CHAPTER 2, ESTIMATION #####

library(faraway)
library(tidyverse)

# 2.1 LINEAR MODEL (p. 12) #####
# 2.2 MATRIX REPRESENTATION (p. 13) #####
# 2.3 ESTIMATING b (p. 13) #####
# 2.4 LEAST SQUARES ESTIMATION (p. 14) #####
# 2.5 EXAMPLES OF CALCULATING b (p. 16) #####
# 2.6 GAUSS_MARKOV THEOREM (p. 17) #####
# 2.7 GOODNESS OF FIT (p. 20) #####
# 2.8 EXAMPLE (p. 20) #####
data(gala)
dim(gala)
head(gala)
str(gala)

# solving the model using lm
mdl <- lm(Species ~ Area + Elevation + Nearest + Scruz
            + Adjacent, data = gala)
summary(mdl)
str(mdl)

# independent variables
x <- model.matrix( ~ Area + Elevation + Nearest + Scruz
                    + Adjacent, gala)
x

# dependent variable
y <- gala$Species
y

# matrix algebra solution to models (prone to singularity problems)
solve(t(x) %*% x) %*% t(x) %*% y

# alternative solution but still not perfect
solve(crossprod(x, x), crossprod(x, y))

# first level of calls from the model
names(mdl)

mdl$coefficients
mdl$residuals
mdl$effects
mdl$rank
mdl$fitted.values
mdl$assign
mdl$gr
mdl$df.residual
mdl$xlevels
mdl$call
mdl$terms
mdl$model

# second level of calls from the model
mdls <- summary(mdl)
names(mdls)
str(mdls)

mdls$call
mdls$terms
mdls$residuals
mdls$coefficients
mdls$aliased
mdls$sigma
mdls$df
mdls$r.squared
mdls$adj.r.squared
mdls$fstatistic
mdls$cov.unscaled

deviance(mdl) # equal to the residual sum of squares (RSS)

sum(mdl$residuals^2) # calculated deviance

df.residual(mdl) # degrees of freedom

sqrt(deviance(mdl)/df.residual(mdl)) # SD calculated
mdls$sigma # SD extracted directly from model

# calculate the standard error of the coefficients
xtxi <- mdls$cov.unscaled # what is happening here? Just calling the covariance matrix?
xtxi
sqrt(diag(xtxi)) * 60.975 # square root of diagonals * SD
mdls$coef [,2] # SE of coefficients extracted directly from model

solve(t(x) %*% x) # matrix algebra solution for covariance matrix

# calculate R2
1 - deviance(mdl) / sum((y - mean(y))^2) # 1 - RSS / SOS
mdls$r.squared

# 2.9 IDENTIFIABILITY (p. 23) #####

# add a correlated independent variable
gala$Adiff <- gala$Area - gala$Adjacent
head(gala)

g <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent
            + Adiff, gala)
summary(g)

# add some variability to the variable to make it 'close' top identifiable
Adiffe <- gala$Adiff + 0.001 * (runif(30)  -0.5)
head(gala)

g <- lm (Species ~ Area + Elevation + Nearest + Scruz
            + Adjacent + Adiffe, gala)
summary(g)

# 2.10 EXERCISES (p. 25) #####
# ____.1 Exercise 1 ####
data("teengamb")

# exploratory data analysis
dim(teengamb)
head(teengamb)
str(teengamb)
pairs(teengamb)

teengamb %>%
  gather(key = dependents, value = values, -gamble) %>%
  ggplot(aes(y = gamble, x = values)) +
  geom_point(aes(color = values)) +
  geom_smooth(formula = y ~ x, method = lm) +
  facet_wrap(~dependents, ncol = 2, scales = 'free') +
  theme_bw() +
  theme(legend.position = 'none')

teengamb %>%
  group_by(sex) %>%
  ggplot(aes(x = sex, y = gamble)) +
  geom_boxplot(aes(group = sex))

teengamb %>%
  ggplot(aes(x = status, y = gamble)) +
  geom_point(aes(color = sex))

teengamb %>%
  ggplot(aes(x = income, y = gamble)) +
  geom_point(aes(color = sex))

teengamb %>%
  ggplot(aes(x = verbal, y = gamble)) +
  geom_point(aes(color = sex))

# fit a model
mdl_1 <- lm(gamble ~ sex + status + income + verbal, data = teengamb)
summary(mdl_1)
plot(mdl_1)

# a. percent variation explained by these predictors = 53%
summary(mdl_1)$r.squared

# b. largest positive residual, case 24, residual = 94
sort(mdl_1$residuals, decreasing = TRUE)[1]

# c. mean and median of the residuals
mean(mdl_1$residuals)
median(mdl_1$residuals)

# d. correlation of residuals and fitted values
plot(mdl_1$residuals ~ mdl_1$fitted.values)
cor(mdl_1$residuals, mdl_1$fitted.values)

# e. correlation of residuals and income
plot(mdl_1$residuals ~ teengamb$income)
cor(mdl_1$residuals, teengamb$income)

# f. predicted expenditure males vs females
mdl_1$coefficients[1] # males = $ 22.56
mdl_1$coefficients[1] + mdl_1$coefficients[2] # females = $ 0.44



# ____.2 Exercise 2 ####
data("uswages")

dim(uswages)
head(uswages)
str(uswages)

uswages %>%
  select(wage, educ, exper) %>%
  gather(key = dependents, value = values, -wage) %>%
  ggplot(aes(y = wage, x = values)) +
  geom_point(aes(color = values)) +
  geom_smooth(formula = y ~ x, method = lm) +
  facet_wrap(~dependents, ncol = 2, scales = 'free') +
  theme_bw() +
  theme(legend.position = 'none')

mdl_2 <- lm(wage ~ educ + exper, data = uswages)
summary(mdl_2)

mdl_2log <- lm(log(wage) ~ educ + exper, data = uswages)
summary(mdl_2log)
mdl_2log$coefficients





# ____.3 Exercise 3 ####
# ____.4 Exercise 4 ####
# ____.5 Exercise 5 ####