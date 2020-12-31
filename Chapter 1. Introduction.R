##### FARAWAY, LINEAR MODELS #####
##### CHAPTER 1, INTRODUCTION #####

library(faraway)
library(tidyverse)

# 1.1 BEFORE YOU START (p. 1) #####
# 1.2 INITIAL DATA ANALYSIS (p, 2) #####

data(pima) # load data
head(pima) # take a preliminary look

summary(pima) # get a summary, look at min and max; too many 0's
sort(pima$diastolic) # can't have a 0 blood pressure
pima$diastolic[pima$diastolic == 0] <- NA # change 0's to NA
pima$glucose[pima$glucose == 0] <- NA
pima$triceps[pima$triceps == 0] <- NA
pima$insulin[pima$insulin == 0] <- NA
pima$bmi[pima$bmi == 0] <- NA

str(pima) # check the factory type; test should be a factor
pima$test <- as.factor(pima$test)
summary(pima$test)
levels(pima$test) <- c("negative", "positive")

hist(pima$diastolic) # histogram of diastolic pressure
plot(density(pima$diastolic, na.rm = TRUE)) # kernel estimate; missing values removed
plot(sort(pima$diastolic), pch = ".") # sorted diastolic against index
plot(diabetes ~ diastolic, data = pima) # default scatter plot when x = numerical
plot(diabetes ~ test, data = pima) # default boxplot when x = factors
pairs(pima) # scatterplot matrix

# 1.3 WHEN TO USE REGRESSION ANALYSIS (p. 7) #####
# 1.4 HISTORY (p. 7) #####
data(stat500)

stat500 <- data.frame(scale(stat500)) # set all response to mean = 0, sd = 1
plot(final ~ midterm, data = stat500) # plot the data
abline(0, 1) # add a y ~ x line

g <- lm(final ~ midterm, data = stat500) # development model
abline(coef(g), lty = 5) # plot the regression
cor(stat500) # calculate all the correlations (NB: slopes not R2)

# 1.5 EXERCISES (p. 9) #####
# ___.1 Exercise 1 ####
data("teengamb")
head(teengamb)
summary(teengamb)
str(teengamb)

# ___.2 Exercise 2 ####
data("uswages")
head(uswages)

# ___.3 Exercise 3 ####
data(prostate)
head(prostate)

# ___.4 Exercise 4 ####
data(sat)
head(sat)

# ___.5 Exercise 5 ####
data(divusa)
head(divusa)
