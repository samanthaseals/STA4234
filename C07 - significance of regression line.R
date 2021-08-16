library(tidyverse)

# The following data are systolic blood pressure (SBP) and age for a sample of 
# 30 individuals.

sbp <- c(144, 220, 138, 145, 162, 142, 170, 124, 158, 154, 162, 150, 140, 110, 128, 
         130, 135, 114, 116, 124, 136, 142, 120, 120, 160, 158, 144, 130, 125, 175)

age<- c(39, 47, 45, 47, 65, 46, 67, 42, 67, 56, 64, 56, 59, 34, 42, 
        48, 45, 17, 20, 19, 36, 50, 39, 21, 44, 53, 63, 29, 25, 69)

one <- tibble(sbp, age)

# construct model
m1 <- lm(sbp ~ age, data=one)

# is the regression line significant?

# with simple linear regression, we can run this through the anova() function
anova(m1)

# but when we get to multiple regression, we will need to use the F test from
# the summary() function
summary(m1)

# the regression line is significant (p<0.001)

# again, note that anova() and summary() give the same results with simple 
# linear regression; when we move on to multiple regression, we will only use
# the summary() function for this