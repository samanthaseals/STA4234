library(tidyverse)

# Example 1

# The following data are systolic blood pressure (SBP) and age for a sample of 
# 30 individuals. 

sbp <- c(144, 220, 138, 145, 162, 142, 170, 124, 158, 154, 162, 150, 140, 110, 128, 
         130, 135, 114, 116, 124, 136, 142, 120, 120, 160, 158, 144, 130, 125, 175)

age<- c(39, 47, 45, 47, 65, 46, 67, 42, 67, 56, 64, 56, 59, 34, 42, 
        48, 45, 17, 20, 19, 36, 50, 39, 21, 44, 53, 63, 29, 25, 69)

one <- tibble(sbp, age)

# Correlation between age and SBP

cor(one, method="pearson")

# correlation is 0.66

# Is the correlation non-zero?

cor.test(one$age, one$sbp, method="pearson")

# we specify the two variables we want to test the correlation for
# it does not matter which is listed first

# the correlation is significantly different from 0 (p<0.001)

# we also get the CI for rho from the cor.test() function
# the 95% CI for rho is (0.39, 0.82)

# we can change the confidence level with the conf.level option
cor.test(one$age, one$sbp, method="pearson", conf.level=0.99)

# the 99% CI for rho is (0.28, 0.86)