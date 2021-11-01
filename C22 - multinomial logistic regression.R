library(gsheet)
library(tidyverse)
library(nnet)
library(AER)

# pull in data
data <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1T2JvJY2IG9fH8Q26RvLV6MM9ed5f8EbECGRnt0aZQ2M/edit?usp=sharing"))

# set order of the outcome
# we want the "other" category to be the reference group, so it needs to be "first"
data$food <- factor(data$food, levels = c("O", "I", "F"))

# use the multinom() function to construct the multinomial logistic regression
m1 <- multinom(food ~ length, data = data)

# summary, which includes all models constructed
summary(m1)

# look at overall significance (use partial F test)
# saying outcome ~ 1 gives us the intercept-only model
m2 <- multinom(food ~ 1, data = data)
anova(m2, m1)

# find odds ratios
coefficients(m1) # plain model terms
exp(coefficients(m1)) # exponentiate for OR

# get test for significant predictors out using coeftest()
coeftest(m1)

# 95% confidence intervals for model terms
confint(m1)

# 95% confidence intervals for OR
exp(confint(m1))


