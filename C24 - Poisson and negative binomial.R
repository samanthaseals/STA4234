library(gsheet)
library(tidyverse)
library(MASS)

# define function to graph ANOVA assumptions
# written by previous student Reid Ginoza
almost_sas <- function(aov.results){
  aov_residuals <- residuals(aov.results)
  par(mfrow=c(2,2))
  plot(aov.results, which=1)
  hist(aov_residuals)
  plot(aov.results, which=2)
  plot(density(aov_residuals))
}

# pull in data
data <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1qpyN390as_hbJul46yVH7V6Qj-gLiSlhkX4clE5FppU/edit?usp=sharing"))

# try linear regression with normal distribution
m1 <- lm(sat ~ width, data=data)
summary(m1)
almost_sas(m1)

#### POISSON REGRESSION ####

# construct Poisson regression model
m1 <- glm(sat ~ width, family=poisson, data=data)

# summary of model - includes tests
summary(m1)

# beta hats
coefficients(m1)

# incident rate ratios
exp(coefficients(m1))

# 95% confidence intervals for beta
confint(m1)

# 95% confidence intervals for incident rate ratios
exp(confint(m1))

#### NEGATIVE BINOMIAL REGRESSION ####

# check the mean = var assumption
mean(data$sat)
var(data$sat)

# construct negative binomial regression model
m2 <- glm.nb(sat ~ width, data=data)

# summary of model - includes tests
summary(m2)

# beta hats
coefficients(m2)

# incident rate ratios
exp(coefficients(m2))

# 95% confidence intervals for beta
confint(m2)

# 95% confidence intervals for incident rate ratios
exp(confint(m2))
