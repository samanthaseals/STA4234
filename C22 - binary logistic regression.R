library(gsheet)
library(tidyverse)

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

data <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1fCIhZTf4BnE_Xly4zp8Cg_cz4wAYrQN0WPN9vDnqSEE/edit#gid=0"))

data$rank <- as_factor(data$rank)

# try regression using normal distribution
m1 <- lm(admit ~ gre + gpa + rank, data = data)
summary(m1)
almost_sas(m1)

# logistic regression is done with the glm() function
# we specify family = "binomial" to tell R that it's a 0/1 outcome
m1 <- glm(admit ~ gre + gpa + rank, data = data, family = "binomial")

# summary of model - includes tests
summary(m1)

# beta hats
coefficients(m1)

# odds ratios
exp(coefficients(m1))

# 95% confidence intervals for beta
confint(m1)

# 95% confidence intervals for odds ratios
exp(confint(m1))
