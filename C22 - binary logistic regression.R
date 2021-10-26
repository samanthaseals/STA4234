library(gsheet)
library(tidyverse)

data <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1fCIhZTf4BnE_Xly4zp8Cg_cz4wAYrQN0WPN9vDnqSEE/edit#gid=0"))

data$rank <- as_factor(data$rank)

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
