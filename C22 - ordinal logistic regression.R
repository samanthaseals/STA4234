library(gsheet)
library(tidyverse)
library(MASS)
library(fastDummies)

# pull in data
data <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1Q8ZS-umaaSHbPA3gGEy-UConGON_1_oNR_O6Wpsf7bE/edit?usp=sharing"))
# that we have summarized data
# the Count column gives us the number of people in each category
# note that this is very different than how we've received data in the past
# (previously we had "raw" data -- one row for each observation)

# turn outcome into a factor variable (with order)
# note that I coded this so that it would be in the order we want it in
# you may need to edit other data further (see multinomial example)
data$Ideology <- as.factor(data$Ideology)

# create dummy variables 
data <- dummy_cols(data, select_columns = c("Party", "Sex"))

# construct model
m1 <- polr(Ideology ~ Party_Republican + Sex_Male, data = data, weights = Count)
# polr() is for ordinal logistic regression
# the weights = option allows us to specify what column contains counts

# get summary of model
summary(m1)

# odds ratios
round(exp(coefficients(m1)), 2)

# CI of OR
round(exp(confint(m1)), 2)
# note that you can just do confint() by itself if you want the CI for beta