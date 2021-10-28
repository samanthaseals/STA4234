library(tidyverse)
library(car)
library(lindia)
library(gsheet)

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

## HuMoSim Example

one <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1LRFlQuBcLrP0N2dxpBiUvEeqendWFhV0tz7o_OSDOOw/edit#gid=0"))

# create ID variable
one <- mutate(one, obs = row_number())

# Let's look at the correlation between the variables. Are there any correlations 
# that are abnormally high and we should be worried about?
cor(one)

# Model 1
# Let's construct a basic model with only main effects.
m1 <- lm(weight	~ age + htshoes +	ht + seated + arm + thigh + leg, data=one)

# summary of model
summary(m1)

# VIF
# Now, let's check the VIF of that model:
vif(m1)

# Should we adjust our model? YES!
  
# Model 2
m2 <- lm(weight	~ age +	ht + seated + arm + thigh + leg, data=one)
summary(m2)
vif(m2)

# We still need to adjust our model -- ht's VIF is too high.

# Model 3
m3 <- lm(weight	~ age + seated + arm + thigh + leg, data=one)
summary(m3)
vif(m3)

# No further adjustment needed -- all VIF are less than 10.

# R Student

# Let's check the studentized residuals (note: in R they are called standardized 
# residuals; R uses "studentized" to describe another type of residual, the 
# jackknife residual).
one$outlier <- abs(rstandard(m3))>2.5

# Should we investigate any points? 
table(one$outlier)
filter(one, one$outlier == TRUE)
# Yes: 13 and 22 are over 2.5.

# Cook's D
# Let's check influence/leverage using Cook's distance. 
gg_cooksd(m3) + theme_minimal()
# Are there any points we should investigate? Yes! 13 and 22 are "spikes."

# Model Assumptions

# Let's check the model assumptions. Remember, we assume that the residuals are 
# normally distributed with mean 0 and a common variance. We should use the 
# scatterplot (top left) to assess variance, the Q-Q plot (bottom left) to assess 
# normality, and the density/histogram (right side) to further assess normality.
almost_sas(m3)

# What are your thoughts? Are the assumptions met (roughly)? 
# Mostly - normality is questionable. Variance looks okay.

# Model 4 
# Reconstruct M3 without the points identified as issues. (Removing 13 and 22.)

# Exclude the potential issue datapoints
one_m4 <- filter(one, obs != 13 & obs != 22)
m4 <- lm(weight	~ age + seated + arm + thigh + leg, data=one_m4)

summary(m3)
summary(m4)

# How different are the results?

# Remember: we only check diagnostics once. If we continue to re-evaluate, we 
# will continue to find issue points (and will whittle down our dataset such that 
# it is no longer generalizable to the initial population of interest).

# Model Assumptions
# Let's check the model assumptions for M4. 
almost_sas(m4)

# What are your thoughts? Are the assumptions met (roughly)?
  
  