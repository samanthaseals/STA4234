library(tidyverse)
library(fastDummies)
library(car)

#############
# EXAMPLE 1 #
#############

one <- read_csv("/Volumes/GoogleDrive/My Drive/STA4234/datasets/kickstarter.csv")

# look at the state of the campaigns 
# table(one$state)

# want to exclude "live" "suspended" and undefined
one2 <- one %>% filter(state != "live" & state != "suspended" & state != "undefined")

# check to see if our filtering worked
# table(one2$state)

m1 <- lm(usd_goal_real ~ state, data=one2)

summary(m1)

# create dummy variables
one2 <- dummy_cols(one2, select_columns = "state")

# can specify dummy variables in model itself
m2 <- lm(usd_goal_real ~ state_successful + state_failed, data=one2)
summary(m2)

# want to specify "successful" as the reference group, so that is the one we will leave out
m3 <- lm(usd_goal_real ~ state_canceled + state_failed, data=one2)
summary(m3)

#############
# EXAMPLE 2 #
#############

# read in data
two <- read_sheet("https://docs.google.com/spreadsheets/d/1QH5XDEUEmBGp2ZzO7OQS0T1vSELIjFAi2gTmnqokoMg/edit#gid=0")

two$Sweetner <- as.factor(two$Sweetner)
two$MilkFat <- as.factor(two$MilkFat)
two$Air <- as.factor(two$Air)

m1 <- lm(Ratings ~ Sweetner + MilkFat + Air + Sweetner:MilkFat + Sweetner:Air + MilkFat:Air + Sweetner:MilkFat:Air, data=two)

summary(m1)