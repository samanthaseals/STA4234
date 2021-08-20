library(tidyverse)

wgt <- c(64, 71, 53, 67, 55, 58, 77, 57, 56, 51, 76, 68)
hgt <- c(57, 59, 49, 62, 51, 50, 55, 48, 42, 42, 61, 57)
age <- c(8, 10, 6, 11, 8, 7, 10, 9, 10, 6, 12, 9)

one <- tibble(wgt, hgt, age)

# create age^2 variable
one$age2 <- one$age^2

###########
# MODEL 1 #
###########

m1 <- lm(wgt ~ hgt + age + age2, data=one)

# model summary - omnibus test is here at the bottom
summary(m1)

# partial F tests

# height alone
full <- lm(wgt ~ hgt, data=one) # Full Model
anova(full) 

# height after adjusting for age
full <- lm(wgt ~ hgt + age, data=one) # Full Model
reduced <- lm(wgt ~ hgt, data=one) # Reduced model
anova(reduced, full) # Compare the models

# age^2 after adjusting for height and age
full <- lm(wgt ~ hgt + age + age2, data=one) # Full Model
reduced <- lm(wgt ~ hgt + age, data=one) # Reduced model
anova(reduced, full) # Compare the models

# multiple partial F tests

# age and age^2 after adjusting for height
full <- lm(wgt ~ hgt + age + age2, data=one) # Full Model
reduced <- lm(wgt ~ hgt, data=one) # Reduced model
anova(reduced, full) # Compare the models

# variable added last F tests
# (equivalent to what we did above -- but remember that order matters)
drop1(m1, .~., test="F")

# t tests 
summary(m1)

# 95% confidence interval for beta_i
confint(m1, level=0.95)

###########
# MODEL 2 #
###########

m2 <- lm(wgt ~ hgt + age, data=one)

# model results
summary(m2)

confint(m2)

# 95% confidence interval for mu_{Y|{hgt=55,age=9}}

# want CI for mu when hgt=55 and age=9, but this doesn't exist in dataset
hgt <- 55
age <- 9

new <- tibble(hgt, age)

# combine old and new data
one_cb <- bind_rows(one, new)

# find predictions and CI
one_cb$Prediction <- predict(m2, newdata = one_cb)

one_cb$LCL <- predict(m2, newdata = one_cb, 
                      interval = "confidence", 
                      level = 0.95)[, 2]
one_cb$UCL <- predict(m2, newdata = one_cb, 
                      interval = "confidence", 
                      level = 0.95)[, 3]

# filter down to just hgt=55 and age=9
select(filter(one_cb, age==9 & hgt==55), LCL, UCL)

# 95% prediction interval for $Y_{X_0}$ when height=55 and age=9

# want CI for mu when hgt=55 and age=9, but this doesn't exist in dataset
hgt <- 55
age <- 9

new <- tibble(hgt, age)

# combine old and new data
one_pb <- bind_rows(one, new)

# find predictions and CI
one_pb$Prediction <- predict(m2, newdata = one_pb)

one_pb$LCL <- predict(m2, newdata = one_pb, 
                      interval = "prediction", 
                      level = 0.95)[, 2]
one_pb$UCL <- predict(m2, newdata = one_pb, 
                      interval = "prediction", 
                      level = 0.95)[, 3]

# filter down to just hgt=55 and age=9
select(filter(one_pb, age==9 & hgt==55), LCL, UCL)