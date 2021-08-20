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

summary(m1)

c1 <- coefficients(m1)

# graph holding height constant

# add variables containing y hat
one$pred50_m1 <- c1[[1]] + c1[[2]]*50 + c1[[3]]*one$age + c1[[4]]*one$age2
one$pred53_m1 <- c1[[1]] + c1[[2]]*53 + c1[[3]]*one$age + c1[[4]]*one$age2
one$pred55_m1 <- c1[[1]] + c1[[2]]*55 + c1[[3]]*one$age + c1[[4]]*one$age2

ggplot(one, aes(x=age, y=wgt)) +
  geom_point() +
  geom_line(aes(y = pred50_m1), color = "black", linetype = "dashed") +
  geom_line(aes(y = pred53_m1), color = "black", linetype = "dashed") +
  geom_line(aes(y = pred55_m1), color = "black", linetype = "dashed") +
  geom_text(aes(x = 12.5, y = 70.7, label = "height = 55")) + 
  geom_text(aes(x = 12.5, y = 69.25, label = "height = 53")) +
  geom_text(aes(x = 12.5, y = 67, label = "height = 50")) +
  xlab("Age") + xlim(6, 13) +
  ylab("Weight") +
  theme_minimal()

# ANOVA table
anova(m1)

###########
# MODEL 2 #
###########

m2 <- lm(wgt ~ hgt + age, data=one)

summary(m2)

c2 <- coefficients(m2)

# Graph holding height constant

# add variables containing y hat
one$pred50_m2 <- c2[[1]] + c2[[2]]*50 + c2[[3]]*one$age 
one$pred53_m2 <- c2[[1]] + c2[[2]]*53 + c2[[3]]*one$age 
one$pred55_m2 <- c2[[1]] + c2[[2]]*55 + c2[[3]]*one$age 

ggplot(one, aes(x=age, y=wgt)) +
  geom_point() +
  geom_line(aes(y = pred50_m2), color = "black", linetype = "dashed") +
  geom_line(aes(y = pred53_m2), color = "black", linetype = "dashed") +
  geom_line(aes(y = pred55_m2), color = "black", linetype = "dashed") +
  geom_text(aes(x = 12.5, y = 71, label = "height = 55")) + 
  geom_text(aes(x = 12.5, y = 69.5, label = "height = 53")) +
  geom_text(aes(x = 12.5, y = 67.25, label = "height = 50")) +
  xlab("Age") + xlim(6, 13) +
  ylab("Weight") +
  theme_minimal()

# ANOVA table
anova(m2)