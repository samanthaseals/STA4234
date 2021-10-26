library(tidyverse)
library(gsheet)

# read in data
one <- read_sheet("https://docs.google.com/spreadsheets/d/1xzy1S8aoQsL8UdzdCgjUMofxFXoZ6L69WODAxmGifpg/edit#gid=0")

###########
# MODEL 1 #
###########

# we specify an interaction between two variables by joining them with a colon (:)
m1 <- lm(sbp ~ age + male + age:male, data=one)

# see model results
summary(m1)

# confidence intervals
confint(m1)

# coefficients for creating variables for graphing
c1 <- coefficients(m1)

# graph

# make R see sex as a categorical variable
one$male_fac <- as.factor(one$male) 

# create predicted values for males (male=1) and females (male=0)
one$pred_male <- c1[[1]] + c1[[2]]*one$age + c1[[3]] + c1[[4]]*one$age
one$pred_female <- c1[[1]] + c1[[2]]*one$age

ggplot(one, aes(x=age, y=sbp, color=male_fac)) +
  geom_point() +
  geom_text(aes(x = 74.9, y = 178, label = "Females"), color="black", show.legend = FALSE) + 
  geom_text(aes(x = 73.5, y = 164, label = "Males"), color="black", show.legend = FALSE) +
  geom_line(aes(y = pred_male), color = "black", linetype = "solid") +
  geom_line(aes(y = pred_female), color = "black", linetype = "solid") +
  xlab("Age") + xlim(15, 80) +
  ylab("SBP") +
  scale_color_discrete(name = "Sex", labels = c("Female", "Male")) +
  theme_minimal() 

###########
# MODEL 2 #
###########

# remove the interaction because it is not significant
m2 <- lm(sbp ~ age + male, data=one)

# see model results
summary(m2)

# confidence intervals
confint(m2)

# coefficients for creating variables for graphing
c2 <- coefficients(m2)

# create predicted values for males (male=1) and females (male=0)
one$pred_male <- c2[[1]] + c2[[2]]*one$age + c2[[3]]
one$pred_female <- c2[[1]] + c2[[2]]*one$age

ggplot(one, aes(x=age, y=sbp, color=male_fac)) +
  geom_point() +
  geom_text(aes(x = 74.9, y = 178, label = "Females"), color="black", show.legend = FALSE) + 
  geom_text(aes(x = 73.5, y = 164, label = "Males"), color="black", show.legend = FALSE) +
  geom_line(aes(y = pred_male), color = "black", linetype = "solid") +
  geom_line(aes(y = pred_female), color = "black", linetype = "solid") +
  xlab("Age") + xlim(15, 80) +
  ylab("SBP") +
  scale_color_discrete(name = "Sex", labels = c("Female", "Male")) +
  theme_minimal() 

#############
# EXAMPLE 2 #
#############

# read in data
two <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1QH5XDEUEmBGp2ZzO7OQS0T1vSELIjFAi2gTmnqokoMg/edit#gid=0"))

# want to treat the predictors as continuous, but R read them in as factors 
two$Sweetner <- str_remove(two$Sweetner, "[%]")
  two$Sweetner <- as.numeric(two$Sweetner)
two$MilkFat <- str_remove(two$MilkFat, "[%]")
  two$MilkFat <- as.numeric(two$MilkFat)
two$Air <- str_remove(two$Air, "[%]")
  two$Air <- as.numeric(two$Air)

m1 <- lm(Ratings ~ Sweetner + MilkFat + Air + Sweetner:MilkFat + Sweetner:Air + MilkFat:Air + Sweetner:MilkFat:Air, data=two)

summary(m1)