library(tidyverse)

###############################################################################
#                                EXAMPLE 1                                    #
###############################################################################

# The following data are systolic blood pressure (SBP) and age for a sample of 
# 30 individuals. We will construct the scatter plot and the regression line of 
# best fit.

sbp <- c(144, 220, 138, 145, 162, 142, 170, 124, 158, 154, 162, 150, 140, 110, 128, 
         130, 135, 114, 116, 124, 136, 142, 120, 120, 160, 158, 144, 130, 125, 175)

age<- c(39, 47, 45, 47, 65, 46, 67, 42, 67, 56, 64, 56, 59, 34, 42, 
        48, 45, 17, 20, 19, 36, 50, 39, 21, 44, 53, 63, 29, 25, 69)

one <- tibble(sbp, age)

###############################################################################
#                            DATA VISUALIZATION                               #
###############################################################################

# scatterplot with age on the x-axis and SBP on the y-axis
ggplot(one, aes(x=age, y=sbp)) + 
  geom_point() + 
  labs(x="Age", y="SBP") +
  theme_minimal()

###############################################################################
#                            REGRESSION MODEL                                 #
###############################################################################

# construct the model using the lm() function
# lm = linear model
m1 <- lm(sbp ~ age, data=one)
# on the left side of the ~ is the outcome
# on the rights ide of the ~ is the predictor(s)

# we are saving the model results to a list called "m1"
# this allows us to run the model results through other functions
# we will learn more functions as we progress through the course

# run the model through the summary() function to see coefficients
summary(m1)
# note that there will be other pieces we are interested in in the future,
# but for now we are only interested in the coefficients

# the y-intercept is 98.7147
# the slope is 0.9709

# when age is 0, the average SBP is 98.7147 mmHg
# as age increases by 1 year, we expect SBP to increase by 0.9709 mmHg
# as age increases by 10 years, we expect SBP to increase by 9.709 mmHg

###############################################################################
#                          STATISTICAL INFERENCE                              #
###############################################################################

# estimate s^2_{Y|X} using MSE
anova(m1)
# the anova() function will give us the ANOVA table for the model

# MSE = 299.8

# is X a significant predictor of Y?
summary(m1)

# now we look at the t-test results from the summary() function

# age is a significant predictor of SBP (t_0=4.618, p<0.001)

# 95% confidence interval for \beta_1
confint(m1, level=0.95)

# the 95% CI for \beta_1 is (0.54, 1.40)

# 99% confidence interval for \beta_1
confint(m1, level=0.99)

# the 99% CI for \beta_1 is (0.39, 1.55)


# 95% confidence interval for \mu_{Y|X_0} when X_0=35

# want CI for mu when X_0=35, but this doesn't exist in dataset
age <- 35
sbp <- NA

new <- tibble(sbp, age)

# combine old and new data
one_cb <- rbind(one, new)

# find predictions and CI
one_cb$Prediction <- predict(m1, newdata = one_cb)

one_cb$LCL <- predict(m1, newdata = one_cb, 
                      interval = "confidence", 
                      level = 0.95)[, 2]
one_cb$UCL <- predict(m1, newdata = one_cb, 
                      interval = "confidence", 
                      level = 0.95)[, 3]

# filter down to just X_0=35
filter(one_cb, age==35)

# the 95% CI for mu is (125, 141)

# 95% confidence band for \mu_{Y|X_0}

ggplot(one_cb, aes(x=age, y=Prediction)) +
  # Add a ribbon with the confidence band
  geom_smooth(color="black",
              aes(
                # lower and upper bound of the ribbon
                ymin = LCL, ymax = UCL
              ),
              stat = "identity") +
  xlab("Age") +
  ylab("SBP") +
  theme_minimal()

# 95% prediction interval for Y_{X_0} when X_0=35

# want CI for mu when X_0=35, but this doesn't exist in dataset
age <- 35
sbp <- NA

new <- tibble(sbp, age)

# combine old and new data
one_pb <- rbind(one, new)

# find predictions and CI
one_pb$Prediction <- predict(m1, newdata = one_pb)

one_pb$LCL <- predict(m1, newdata = one_pb, 
                      interval = "prediction", 
                      level = 0.95)[, 2]
one_pb$UCL <- predict(m1, newdata = one_pb, 
                      interval = "prediction", 
                      level = 0.95)[, 3]

# filter down to just X_0=35
filter(one_pb, age==35)

# the 95% PI for Y is (96.4, 169.0)

# 95% prediction band for \mu_{Y|X_0}

ggplot(one_pb, aes(x=age, y=Prediction)) +
  # Add a ribbon with the confidence band
  geom_smooth(color="black",
              aes(
                # lower and upper bound of the ribbon
                ymin = LCL, ymax = UCL
              ),
              stat = "identity") +
  xlab("Age") +
  ylab("SBP") +
  theme_minimal()


# scatterplot with regression line, 95% confidence bands, and 95% prediction bands

# Redo predictions without X=35
one_pi <- predict(m1, interval = "prediction")

# Combine dataset with PI 
new_one <- cbind(one, one_pi)

ggplot(new_one, aes(x=age, y=sbp)) +
  geom_point() +
  stat_smooth(method = lm,color="black") +
  geom_line(aes(y = lwr), color = "black", linetype = "dashed") +
  geom_line(aes(y = upr), color = "black", linetype = "dashed") +
  xlab("Age") +
  ylab("SBP") +
  theme_minimal()
