library(tidyverse)

# The survival times (in months) for two treatments for patients with severe 
# chronic left-ventricular heart failure are as follows:

###############################################################################
#                           CREATE DATA SET                                   #
###############################################################################

# define vector "new"
Treatment <- c(rep("Standard", 28), rep("New", 28))

# define vector "survival_time"
SurvivalTime <- c( 4, 15, 24, 10,  1, 27, 31,
                    14,  2, 16, 32,  7, 13, 36,
                    29,  6, 12, 18, 14, 15, 18,
                    6, 13, 21, 20,  8,  3, 24,
                    5, 20, 29, 15,  7, 32, 36,
                    17, 15, 19, 35, 10, 16, 39,
                    27, 14, 10, 16, 12, 13, 16,
                    9, 18, 33, 30, 29, 31, 27)  

# put the vectors into a data set together
data <- tibble(Treatment, SurvivalTime)
# we are using tibbles rather than data frames
# you can read about tibbles here:
# https://tibble.tidyverse.org/

###############################################################################
#                         SUMMARIZE DATA                                      #
###############################################################################

# OVERALL SUMMARY

# this creates new variables "mean" "sd" and "median"
summary_all <- data %>% summarize(mean = mean(SurvivalTime), 
                                  sd = sd(SurvivalTime),
                                  median = median(SurvivalTime)) 
# there are other statistics we can request from the summarize() function
# you can read about the summarize() function here:
# https://dplyr.tidyverse.org/reference/summarise.html

summary_all

# The mean survival time is 
round(summary_all$mean[1], digits = 2)

# The standard deviation of survival time is
round(summary_all$sd[1], digits = 2)

# The median survival time is 
round(summary_all$median[1], digits = 2)

# GROUP SUMMARIES

summary_groups <- data %>% group_by(Treatment) %>% # telling it to do everything by therapy group 
  summarise(mean = mean(SurvivalTime), 
            sd = sd(SurvivalTime), 
            median = median(SurvivalTime))

summary_groups

# The mean survival time for standard therapy is 
round(summary_groups$mean[2], digits = 2)

# The standard deviation of survival time for standard therapy is 
round(summary_groups$sd[2], digits = 2)

# The median survival time for standard therapy is 
round(summary_groups$median[2], digits = 2)

# The mean survival time for new therapy is 
round(summary_groups$mean[1], digits = 2)

# The standard deviation of survival time for new therapy is 
round(summary_groups$sd[1], digits = 2)

# The median survival time for new therapy is 
round(summary_groups$median[1], digits = 2)

###############################################################################
#                            DATA VISUALIZATION                               #
###############################################################################

data %>% ggplot(aes(x = Treatment, y = SurvivalTime, fill = Treatment)) +
  # ggplot() tells R we want to graph; we declare the x and y here
  # we can declare them elsewhere, though (in other geom_XXXXX() functions)
  # fill specifies the grouping variable
  geom_boxplot() + # creates a boxplot
  xlab("Treatment") + # adds x-axis label
  ylab("Survival Time (months)") + # adds y-axis label 
  theme_minimal() # changes graph background formatting

# EXAMPLE 2

# The following data are weight ($Y$), 
#                        height ($X_1$), 
#                    and age ($X_2$) 
# for a sample of 12 children. 

wgt <- c(64, 71, 53, 67, 55, 58, 77, 57, 56, 51, 76, 68)
hgt <- c(57, 59, 49, 62, 51, 50, 55, 48, 42, 42, 61, 57)
age <- c(8, 10, 6, 11, 8, 7, 10, 9, 10, 6, 12, 9)

data <- tibble(wgt, hgt, age)

###############################################################################
#                            DATA VISUALIZATION                               #
###############################################################################

#scatterplot for wgt x hgt
data %>% ggplot(aes(x = hgt, y = wgt)) +
  geom_point() + # puts points on the graph
  xlab("Height") +
  ylab("Weight") +
  theme_minimal()
  
# scatterplot for wgt x age
data %>% ggplot(aes(x = age, y = wgt)) +
  geom_point() + # puts points on the graph
  xlab("Age") +
  ylab("Weight") +
  theme_minimal()