library(gsheet)
library(tidyverse)

# INSERT YOUR NAME HERE

data <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1H3TP-2SBMGleriJLESOe1cdCjtSj2F76bUh5iBqC8tI"))

# keep the variables of interest
data <- data %>% select(subjid, sbp, BMI, HbA1c, BPmeds, DMmeds, Insured, HSgrad)

# keep complete cases
data <- na.omit(data)

# The variables you will be using:
# sbp
# BMI
# HbA1c
# BPmeds
# DMmeds
# HSgrad
# Insured