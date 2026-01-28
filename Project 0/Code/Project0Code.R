# Import Dataset

cort <- read.csv("C:/Users/stein/OneDrive/Documents/School/2026 Spring/Advanced Methods/BIOS6624/Project 0/Project0_Clean_v2.csv")


# Setting up libraries commonly used

library(MASS)
library(knitr)
library(emmeans)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lme4)
library(lmerTest)
library(nlme)
library(car)

# Data Cleaning/Inspection

colSums(is.na(cort)) # Cortisol..nmol.L. 5 missing 
                    # Booket..Clock.Time 0 missing 
                    # Booklet..Sample.interval.Decimal.Time..mins. 55 missing
                    # Booklet..Sample.interval 0 missing
                    # DHEA..nmol.L. 5 missing

sum(cort$Cortisol..nmol.L. >= 80, na.rm = T) # only 1 >= 80 
sum(cort$DHEA..nmol.L. >= 5.205, na.rm = T) # 6 >= 5.205

# Remove the missing values and the values outside range given

cort_clean <- cort[
  !is.na(cort$Cortisol..nmol.L.) &
    !is.na(cort$DHEA..nmol.L.) &
    cort$Cortisol..nmol.L. < 80 &
    cort$DHEA..nmol.L. < 5.205,
]

nrow(cort) - nrow(cort_clean) # 6 total rows removed

colSums(is.na(cort_clean)) # correctly removed missing cortisol and DHEA


# Boxplots to check for outliers

boxplot(cort_clean$Cortisol..nmol.L.,
        main = "Cortisol",
        ylab = "Cortisol (nmol/L)")

boxplot(cort_clean$DHEA..nmol.L.,
        main = "DHEA",
        ylab = "DHEA (nmol/L)")

boxplot(cort_clean$Booklet..Sample.interval.Decimal.Time..mins.,
        main = "Time to sample",
        ylab = "Time in Minutes")


# Histograms

hist(cort_clean$Cortisol..nmol.L.,
     breaks = 20,
     main = "Cortisol")

hist(cort_clean$DHEA..nmol.L.,
     breaks = 20,
     main = "DHEA")

hist(cort_clean$Booklet..Sample.interval.Decimal.Time..mins.,
     breaks = 20,
     main = "Time")

# All three are right skewed 

