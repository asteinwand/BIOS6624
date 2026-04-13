library(survminer)
library(gtsummary)



# Upload Data set
fhsdata <- read.csv("C:/Users/stein/OneDrive/Documents/School/2026 Spring/Advanced Methods/BIOS6624/Project 3/Data Raw/frmgham2.csv")


# Notes to self:
# remove stroke = 1 at baseline
# remove stroke >3652.5 days
# check missing: BP(systolic), age, diabetes
# heart disease, smoking status, bp medication, HDL/LDL, BMI
# 