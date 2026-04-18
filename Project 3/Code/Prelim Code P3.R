library(survminer)
library(survival)
library(gtsummary)



# Upload Data set
fhsdata <- read.csv("C:/Users/stein/OneDrive/Documents/School/2026 Spring/Advanced Methods/BIOS6624/Project 3/Data Raw/frmgham2.csv")


# Notes to self:
# remove stroke = 1 at baseline
# remove stroke >3652.5 days
# check missing: BP(systolic), age, diabetes
# heart disease, smoking status, bp medication, HDL/LDL, BMI
# 

## Initial data cleaning

# filter to use just the baseline values
base <- fhsdata[fhsdata$PERIOD == 1,]

# remove anyone with stroke at baseline (STROKE = 1 at period 1)
base <- base[base$STROKE != 1, ]

# remove stroke time = 0 
base <- base[base$TIMESTRK != 0, ]

# censor follow-up to 10 years (365.25*10)
base$stroke_event <- ifelse(base$TIMESTRK <= 3652.5 & base$STROKE == 1, 1, 0)
base$stroke_time <- ifelse(base$TIMESTRK <= 3652.5, base$TIMESTRK, 3652.5)

# check covariates for missing
sum(is.na(base$SYSBP)) # 0
sum(is.na(base$AGE)) # 0
sum(is.na(base$DIABETES)) # 0 
sum(is.na(base$CURSMOKE)) # 0
sum(is.na(base$BMI)) # 16 
sum(is.na(base$TOTCHOL)) # 49
sum(is.na(base$BPMEDS)) # 54
sum(is.na(base$HEARTRTE)) # 0

# check covariates for implausible values
sum(base$SYSBP > 300 | base$SYSBP < 60) # 0
sum(base$AGE > 99 | base$AGE <= 18) # 0

min(base$BMI, na.rm = TRUE) # 15.54
max(base$BMI, na.rm = TRUE) # 56.8 all plausible values for BMI





### fit KM curves to determine what we put in the cox model

surv_obj <- Surv(time = base$stroke_time, event = base$stroke_event)


# not stratified
km_overall <- survfit(surv_obj ~ 1, data = base)

ggsurvplot(km_overall,
           data = base,
           xlab = "Time (days)",
           ylab = "Stroke-free Survival Probability",
           title = "Overall Kaplan-Meier Curve")


# stratified by sex
km_sex <- survfit(surv_obj ~ SEX, data = base)

ggsurvplot(km_sex,
           data = base,
           legend.labs = c("Male", "Female"),
           xlab = "Time (days)",
           ylab = "Stroke-free Survival Probability",
           title = "Kaplan-Meier by Sex")



##### FILTERED DATA WRONG ####### fix it

hist(fhsdata$TIMESTRK[fhsdata$PERIOD == 1 & fhsdata$STROKE == 1],
     main = "TIMESTRK for Stroke=1 at Period 1",
     xlab = "Days")


summary(fhsdata$TIMESTRK[fhsdata$PERIOD == 1 & fhsdata$STROKE == 1])















