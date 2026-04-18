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

# remove anyone with stroke at baseline (STROKE = 1 at period 1)
clean <- fhsdata[!(fhsdata$TIMESTRK == 0 & fhsdata$PERIOD == 1), ]

# filter to use just the baseline values
base <- clean[clean$PERIOD == 1, ]

# censor follow-up to 10 years (365.25*10)
base$stroke_event <- ifelse(base$TIMESTRK <= 3652.5 & base$STROKE == 1, 1, 0)
base$stroke_time <- ifelse(base$TIMESTRK <= 3652.5, base$TIMESTRK, 3652.5)


# check covariates for missing
sum(is.na(base$SYSBP)) # 0
sum(is.na(base$AGE)) # 0
sum(is.na(base$DIABETES)) # 0 
sum(is.na(base$CURSMOKE)) # 0
sum(is.na(base$BMI)) # 17 
sum(is.na(base$TOTCHOL)) # 52
sum(is.na(base$BPMEDS)) # 60
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
           ylab = "Survival Probability",
           title = "Kaplan-Meier by Sex",
           ylim = c(0.96, 1),        
           break.y.by = 0.02,
           risk.table = T)



##### FILTERED DATA WRONG ####### fix it

hist(fhsdata$TIMESTRK[fhsdata$PERIOD == 1 & fhsdata$STROKE == 1],
     main = "TIMESTRK for Stroke=1 at Period 1",
     xlab = "Days")


summary(fhsdata$TIMESTRK[fhsdata$PERIOD == 1 & fhsdata$STROKE == 1])


### check the variables that we are debating including


# Dichotomized versions are just for KM plotting
base$bmi_cat    <- ifelse(base$BMI >= 30, "Obese", "Non-obese")
base$chol_cat   <- ifelse(base$TOTCHOL >= 240, "High Chol", "Normal Chol")
base$bpmeds_cat <- ifelse(base$BPMEDS == 1, "On BP Meds", "Not on BP Meds")
base$heartrte_cat <- ifelse(base$HEARTRTE > 100, "High HR", "Normal HR")
base$smoke_cat <- ifelse(base$CURSMOKE == 1, "Smoker", "Non-smoker")


# BMI
km_bmi <- survfit(surv_obj ~ bmi_cat, data = base)
ggsurvplot(km_bmi, data = base,,
           ylim = c(0.94, 1),
           xlab = "Time (days)", 
           ylab = "Stroke-free Survival Probability",
           title = "KM Curve by BMI")

# Total cholesterol
km_chol <- survfit(surv_obj ~ chol_cat, data = base)
ggsurvplot(km_chol, data = base,
           ylim = c(0.94, 1),
           xlab = "Time (days)",
           ylab = "Stroke-free Survival Probability",
           title = "KM Curve by Total Cholesterol")

# BP meds
km_bpmeds <- survfit(surv_obj ~ bpmeds_cat, data = base)
ggsurvplot(km_bpmeds, data = base,
           ylim = c(0.94, 1),
           xlab = "Time (days)",
           ylab = "Stroke-free Survival Probability",
           title = "KM Curve by BP Meds")

# Smoking
km_smoke <- survfit(surv_obj ~ smoke_cat, data = base)
ggsurvplot(km_smoke, data = base,
           ylim = c(0.96, 1),
           xlab = "Time (days)",
           ylab = "Stroke-free Survival Probability",
           title = "KM Curve by Smoking Status")

# Heart rate
km_heartrte <- survfit(surv_obj ~ heartrte_cat, data = base)
ggsurvplot(km_heartrte, data = base,
           ylim = c(0.96, 1),
           xlab = "Time (days)",
           ylab = "Stroke-free Survival Probability",
           title = "KM Curve by Heart Rate")


### complete case analysis to add back in dropped variables

base_final <- base[complete.cases(base[, c("SYSBP", "AGE", "DIABETES", "BPMEDS")]), ]
nrow(base_final)

sapply(base[, c("SYSBP", "AGE", "DIABETES", "BPMEDS")], function(x) sum(is.na(x)))


