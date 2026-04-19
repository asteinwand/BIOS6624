library(survminer)
library(survival)
library(gtsummary)
library(MASS)



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




### Start survival models ####

### complete case analysis to add back in dropped variables

base_final <- base[complete.cases(base[, c("SYSBP", "AGE", 
                                           "DIABETES", "BPMEDS")]), ]
nrow(base_final)

sapply(base[, c("SYSBP", "AGE", 
                "DIABETES", "BPMEDS")], function(x) sum(is.na(x)))

## split into male and female

base_male <- base_final[base_final$SEX == 1, ]
base_female <- base_final[base_final$SEX == 2, ]


## update survival objects for both

surv_male <- Surv(time = base_male$stroke_time, 
                  event = base_male$stroke_event)
surv_female <- Surv(time = base_female$stroke_time, 
                    event = base_female$stroke_event)


## Full model then stepwise model
# Male
library(MASS)

# Male full model
full_male <- coxph(surv_male ~ SYSBP + AGE + DIABETES + BPMEDS, 
                   data = base_male)

# Stepwise on male model
step_male <- stepAIC(full_male, 
                     scope = list(lower = ~ SYSBP + AGE + DIABETES,
                                  upper = ~ SYSBP + AGE + DIABETES + BPMEDS),
                     direction = "both",
                     trace = TRUE)

summary(step_male)


# female
# Female full model
full_female <- coxph(surv_female ~ SYSBP + AGE + DIABETES + BPMEDS, 
                     data = base_female)

# Stepwise on female model
step_female <- stepAIC(full_female,
                       scope = list(lower = ~ SYSBP + AGE + DIABETES,
                                    upper = ~ SYSBP + AGE + DIABETES + BPMEDS),
                       direction = "both",
                       trace = TRUE)

summary(step_female)


## remove bpmeds as not significant in males, barely in females
# add back in cursmoke becasuee we need one extra profile

final_male <- coxph(surv_male ~ SYSBP + AGE + 
                      DIABETES + CURSMOKE, data = base_male)
final_female <- coxph(surv_female ~ SYSBP + AGE + 
                        DIABETES + CURSMOKE, data = base_female)

### check assumptions

# Schoenfeld residuals
ph_male <- cox.zph(final_male)
ph_female <- cox.zph(final_female)

print(ph_male)
print(ph_female)


# Male
par(mfrow = c(2, 2))
plot(ph_male)

# Female
par(mfrow = c(2, 2))
plot(ph_female)



#### Survival Models ####

## profiles with age

#40
profiles_male_40 <- data.frame(
  SYSBP    = c(mean(base_male$SYSBP, na.rm = TRUE), 160, 
               mean(base_male$SYSBP, na.rm = TRUE), 160, 
               mean(base_male$SYSBP, na.rm = TRUE)),
  AGE      = 40,
  DIABETES = c(0, 0, 1, 1, 0),
  CURSMOKE = c(0, 0, 0, 0, 1)
)

profiles_female_40 <- data.frame(
  SYSBP    = c(mean(base_female$SYSBP, na.rm = TRUE), 160, 
               mean(base_female$SYSBP, na.rm = TRUE), 160, 
               mean(base_female$SYSBP, na.rm = TRUE)),
  AGE      = 40,
  DIABETES = c(0, 0, 1, 1, 0),
  CURSMOKE = c(0, 0, 0, 0, 1)
)

# Age 40
male_40   <- 1 - summary(survfit(final_male, newdata = profiles_male_40), 
                         times = 3652.5)$surv
female_40 <- 1 - summary(survfit(final_female, newdata = profiles_female_40), 
                         times = 3652.5)$surv

table_40 <- data.frame(
  Profile  = c("Average", "High BP", "Diabetes", "High BP + DM", "Smoker"),
  Male     = round(male_40, 4),
  Female   = round(female_40, 4)
)



# 50
profiles_male_50 <- data.frame(
  SYSBP    = c(mean(base_male$SYSBP, na.rm = TRUE), 160, 
               mean(base_male$SYSBP, na.rm = TRUE), 160, 
               mean(base_male$SYSBP, na.rm = TRUE)),
  AGE      = 50,
  DIABETES = c(0, 0, 1, 1, 0),
  CURSMOKE = c(0, 0, 0, 0, 1)
)

profiles_female_50 <- data.frame(
  SYSBP    = c(mean(base_female$SYSBP, na.rm = TRUE), 160, 
               mean(base_female$SYSBP, na.rm = TRUE), 160, 
               mean(base_female$SYSBP, na.rm = TRUE)),
  AGE      = 50,
  DIABETES = c(0, 0, 1, 1, 0),
  CURSMOKE = c(0, 0, 0, 0, 1)
)

# Age 50
male_50   <- 1 - summary(survfit(final_male, newdata = profiles_male_50), 
                         times = 3652.5)$surv
female_50 <- 1 - summary(survfit(final_female, newdata = profiles_female_50), 
                         times = 3652.5)$surv

table_50 <- data.frame(
  Profile  = c("Average", "High BP", "Diabetes", "High BP + DM", "Smoker"),
  Male     = round(male_50, 4),
  Female   = round(female_50, 4)
)


# 60
profiles_male_60 <- data.frame(
  SYSBP    = c(mean(base_male$SYSBP, na.rm = TRUE), 160, 
               mean(base_male$SYSBP, na.rm = TRUE), 160, 
               mean(base_male$SYSBP, na.rm = TRUE)),
  AGE      = 60,
  DIABETES = c(0, 0, 1, 1, 0),
  CURSMOKE = c(0, 0, 0, 0, 1)
)

profiles_female_60 <- data.frame(
  SYSBP    = c(mean(base_female$SYSBP, na.rm = TRUE), 160, 
               mean(base_female$SYSBP, na.rm = TRUE), 160, 
               mean(base_female$SYSBP, na.rm = TRUE)),
  AGE      = 60,
  DIABETES = c(0, 0, 1, 1, 0),
  CURSMOKE = c(0, 0, 0, 0, 1)
)

# Age 60
male_60   <- 1 - summary(survfit(final_male, newdata = profiles_male_60), 
                         times = 3652.5)$surv
female_60 <- 1 - summary(survfit(final_female, newdata = profiles_female_60), 
                         times = 3652.5)$surv

table_60 <- data.frame(
  Profile  = c("Average", "High BP", "Diabetes", "High BP + DM", "Smoker"),
  Male     = round(male_60, 4),
  Female   = round(female_60, 4)
)



# FInal table for all of them

# Add age column to each table
table_40$Age <- 40
table_50$Age <- 50
table_60$Age <- 60

# Combine all three
final_table <- rbind(table_40, table_50, table_60)

# Reorder columns so Age is first
final_table <- final_table[, c("Age", "Profile", "Male", "Female")]

print(final_table)



















