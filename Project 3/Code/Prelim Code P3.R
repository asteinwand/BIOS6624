library(survminer)
library(survival)
library(gtsummary)
library(MASS)
library(gt)



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


##########################################################
## Table 1
############################################################
library(gtsummary)

table1_data <- base

table1_data$SEX       <- factor(base$SEX,       levels = c(1, 2), labels = c("Male", "Female"))
table1_data$DIABETES  <- factor(base$DIABETES,  levels = c(0, 1), labels = c("No", "Yes"))
table1_data$CURSMOKE  <- factor(base$CURSMOKE,  levels = c(0, 1), labels = c("No", "Yes"))
table1_data$stroke_event <- factor(base$stroke_event, levels = c(0, 1), labels = c("No", "Yes"))

# Build Table 1 - only variables in final model + age + outcome
table1 <- tbl_summary(
  data = table1_data[, c("SEX", "AGE", "SYSBP", "DIABETES", "CURSMOKE", "stroke_event")],
  by = SEX,
  label = list(
    AGE          ~ "Age (years)",
    SYSBP        ~ "Systolic Blood Pressure (mmHg)",
    DIABETES     ~ "Diabetes",
    CURSMOKE     ~ "Current Smoker",
    stroke_event ~ "Stroke Event within 10 Years"
  ),
  statistic = list(
    all_continuous()  ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = list(
    all_continuous()  ~ 1,
    all_categorical() ~ c(0, 1)
  ),
  missing = "ifany",
  missing_text = "Missing"
) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_caption("**Table 1. Baseline Characteristics by Sex**")


print(table1)

table1


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
male_40   <- as.vector(summary(survfit(final_male, newdata = profiles_male_40), 
                         times = 3652.5)$surv)
female_40 <- as.vector(summary(survfit(final_female, newdata = profiles_female_40), 
                         times = 3652.5)$surv)

table_40 <- data.frame(
  Age      = 40,
  Profile  = c("Average", "High BP", "Diabetes", "High BP + DM", "Smoker"),
  Male     = round(1 - male_40, 4),
  Female   = round(1 - female_40, 4)
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
male_50   <- as.vector(summary(survfit(final_male, newdata = profiles_male_50), 
                         times = 3652.5)$surv)
female_50 <- as.vector(summary(survfit(final_female, newdata = profiles_female_50), 
                         times = 3652.5)$surv)

table_50 <- data.frame(
  Age      = 50,
  Profile  = c("Average", "High BP", "Diabetes", "High BP + DM", "Smoker"),
  Male     = round(1 - male_50, 4),
  Female   = round(1 - female_50, 4)
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
male_60   <- as.vector(summary(survfit(final_male, newdata = profiles_male_60), 
                         times = 3652.5)$surv)
female_60 <- as.vector(summary(survfit(final_female, newdata = profiles_female_60), 
                         times = 3652.5)$surv)

table_60 <- data.frame(
  Age      = 60,
  Profile  = c("Average", "High BP", "Diabetes", "High BP + DM", "Smoker"),
  Male     = round(1 - male_60, 4),
  Female   = round(1 - female_60, 4)
)


########## DO NOT USE THIS TABLE ###############
# FInal table for all of them

final_table <- rbind(table_40, table_50, table_60)
rownames(final_table) <- NULL

print(final_table)
#################################################


# Try a different approach
# Age 40
m40 <- summary(survfit(final_male,   newdata = profiles_male_40),
               times = 3652.5)$surv
f40 <- summary(survfit(final_female, newdata = profiles_female_40),
               times = 3652.5)$surv

# Age 50
m50 <- summary(survfit(final_male,   newdata = profiles_male_50),
               times = 3652.5)$surv
f50 <- summary(survfit(final_female, newdata = profiles_female_50),
               times = 3652.5)$surv

# Age 60
m60 <- summary(survfit(final_male,   newdata = profiles_male_60),
               times = 3652.5)$surv
f60 <- summary(survfit(final_female, newdata = profiles_female_60),
               times = 3652.5)$surv

# Build table manually indexing each profile [1,i]
final_table <- data.frame(
  Age     = rep(c(40, 50, 60), each = 5),
  Profile = rep(c("Average", "High BP", "Diabetes", "High BP + DM", "Smoker"), 3),
  Male    = round(1 - c(m40[1,1], m40[1,2], m40[1,3], m40[1,4], m40[1,5],
                        m50[1,1], m50[1,2], m50[1,3], m50[1,4], m50[1,5],
                        m60[1,1], m60[1,2], m60[1,3], m60[1,4], m60[1,5]), 4),
  Female  = round(1 - c(f40[1,1], f40[1,2], f40[1,3], f40[1,4], f40[1,5],
                        f50[1,1], f50[1,2], f50[1,3], f50[1,4], f50[1,5],
                        f60[1,1], f60[1,2], f60[1,3], f60[1,4], f60[1,5]), 4)
)

print(final_table)

# Option 1 - knitr alone (likely already installed)
library(knitr)
kable(final_table,
      col.names = c("Age", "Risk Profile", "Male", "Female"),
      caption = "10-Year Stroke Probability by Risk Profile, Age, and Sex",
      align = c("c", "l", "c", "c"))


library(gt)

# Build survival probability table (remove 1- for survival, not event prob)
# Rows: Male then Female | Columns: Age 40, 50, 60 | Profiles: 5 rows each

surv_table <- data.frame(
  Sex = c(rep("Male", 5), rep("Female", 5)),
  Profile = rep(c("Average", "High BP (160)", "Diabetes", "High BP + DM", "Smoker"), 2),
  Age_40 = round(c(
    m40[1,1], m40[1,2], m40[1,3], m40[1,4], m40[1,5],
    f40[1,1], f40[1,2], f40[1,3], f40[1,4], f40[1,5]
  ), 4),
  Age_50 = round(c(
    m50[1,1], m50[1,2], m50[1,3], m50[1,4], m50[1,5],
    f50[1,1], f50[1,2], f50[1,3], f50[1,4], f50[1,5]
  ), 4),
  Age_60 = round(c(
    m60[1,1], m60[1,2], m60[1,3], m60[1,4], m60[1,5],
    f60[1,1], f60[1,2], f60[1,3], f60[1,4], f60[1,5]
  ), 4)
)

# Format as gt table with Sex as row grouping
surv_gt <- surv_table %>%
  gt(groupname_col = "Sex") %>%
  cols_label(
    Profile = "Risk Profile",
    Age_40  = "Age 40",
    Age_50  = "Age 50",
    Age_60  = "Age 60"
  ) %>%
  tab_spanner(
    label = "10-Year Stroke-Free Survival Probability",
    columns = c(Age_40, Age_50, Age_60)
  ) %>%
  tab_header(
    title    = "10-Year Stroke-Free Survival Probability by Risk Profile",
    subtitle = "Stratified by Sex and Age"
  ) %>%
  fmt_number(
    columns  = c(Age_40, Age_50, Age_60),
    decimals = 4
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style     = cell_fill(color = "#f2f2f2"),
    locations = cells_row_groups()
  ) %>%
  cols_align(align = "center", columns = c(Age_40, Age_50, Age_60)) %>%
  cols_align(align = "left",   columns = Profile)

surv_gt












