## In class WS 4

library(survival)
library(survminer)

ovdat <- read.csv("C:/Users/stein/Downloads/ovarian.csv")

# Fit a Cox model with all covariates
cox_full <- coxph(Surv(futime, fustat) ~ rx + age + factor(resid.ds) + factor(ecog.ps),
                  data = ovarian)

summary(cox_full)

# Test proportional hazards using Schoenfeld residuals
ph_test <- cox.zph(cox_full)
ph_test

# Graphical PH diagnostics with survminer
ggcoxzph(ph_test)


## 2

cox_model <- coxph(Surv(futime, fustat) ~ rx + age + factor(resid.ds) + factor(ecog.ps),
                   data = ovarian)

summary(cox_model)


## 3


fit_km_rx <- survfit(Surv(futime, fustat) ~ rx, data = ovarian)

ggsurvplot(
  fit_km_rx,
  data = ovarian,
  pval = TRUE,
  legend.title = "Treatment",
  legend.labs = c("Rx 1", "Rx 2"),
  xlab = "Days since diagnosis",
  ylab = "Survival probability",
  title = "Kaplan–Meier curves by treatment"
)


# Compute mean age
mean_age <- mean(ovarian$age, na.rm = TRUE)

# Newdata for Rx 1 and Rx 2 at mean age, resid.ds=1, ecog.ps=1
newdata_rx <- data.frame(
  rx       = c(1, 2),
  age      = mean_age,
  resid.ds = factor(1, levels = sort(unique(ovarian$resid.ds))),
  ecog.ps  = factor(1, levels = sort(unique(ovarian$ecog.ps)))
)

# Option 1: survfit from cox model
fit_cox_rx <- survfit(cox_model, newdata = newdata_rx)

ggsurvplot(
  fit_cox_rx,
  data = newdata_rx,
  legend.title = "Treatment",
  legend.labs = c("Rx 1 (adjusted)", "Rx 2 (adjusted)"),
  xlab = "Days since diagnosis",
  ylab = "Adjusted survival probability",
  title = "Cox-adjusted survival curves by treatment"
)


## 4

# Time point: 1 year = 365 days
t1 <- 365

# Create 4 profiles for treatment 1
profiles_t1 <- expand.grid(
  rx       = 1,
  resid.ds = c(1, 2),
  ecog.ps  = c(1, 2)
)

profiles_t1$age <- mean(ovarian$age)

# Convert to factors with correct levels
profiles_t1$resid.ds <- factor(profiles_t1$resid.ds,
                               levels = sort(unique(ovarian$resid.ds)))
profiles_t1$ecog.ps  <- factor(profiles_t1$ecog.ps,
                               levels = sort(unique(ovarian$ecog.ps)))

# Fit survival curves for the 4 profiles
fit_profiles_t1 <- survfit(cox_model, newdata = profiles_t1)

# Function to extract survival at a specific time (interpolates if needed)
get_surv_at_time <- function(fit, time_point) {
  s <- summary(fit)
  # last survival value before the time point
  idx <- max(which(s$time <= time_point))
  return(s$surv[idx])
}

# Apply to each profile
one_year_surv <- sapply(1:4, function(i) {
  get_surv_at_time(fit_profiles_t1[i], t1)
})

# Combine results
result_profiles <- cbind(profiles_t1, one_year_surv)
result_profiles



## 5

# Patient profiles
new_patients <- data.frame(
  rx       = c(1, 2),
  age      = c(60, 50),
  resid.ds = factor(c(1, 2), levels = sort(unique(ovarian$resid.ds))),
  ecog.ps  = factor(c(1, 2), levels = sort(unique(ovarian$ecog.ps)))
)

# Fit survival curves
fit_new <- survfit(cox_model, newdata = new_patients)

# Plot curves
ggsurvplot(
  fit_new,
  data = new_patients,
  legend.title = "Patient",
  legend.labs = c("Patient 1", "Patient 2"),
  xlab = "Days since diagnosis",
  ylab = "Survival probability",
  title = "Estimated survival curves for Patient 1 and Patient 2"
)


# Times of interest
times_interest <- c(365, 365*1.5, 365*2)

# Function to extract survival at or before a time point from a survfit object
extract_surv <- function(fit_obj, t) {
  s <- summary(fit_obj)
  idx <- max(which(s$time <= t))
  s$surv[idx]
}

# Compute survival for each patient at each time using fit_new
surv_patient1 <- sapply(times_interest, function(t) extract_surv(fit_new[1], t))
surv_patient2 <- sapply(times_interest, function(t) extract_surv(fit_new[2], t))

# Build table
prob_table <- data.frame(
  patient    = rep(c("Patient 1", "Patient 2"), each = length(times_interest)),
  time_days  = rep(times_interest, times = 2),
  survival   = c(surv_patient1, surv_patient2)
)

prob_table$prob_death <- 1 - prob_table$survival
prob_table
