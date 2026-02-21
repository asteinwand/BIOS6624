library(bayesplot)
library(posterior)
library(bayestestR)
library(mcmcse)
library(loo)
library(MASS)
library(knitr)
library(kableExtra)
library(emmeans)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lme4)
library(grid)

hivdat <gridhivdat <- read.csv("C:/Users/stein/OneDrive/Documents/School/2026 Spring/Advanced Methods/BIOS6624/Project 1/Data Raw/hiv_6624_final.csv")


#####################################################################
# TABLE 1
####################################################################

# Creating graphics to visualize them

# Viral load
mean_vl <- tapply(hivdat$VLOAD,
                  list(hivdat$year, hivdat$hard_drugs),
                  mean)

matplot(as.numeric(rownames(mean_vl)), mean_vl,
        type = "l", lwd = 2, lty = 1,
        col = c("cornflowerblue", "red"),
        xlab = "Years Since HAART Initiation",
        ylab = "Mean Viral Load",
        main = "Viral Load Over Time by Drug Use")

legend("right", inset = -0.30, legend = colnames(mean_vl),
       col = c("cornflowerblue", "red"), lwd = 2, xpd = TRUE)



# CD4 
mean_cd4 <- tapply(hivdat$LEU3N,
                  list(hivdat$year, hivdat$hard_drugs),
                  mean)

matplot(as.numeric(rownames(mean_cd4)), mean_cd4,
        type = "l", lwd = 2, lty = 1,
        col = c("aquamarine2", "red"),
        xlab = "Years Since HAART Initiation",
        ylab = "Mean CD4 Count",
        main = "CD4 Count Over Time by Drug Use")

legend("right", inset = -0.30, legend = colnames(mean_cd4),
       col = c("aquamarine2", "red"), lwd = 2, xpd = TRUE)


# Physical Quality of Life Score
mean_pqol <- tapply(hivdat$AGG_PHYS,
                   list(hivdat$year, hivdat$hard_drugs),
                   mean)

matplot(as.numeric(rownames(mean_pqol)), mean_pqol,
        type = "l", lwd = 2, lty = 1,
        col = c("darkorchid", "red"),
        xlab = "Years Since HAART Initiation",
        ylab = "Mean Physical Quality of Life",
        main = "Physical Quality of Life Over Time by Drug Use")

legend("right", inset = -0.30, legend = colnames(mean_pqol),
       col = c("darkorchid", "red"), lwd = 2, xpd = TRUE)


# Mental Quality of Life Score
mean_mqol <- tapply(hivdat$AGG_MENT,
                    list(hivdat$year, hivdat$hard_drugs),
                    mean)

matplot(as.numeric(rownames(mean_mqol)), mean_mqol,
        type = "l", lwd = 2, lty = 1,
        col = c("deeppink4", "red"),
        xlab = "Years Since HAART Initiation",
        ylab = "Mean Mental Quality of Life",
        main = "Mental Quality of Life Over Time by Drug Use")

legend("right", inset = -0.30, legend = colnames(mean_mqol),
       col = c("deeppink4", "red"), lwd = 2, xpd = TRUE)


# Frequentist Analysis

# Prelim Data Preparation

# subject number tracking
n_start <- nrow(hivdat)

# fixing covariates
#BMI
n_bad_bmi <- sum(hivdat$BMI < 0 | hivdat$BMI > 250, na.rm = TRUE)
hivdat$BMI[hivdat$BMI < 0 | hivdat$BMI > 250] <- NA

# COnverting  to a factor for analysis
hivdat$hard_drugs <- factor(hivdat$hard_drugs,
                            levels = c(0,1),
                            labels = c("No Hard Drugs", "Hard Drug User"))
hivdat$SMOKE   <- factor(hivdat$SMOKE)
hivdat$EDUCBAS <- factor(hivdat$EDUCBAS)
hivdat$RACE    <- factor(hivdat$RACE)
hivdat$ADH     <- factor(hivdat$ADH)



# Creating baseline variables for the analysis
baseline_drugs <- hivdat[hivdat$year == 0, c("newid", "hard_drugs")]
names(baseline_drugs)[2] <- "hard_drugs_baseline"
hivdat <- merge(hivdat, baseline_drugs, by = "newid", all.x = TRUE)

# Pull Year 0 covariates and baseline outcome values
# rename outcomes to _base so don't get confusing
dat_yr0 <- hivdat[hivdat$year == 0,
                  c("newid", "hard_drugs_baseline",
                    "age", "BMI", "SMOKE", "EDUCBAS", "RACE",
                    "VLOAD", "LEU3N", "AGG_PHYS", "AGG_MENT")]

names(dat_yr0)[names(dat_yr0) == "VLOAD"]    <- "VLOAD_base"
names(dat_yr0)[names(dat_yr0) == "LEU3N"]    <- "LEU3N_base"
names(dat_yr0)[names(dat_yr0) == "AGG_PHYS"] <- "AGG_PHYS_base"
names(dat_yr0)[names(dat_yr0) == "AGG_MENT"] <- "AGG_MENT_base"

# Year 2: keep only the id and outcome columns
dat_yr2 <- hivdat[hivdat$year == 2,
                  c("newid", "VLOAD", "LEU3N", "AGG_PHYS", "AGG_MENT", "ADH")]

# Verify no overlapping names before merging (should only show "newid")
intersect(names(dat_yr2), names(dat_yr0))

# Now merge into one usable set
# Continue tracking the n values for analysis
n_yr0 <- nrow(dat_yr0)
n_yr2 <- nrow(dat_yr2)
analytic_all <- merge(dat_yr2, dat_yr0, by = "newid")
n_both <- nrow(analytic_all)
n_unmatched <- (n_yr0 + n_yr2) - (2 * n_both)


# Remove the missing data 
n_before_missing <- nrow(analytic_all)
analytic <- analytic_all[complete.cases(analytic_all), ] # removes any missing
n_after_missing <- nrow(analytic)
n_missing <- n_before_missing - n_after_missing

#Reset factors
analytic$SMOKE   <- droplevels(factor(analytic$SMOKE))
analytic$EDUCBAS <- droplevels(factor(analytic$EDUCBAS))
analytic$RACE    <- droplevels(factor(analytic$RACE))
analytic$ADH     <- droplevels(factor(analytic$ADH))
analytic$hard_drugs_baseline <- droplevels(factor(analytic$hard_drugs_baseline))



# Determining what needs to be log transformed with diagnostic plots
# Creating a function to do it for each variable rather than individually

# histogram + density + QQ for one variable
check_distribution <- function(x, label, color) {
  x_clean <- x[!is.na(x)]
  par(mfrow = c(1, 2))
  
  # Histogram with density overlay
  hist(x_clean,
       freq  = FALSE,
       main  = paste("Histogram:", label),
       xlab  = label,
       col   = adjustcolor(color, alpha.f = 0.5),
       border = "white")
  lines(density(x_clean), col = color, lwd = 2)
  
  # QQ plot
  qqnorm(x_clean,
         main = paste("QQ Plot:", label),
         pch  = 16, cex = 0.7,
         col  = adjustcolor(color, alpha.f = 0.6))
  qqline(x_clean, col = "black", lwd = 2, lty = 2)
  
  par(mfrow = c(1, 1))
  
  # Numeric skewness and excess kurtosis
  n    <- length(x_clean)
  mn   <- mean(x_clean)
  s    <- sd(x_clean)
  skew <- (sum((x_clean - mn)^3) / n) / s^3
  kurt <- (sum((x_clean - mn)^4) / n) / s^4 - 3
  cat(sprintf("%-30s  Skewness: %6.3f  Excess Kurtosis: %6.3f  N: %d\n",
              label, skew, kurt, n))
}



# Checking for each variable using similar colors as above

cat("=== VIRAL LOAD ===\n")
check_distribution(analytic$VLOAD,        "Viral Load (raw)",   "steelblue")
analytic$log10_VLOAD      <- log10(analytic$VLOAD + 1)
analytic$log10_VLOAD_base <- log10(analytic$VLOAD_base + 1)
check_distribution(analytic$log10_VLOAD,  "Viral Load (log10)", "steelblue")

# --- CD4 Count: raw vs log ---
cat("\n=== CD4 COUNT ===\n")
check_distribution(analytic$LEU3N,        "CD4 Count (raw)",    "aquamarine4")
analytic$log_LEU3N      <- log(analytic$LEU3N)
analytic$log_LEU3N_base <- log(analytic$LEU3N_base)
check_distribution(analytic$log_LEU3N,    "CD4 Count (log)",    "aquamarine4")

# --- Physical QoL (check raw only first) ---
cat("\n=== PHYSICAL QoL ===\n")
check_distribution(analytic$AGG_PHYS,     "Physical QoL (raw)", "darkorchid3")

# --- Mental QoL ---
cat("\n=== MENTAL QoL ===\n")
check_distribution(analytic$AGG_MENT,     "Mental QoL (raw)",   "deeppink4")


# From these use log_viral load, regular CD4, and log both mental/physical QoL


# Convert categoricals to factors in the analytic dataset
analytic$SMOKE   <- factor(analytic$SMOKE)
analytic$EDUCBAS <- factor(analytic$EDUCBAS)
analytic$RACE    <- factor(analytic$RACE)
analytic$ADH     <- factor(analytic$ADH)




# Frequentist models

# a is WITHOUT adherence
# b is WITH adherence

# Viral Load (log10 transformed) 
mod1a <- lm(log10(VLOAD) ~ hard_drugs_baseline + log10(VLOAD_base) +
             age + BMI + SMOKE + EDUCBAS + RACE,
           data = analytic)
mod1b <- lm(log10(VLOAD) ~ hard_drugs_baseline + log10(VLOAD_base) +
              age + BMI + SMOKE + EDUCBAS + RACE + ADH,
            data = analytic)
summary(mod1)
par(mfrow = c(2, 2)); plot(mod1); par(mfrow = c(1, 1))

#  CD4 Count (untransformed)
mod2a <- lm(LEU3N ~ hard_drugs_baseline + LEU3N_base +
             age + BMI + SMOKE + EDUCBAS + RACE,
           data = analytic)
mod2b <- lm(LEU3N ~ hard_drugs_baseline + LEU3N_base +
             age + BMI + SMOKE + EDUCBAS + RACE + ADH,
           data = analytic)
summary(mod2)
par(mfrow = c(2, 2)); plot(mod2); par(mfrow = c(1, 1))

# Physical QoL (reflected log)
mod3a <- lm(log(101 - AGG_PHYS) ~ hard_drugs_baseline + log(101 - AGG_PHYS_base) +
             age + BMI + SMOKE + EDUCBAS + RACE,
           data = analytic)
mod3b <- lm(log(101 - AGG_PHYS) ~ hard_drugs_baseline + log(101 - AGG_PHYS_base) +
             age + BMI + SMOKE + EDUCBAS + RACE + ADH,
           data = analytic)
summary(mod3)
par(mfrow = c(2, 2)); plot(mod3); par(mfrow = c(1, 1))

# Mental QoL (reflected log) 
mod4a <- lm(log(101 - AGG_MENT) ~ hard_drugs_baseline + log(101 - AGG_MENT_base) +
             age + BMI + SMOKE + EDUCBAS + RACE,
           data = analytic)
mod4b <- lm(log(101 - AGG_MENT) ~ hard_drugs_baseline + log(101 - AGG_MENT_base) +
             age + BMI + SMOKE + EDUCBAS + RACE + ADH,
           data = analytic)
summary(mod4)
par(mfrow = c(2, 2)); plot(mod4); par(mfrow = c(1, 1))


# Summary of all 4 frequentist models

# extract results from one model function
extract_results <- function(mod, outcome_label) {
  coef_row <- summary(mod)$coefficients["hard_drugs_baselineHard Drug User", ]
  ci_row   <- confint(mod)["hard_drugs_baselineHard Drug User", ]
  data.frame(
    Outcome   = outcome_label,
    Estimate  = round(coef_row[1], 3),
    SE        = round(coef_row[2], 3),
    p         = round(coef_row[4], 4),
    CI_Lower  = round(ci_row[1], 3),
    CI_Upper  = round(ci_row[2], 3)
  )
}

results_table <- rbind(
  extract_results(mod1a, "Viral Load (log10) - No ADH"),
  extract_results(mod1b, "Viral Load (log10) - With ADH"),
  extract_results(mod2a, "CD4 Count - No ADH"),
  extract_results(mod2b, "CD4 Count - With ADH"),
  extract_results(mod3a, "Physical QoL (refl log) - No ADH"),
  extract_results(mod3b, "Physical QoL (refl log) - With ADH"),
  extract_results(mod4a, "Mental QoL (refl log) - No ADH"),
  extract_results(mod4b, "Mental QoL (refl log) - With ADH")
)

# Format p-values so very small ones display nicely
results_table$p <- ifelse(results_table$p < 0.001, "<0.001",
                          as.character(results_table$p))

# Combine CI into one column for cleaner table
results_table$`95% CI` <- paste0("(", results_table$CI_Lower,
                                 ", ", results_table$CI_Upper, ")")
results_table$CI_Lower <- NULL
results_table$CI_Upper <- NULL

kable(results_table,
      row.names = FALSE,
      caption   = "Effect of Baseline Hard Drug Use on Year 2 Outcomes: With and Without Adherence Adjustment",
      booktabs  = TRUE,
      align     = c("l", "r", "r", "r", "r")) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width    = FALSE) %>%
  pack_rows("Viral Load (log10)", 1, 2) %>%
  pack_rows("CD4 Count", 3, 4) %>%
  pack_rows("Physical Quality of Life (reflected log)", 5, 6) %>%
  pack_rows("Mental Quality of Life (reflected log)", 7, 8)


### ADH DID NOT MAKE A DIFFERENCE ### REMOVED FROM FURTHER ANALYSIS


# Estimates for log-transformed outcomes represent differences on the log scale. 
# For viral load, exp(estimate) gives the fold-change in copies/mL.", 
# For QoL outcomes, positive estimates indicate worse quality of life due to reflection 
# of scale.

# Bayesian Analysis
# Used Gleason Worksheet as a template

# Dependencies 
library(cmdstanr)
library(bayesplot)  # diagnostic plots of the MCMC chains
library(posterior)  # for summarizing posterior draws
library(bayestestR) # for calculating highest density posterior intervals
library(mcmcse)     # for calculating MCMCSE's
library(loo)        # for getting model fit statistics (WAIC and LOO-IC)
library(dplyr)
library(tibble)

###########################################################################
# STEP 1: Define the Stan model
# This is a general linear regression with half-normal prior on sigma
# and normal priors on regression coefficients
###########################################################################

stan_file <- write_stan_file("data {
  int<lower=0> N;                  // number of observations
  int<lower=0> P;                  // number of predictors including intercept
  matrix[N, P] X;                  // design matrix (first column = intercept)
  vector[N] y;                     // outcome

  vector[P] prior_mean;            // prior means for each beta
  vector<lower=0>[P] prior_sd;     // prior SDs for each beta

  real<lower=0> sigma_prior_sd;    // SD for half-normal prior on sigma
}

parameters {
  vector[P] beta;                  // regression coefficients
  real<lower=0> sigma;             // residual SD
}

model {
  // Vectorized priors for regression coefficients
  beta ~ normal(prior_mean, prior_sd);

  // Half-normal prior for sigma
  sigma ~ normal(0, sigma_prior_sd);

  // Likelihood
  y ~ normal(X * beta, sigma);
}

generated quantities {
  // log likelihood for each observation for calculating model fit stats
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | X[n] * beta, sigma);
  }
}", dir="STAN", basename='linear_regression_half_normal')



###########################################################################
# STEP 2: Compile the Stan program
###########################################################################

mod <- cmdstan_model('STAN/linear_regression_half_normal.stan')

###########################################################################
# MODEL 1: VIRAL LOAD (log10)
###########################################################################

# Outcome data
y <- log10(analytic$VLOAD)

# Design matrix
X <- model.matrix(~ hard_drugs_baseline + log10(VLOAD_base) +
                    age + BMI + SMOKE + EDUCBAS + RACE, 
                  data = analytic)

N <- nrow(X)
P <- ncol(X)

# Priors: N(0, 2) for coefficients, half-Normal(0, 5) for sigma
m <- c(mean(y), rep(0, P - 1))  # intercept gets outcome mean, rest get 0
s <- c(10, 2, rep(2, P - 2))    # intercept gets 10, rest get 2
sigma_sd <- 5

# Data list for Stan
data_vload <- list(
  N = N,
  P = P,
  X = X,
  y = y,
  prior_mean = m,
  prior_sd = s,
  sigma_prior_sd = sigma_sd
)

# Fit model: 4 chains, 1000 warmup + 1000 sampling = 4000 post-warmup draws
fit_vload <- mod$sample(
  data = data_vload,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 123
)

# Posterior summary
fit_vload$summary(variables = c("beta[1]", "beta[2]", "sigma"))

# Detailed summary table
draws_vload <- fit_vload$draws()
draws_mat_vload <- as_draws_matrix(draws_vload)
params_vload <- colnames(draws_mat_vload)
params_vload <- params_vload[!grepl("lp__|log_lik", params_vload)]

summary_vload <- lapply(params_vload, function(p) {
  vals <- as.numeric(draws_mat_vload[, p])
  mcse_val <- mcmcse::mcse(vals)$se
  ess_val <- ess_bulk(vals)
  hpd <- hdi(vals, ci = 0.95)
  
  tibble(
    Parameter = p,
    Estimate  = mean(vals),
    MCSE      = mcse_val,
    Std_Dev   = sd(vals),
    HPDI_2.5  = hpd$CI_low,
    HPDI_97.5 = hpd$CI_high,
    ESS       = ess_val,
    Rhat      = rhat(vals)
  )
}) %>% bind_rows()

print(summary_vload)

# Check diagnostics
cat("\nMCSE < 6% of SD:", all((100 * summary_vload$MCSE / summary_vload$Std_Dev) < 6), "\n")
cat("ESS > 1000:", all(summary_vload$ESS > 1000), "\n")
cat("Rhat < 1.01:", all(summary_vload$Rhat < 1.01), "\n")

# Model fit statistics
loglik_vload <- as_draws_matrix(fit_vload$draws("log_lik"))
loo_vload <- loo(loglik_vload)
waic_vload <- waic(loglik_vload)


print(waic_vload)
print(loo_vload)



# MCMC Diagnostics
draws_array_vload <- as_draws_array(fit_vload$draws())
params_diag <- c("beta[1]", "beta[2]", "sigma")

mcmc_trace(draws_array_vload, pars = params_diag)
mcmc_dens_overlay(draws_array_vload, pars = params_diag)
mcmc_acf(draws_array_vload, pars = params_diag)

fit_vload$cmdstan_diagnose()



###########################################################################
# MODEL 2: CD4 COUNT
###########################################################################

# Outcome data
y <- analytic$LEU3N

# Design matrix
X <- model.matrix(~ hard_drugs_baseline + LEU3N_base +
                    age + BMI + SMOKE + EDUCBAS + RACE, 
                  data = analytic)

N <- nrow(X)
P <- ncol(X)

# Priors: N(0, 100) for coefficients, half-Normal(0, 5) for sigma
m <- c(mean(y), rep(0, P - 1))
s <- c(200, 100, rep(100, P - 2))  # intercept gets 200, rest get 100
sigma_sd <- 5

data_cd4 <- list(
  N = N, P = P, X = X, y = y,
  prior_mean = m, prior_sd = s,
  sigma_prior_sd = sigma_sd
)

fit_cd4 <- mod$sample(
  data = data_cd4,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 123
)

# Posterior summary
fit_cd4$summary(variables = c("beta[1]", "beta[2]", "sigma"))

# Detailed summary table
draws_cd4 <- fit_cd4$draws()
draws_mat_cd4 <- as_draws_matrix(draws_cd4)
params_cd4 <- colnames(draws_mat_cd4)
params_cd4 <- params_cd4[!grepl("lp__|log_lik", params_cd4)]

summary_cd4 <- lapply(params_cd4, function(p) {
  vals <- as.numeric(draws_mat_cd4[, p])
  mcse_val <- mcmcse::mcse(vals)$se
  ess_val <- ess_bulk(vals)
  hpd <- hdi(vals, ci = 0.95)
  
  tibble(
    Parameter = p,
    Estimate  = mean(vals),
    MCSE      = mcse_val,
    Std_Dev   = sd(vals),
    HPDI_2.5  = hpd$CI_low,
    HPDI_97.5 = hpd$CI_high,
    ESS       = ess_val,
    Rhat      = rhat(vals)
  )
}) %>% bind_rows()

print(summary_cd4)

# Check diagnostics
cat("\nMCSE < 6% of SD:", all((100 * summary_cd4$MCSE / summary_cd4$Std_Dev) < 6), "\n")
cat("ESS > 1000:", all(summary_cd4$ESS > 1000), "\n")
cat("Rhat < 1.01:", all(summary_cd4$Rhat < 1.01), "\n")

# Model fit statistics
loglik_cd4 <- as_draws_matrix(fit_cd4$draws("log_lik"))
loo_cd4 <- loo(loglik_cd4)
waic_cd4 <- waic(loglik_cd4)


print(waic_cd4)
print(loo_cd4)



# MCMC Diagnostics
draws_array_cd4 <- as_draws_array(fit_cd4$draws())
params_diag <- c("beta[1]", "beta[2]", "sigma")

mcmc_trace(draws_array_cd4, pars = params_diag)
mcmc_dens_overlay(draws_array_cd4, pars = params_diag)
mcmc_acf(draws_array_cd4, pars = params_diag)

fit_cd4$cmdstan_diagnose()



###########################################################################
# MODEL 3: PHYSICAL QOL (reflected log)
###########################################################################

# Outcome data
y <- log(101 - analytic$AGG_PHYS)

# Design matrix
X <- model.matrix(~ hard_drugs_baseline + log(101 - AGG_PHYS_base) +
                    age + BMI + SMOKE + EDUCBAS + RACE, 
                  data = analytic)

N <- nrow(X)
P <- ncol(X)

# Priors: N(0, 2) for coefficients, half-Normal(0, 5) for sigma
m <- c(mean(y), rep(0, P - 1))
s <- c(10, 2, rep(2, P - 2))
sigma_sd <- 5

data_phys <- list(
  N = N, P = P, X = X, y = y,
  prior_mean = m, prior_sd = s,
  sigma_prior_sd = sigma_sd
)

fit_phys <- mod$sample(
  data = data_phys,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 123
)

# Posterior summary
fit_phys$summary(variables = c("beta[1]", "beta[2]", "sigma"))

# Detailed summary table
draws_phys <- fit_phys$draws()
draws_mat_phys <- as_draws_matrix(draws_phys)
params_phys <- colnames(draws_mat_phys)
params_phys <- params_phys[!grepl("lp__|log_lik", params_phys)]

summary_phys <- lapply(params_phys, function(p) {
  vals <- as.numeric(draws_mat_phys[, p])
  mcse_val <- mcmcse::mcse(vals)$se
  ess_val <- ess_bulk(vals)
  hpd <- hdi(vals, ci = 0.95)
  
  tibble(
    Parameter = p,
    Estimate  = mean(vals),
    MCSE      = mcse_val,
    Std_Dev   = sd(vals),
    HPDI_2.5  = hpd$CI_low,
    HPDI_97.5 = hpd$CI_high,
    ESS       = ess_val,
    Rhat      = rhat(vals)
  )
}) %>% bind_rows()

print(summary_phys)

# Check diagnostics
cat("\nMCSE < 6% of SD:", all((100 * summary_phys$MCSE / summary_phys$Std_Dev) < 6), "\n")
cat("ESS > 1000:", all(summary_phys$ESS > 1000), "\n")
cat("Rhat < 1.01:", all(summary_phys$Rhat < 1.01), "\n")

# Model fit statistics
loglik_phys <- as_draws_matrix(fit_phys$draws("log_lik"))
loo_phys <- loo(loglik_phys)
waic_phys <- waic(loglik_phys)


print(waic_phys)
print(loo_phys)



# MCMC Diagnostics
draws_array_phys <- as_draws_array(fit_phys$draws())
params_diag <- c("beta[1]", "beta[2]", "sigma")

mcmc_trace(draws_array_phys, pars = params_diag)
mcmc_dens_overlay(draws_array_phys, pars = params_diag)
mcmc_acf(draws_array_phys, pars = params_diag)

fit_phys$cmdstan_diagnose()



###########################################################################
# MODEL 4: MENTAL QOL (reflected log)
###########################################################################

# Outcome data
y <- log(101 - analytic$AGG_MENT)

# Design matrix
X <- model.matrix(~ hard_drugs_baseline + log(101 - AGG_MENT_base) +
                    age + BMI + SMOKE + EDUCBAS + RACE, 
                  data = analytic)

N <- nrow(X)
P <- ncol(X)

# Priors: N(0, 2), half-Normal(0, 5)
m <- c(mean(y), rep(0, P - 1))
s <- c(10, 2, rep(2, P - 2))
sigma_sd <- 5

data_ment <- list(
  N = N, P = P, X = X, y = y,
  prior_mean = m, prior_sd = s,
  sigma_prior_sd = sigma_sd
)

fit_ment <- mod$sample(
  data = data_ment,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 123
)

# Posterior summary
fit_ment$summary(variables = c("beta[1]", "beta[2]", "sigma"))

# Detailed summary table
draws_ment <- fit_ment$draws()
draws_mat_ment <- as_draws_matrix(draws_ment)
params_ment <- colnames(draws_mat_ment)
params_ment <- params_ment[!grepl("lp__|log_lik", params_ment)]

summary_ment <- lapply(params_ment, function(p) {
  vals <- as.numeric(draws_mat_ment[, p])
  mcse_val <- mcmcse::mcse(vals)$se
  ess_val <- ess_bulk(vals)
  hpd <- hdi(vals, ci = 0.95)
  
  tibble(
    Parameter = p,
    Estimate  = mean(vals),
    MCSE      = mcse_val,
    Std_Dev   = sd(vals),
    HPDI_2.5  = hpd$CI_low,
    HPDI_97.5 = hpd$CI_high,
    ESS       = ess_val,
    Rhat      = rhat(vals)
  )
}) %>% bind_rows()

print(summary_ment)

# Check diagnostics
cat("\nMCSE < 6% of SD:", all((100 * summary_ment$MCSE / summary_ment$Std_Dev) < 6), "\n")
cat("ESS > 1000:", all(summary_ment$ESS > 1000), "\n")
cat("Rhat < 1.01:", all(summary_ment$Rhat < 1.01), "\n")

# Model fit statistics
loglik_ment <- as_draws_matrix(fit_ment$draws("log_lik"))
loo_ment <- loo(loglik_ment)
waic_ment <- waic(loglik_ment)


print(waic_ment)
print(loo_ment)



# MCMC Diagnostics
draws_array_ment <- as_draws_array(fit_ment$draws())
params_diag <- c("beta[1]", "beta[2]", "sigma")

mcmc_trace(draws_array_ment, pars = params_diag)
mcmc_dens_overlay(draws_array_ment, pars = params_diag)
mcmc_acf(draws_array_ment, pars = params_diag)

fit_ment$cmdstan_diagnose()



# Looking at Trace plots

# Model 1: Viral Load
draws_vload <- as_draws_array(fit_vload$draws())
params_trace <- c("beta[2]", "sigma")
mcmc_trace(draws_vload, pars = params_trace)

# Model 2: CD4
draws_cd4 <- as_draws_array(fit_cd4$draws())
mcmc_trace(draws_cd4, pars = params_trace)

# Model 3: Physical QoL
draws_phys <- as_draws_array(fit_phys$draws())
mcmc_trace(draws_phys, pars = params_trace)

# Model 4: Mental QoL
draws_ment <- as_draws_array(fit_ment$draws())
mcmc_trace(draws_ment, pars = params_trace)


## Table results of bayesian

# Extract hard drug coefficient (beta[2]) from each model
extract_bayesian_results <- function(fit, model_name) {
  draws_mat <- as_draws_matrix(fit$draws())
  
  # beta[2] is hard_drugs_baseline (adjust index if needed)
  drug_coef <- draws_mat[, "beta[2]"]
  
  # Calculate summary statistics
  hpd <- hdi(drug_coef, ci = 0.95)
  
  data.frame(
    Model = model_name,
    Estimate = mean(drug_coef),
    Std_Dev = sd(drug_coef),
    HPDI_2.5 = hpd$CI_low,
    HPDI_97.5 = hpd$CI_high,
    ESS = ess_bulk(drug_coef),
    Rhat = rhat(drug_coef)
  )
}

# Build table
bayesian_results <- rbind(
  extract_bayesian_results(fit_vload, "Viral Load (log10)"),
  extract_bayesian_results(fit_cd4, "CD4 Count"),
  extract_bayesian_results(fit_phys, "Physical QoL (refl log)"),
  extract_bayesian_results(fit_ment, "Mental QoL (refl log)")
)

# Round for display
bayesian_results$Estimate <- round(bayesian_results$Estimate, 3)
bayesian_results$Std_Dev <- round(bayesian_results$Std_Dev, 3)
bayesian_results$HPDI_2.5 <- round(bayesian_results$HPDI_2.5, 3)
bayesian_results$HPDI_97.5 <- round(bayesian_results$HPDI_97.5, 3)
bayesian_results$ESS <- round(bayesian_results$ESS, 0)
bayesian_results$Rhat <- round(bayesian_results$Rhat, 3)

# Combine HPDI into one column
bayesian_results$`95% HPDI` <- paste0("(", bayesian_results$HPDI_2.5, 
                                      ", ", bayesian_results$HPDI_97.5, ")")
bayesian_results$HPDI_2.5 <- NULL
bayesian_results$HPDI_97.5 <- NULL

# Create nice table
kable(bayesian_results,
      row.names = FALSE,
      caption = "Bayesian Results: Effect of Baseline Hard Drug Use on Year 2 Outcomes",
      booktabs = TRUE,
      align = c("l", "r", "r", "r", "r", "r")) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = FALSE) %>%
  footnote(general = "HPDI = Highest Posterior Density Interval. ESS = Effective Sample Size. Rhat < 1.01 indicates convergence.",
           general_title = "Note: ",
           footnote_as_chunk = TRUE)

print(bayesian_results)



