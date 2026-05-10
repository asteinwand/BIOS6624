library(hdrm)
library(glmnet)
library(future)
library(future.apply)
library(tidyverse)



# Check available cores to use for simulation
parallel::detectCores()
N_CORES <- 12


######## Set up Simulation Parameters ##############


# True coefficients: 5 nonzero, 15 zero
# Project spec: 0.5/3, 1/3, 1.5/3, 2.0/3, 2.5/3
# Shell file used 1/6, 2/6, 3/6, 4/6, 5/6 — identical values
BETAS <- c(0.5/3, 1.0/3, 1.5/3, 2.0/3, 2.5/3, rep(0, 15))

# Simulation profile grid: all combinations of N and rho
profiles <- expand.grid(
  N   = c(250, 500),
  rho = c(0, 0.35, 0.70)
)
profiles$case <- with(profiles, paste0(
  "N=", N, ", rho=", rho
))
profiles

# Number of simulations
# Power target: worst-case coverage Ep=0.5, desired SE = 0.01
Ep  <- 0.5
SEr <- 0.01
NSIM <- ceiling(Ep * (1 - Ep) / SEr^2)



### Helper function to extract per-variable results from a fitted `lm` object

# mod_obj : lm object from the FINAL (reduced) model
# param   : vector of true betas (length p, in variable order V01-V20)
# alpha   : significance level for coverage and error metrics

harvest <- function(mod_obj, param, alpha = 0.05) {
  
  p <- length(param)
  var_names <- sprintf("V%02d", 1:p)
  
  # Coefficient table and CIs from the final model
  coef_tbl <- as.data.frame(summary(mod_obj)$coefficients)
  coef_tbl$param <- row.names(coef_tbl)
  
  ci_tbl <- as.data.frame(confint.default(mod_obj, level = 1 - alpha))
  names(ci_tbl) <- c("LCL", "UCL")
  ci_tbl$param  <- row.names(ci_tbl)
  
  # Merge coefficient estimates for selected variables (exclude intercept)
  selected_vars <- coef_tbl$param[coef_tbl$param != "(Intercept)"]
  sig_vars      <- coef_tbl$param[coef_tbl$param != "(Intercept)" &
                                    coef_tbl[, 4] < alpha]
  
  # Build variable-level summary
  res <- data.frame(
    variables     = var_names,
    true_values   = param,
    true_non_zero = as.integer(var_names %in% sprintf("V%02d", 1:5)),
    selected      = as.integer(var_names %in% selected_vars),
    signif        = as.integer(var_names %in% sig_vars),
    stringsAsFactors = FALSE
  )
  
  # Merge CIs (only for selected variables)
  res <- merge(res, ci_tbl[ci_tbl$param != "(Intercept)", ],
               by.x = "variables", by.y = "param", all.x = TRUE)
  
  # Merge point estimates
  coef_est <- coef_tbl[coef_tbl$param != "(Intercept)",
                       c("param", "Estimate")]
  res <- merge(res, coef_est,
               by.x = "variables", by.y = "param", all.x = TRUE)
  
  # Coverage: 1 if CI contains true value, 0 otherwise
  res$covered <- as.integer(res$LCL <= res$true_values &
                              res$true_values <= res$UCL)
  
  # For non-selected TRUE predictors: coverage = 0 (CI is empty / implied 0)
  res$covered[is.na(res$covered) & res$selected == 0 & res$true_non_zero == 1] <- 0
  
  # For non-selected NULL predictors: coverage = 1 (true value 0 is "in" the empty CI)
  res$covered[is.na(res$covered) & res$selected == 0 & res$true_non_zero == 0] <- 1
  
  # For non-selected variables, impute estimate = 0 (standard convention)
  res$Estimate[is.na(res$Estimate)] <- 0
  
  # Bias = estimate - true value  (computed here; averaged across reps later)
  res$bias <- res$Estimate - res$true_values
  
  return(res[order(res$variables), ])
}



## Second helper function for Lasso/elastic net
# dat      : data frame with columns y, V01, ..., V20
# alpha_en : elastic net mixing parameter (1 = lasso, 0 = ridge)
# rule     : "min" (lambda.min) or "1se" (lambda.1se)
# Returns  : lm object refitted on selected variables

fit_penalized <- function(dat, alpha_en = 1, rule = "min") {
  
  X <- as.matrix(dat[, -1])  # predictors
  y <- dat$y
  
  cv_fit <- cv.glmnet(X, y, alpha = alpha_en, standardize = TRUE,
                      nfolds = 10, family = "gaussian")
  
  lambda_choice <- if (rule == "min") cv_fit$lambda.min else cv_fit$lambda.1se
  
  coefs <- coef(cv_fit, s = lambda_choice)
  # Variable names with nonzero coefficients (excluding intercept)
  selected <- rownames(coefs)[coefs[, 1] != 0 & rownames(coefs) != "(Intercept)"]
  
  if (length(selected) == 0) {
    # If nothing selected, fit intercept-only model as a placeholder
    return(lm(y ~ 1, data = dat))
  }
  
  # Refit OLS on selected variables for inference
  form <- as.formula(paste("y ~", paste(selected, collapse = " + ")))
  lm(form, data = dat)
}


###################### Core Simulation Function ##############################
simfunc <- function(n, rho) {
  
  param <- BETAS
  
  # ---- Generate data -------------------------------------------------------
  dat_obj <- hdrm::gen_data(
    n      = n,
    p      = 20,
    p1     = 5,
    beta   = param,
    family = "gaussian",
    corr   =  "exchangeable",
    rho    = rho
  )
  dat <- data.frame(y = dat_obj$y, dat_obj$X)
  # Ensure column names match V01..V20
  names(dat)[-1] <- sprintf("V%02d", 1:20)
  
  # ---- Saturated OLS model for stepwise ------------------------------------
  sat_model <- lm(y ~ ., data = dat)
  
  # ---- 1. Backward selection by p-value (F-test, α = 0.05) ----------------
  pval_redux <- step(sat_model,
                     direction = "backward",
                     trace     = 0,
                     k         = qchisq(1 - 0.05, 1))
  pval_res         <- harvest(pval_redux, param = param)
  pval_res$method  <- "PVAL"
  
  # ---- 2. AIC backward selection -------------------------------------------
  aic_redux <- step(sat_model,
                    direction = "backward",
                    trace     = 0,
                    k         = 2)
  aic_res         <- harvest(aic_redux, param = param)
  aic_res$method  <- "AIC"
  
  # ---- 3. BIC backward selection -------------------------------------------
  bic_redux <- step(sat_model,
                    direction = "backward",
                    trace     = 0,
                    k         = log(n))
  bic_res         <- harvest(bic_redux, param = param)
  bic_res$method  <- "BIC"
  
  # ---- 4. Lasso – lambda.min -----------------------------------------------
  lasso_min_fit  <- fit_penalized(dat, alpha_en = 1, rule = "min")
  lasso_min_res  <- harvest(lasso_min_fit, param = param)
  lasso_min_res$method <- "LASSO_min"
  
  # ---- 5. Lasso – lambda.1se -----------------------------------------------
  lasso_1se_fit  <- fit_penalized(dat, alpha_en = 1, rule = "1se")
  lasso_1se_res  <- harvest(lasso_1se_fit, param = param)
  lasso_1se_res$method <- "LASSO_1se"
  
  # ---- 6. Elastic net (alpha=0.5) – lambda.min -----------------------------
  en_min_fit  <- fit_penalized(dat, alpha_en = 0.5, rule = "min")
  en_min_res  <- harvest(en_min_fit, param = param)
  en_min_res$method <- "EN05_min"
  
  # ---- 7. Elastic net (alpha=0.5) – lambda.1se -----------------------------
  en_1se_fit  <- fit_penalized(dat, alpha_en = 0.5, rule = "1se")
  en_1se_res  <- harvest(en_1se_fit, param = param)
  en_1se_res$method <- "EN05_1se"
  
  # ---- Combine and tag with simulation settings ----------------------------
  out <- rbind(pval_res, aic_res, bic_res,
               lasso_min_res, lasso_1se_res,
               en_min_res, en_1se_res)
  out$n   <- n
  out$rho <- rho
  
  return(out)
}


# Wrapper: run NSIM iterations for one profile
spin <- function(nsim = NSIM, profile) {
  
  stopifnot(nrow(profile) == 1)
  
  res <- vector("list", nsim)
  for (iter in seq_len(nsim)) {
    tmp       <- simfunc(n = profile[["N"]], rho = profile[["rho"]])
    tmp$iter  <- iter
    res[[iter]] <- tmp
  }
  
  do.call(rbind, res)
}



####################### Running the simulation ###############################
n_profiles <- nrow(profiles)

# Set up parallel backend
plan(multisession, workers = N_CORES)

set.seed(42) # the meaning of life

cat("Starting simulation:", Sys.time(), "\n")
cat("Profiles:", n_profiles, " | Reps per profile:", NSIM,
    " | Cores:", N_CORES, "\n\n")

system.time({
  simres_list <- future_lapply(
    seq_len(n_profiles),
    function(i) {
      spin(nsim = NSIM, profile = profiles[i, ])
    },
    future.seed = TRUE
  )
})

# Shut down workers
plan(sequential)

# Combine results
simres <- do.call(rbind, simres_list)
simres$case <- paste0("N=", simres$n, ", rho=", simres$rho)

cat("Simulation complete:", Sys.time(), "\n")
cat("Total rows:", nrow(simres), "\n")

# Save to disk
saveRDS(simres, "simres.rds")






















































