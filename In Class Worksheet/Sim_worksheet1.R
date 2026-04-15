
### 1 ###


set.seed(42)

n <- 100
n_sims <- 20

beta0 <- 1
beta1 <- 1
beta2 <- 0.5
beta3 <- 0.25

error <- 1
error_sd <- sqrt(error)


sim_datasets <- vector("list", n_sims)


for (i in 1:n_sims) {
  
  # X1: binary variable, 0 or 1, with equal probability (50/50)
  X1 <- sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
  
  # X2: age, normally distributed with mean = 39 and SD = 5
  X2 <- rnorm(n, mean = 39, sd = 5)
  
  # X3: interaction term = X1 multiplied by X2
  X3 <- X1 * X2
  
  # Error term: normally distributed with mean 0, SD = 1
  epsilon <- rnorm(n, mean = 0, sd = error_sd)
  
  # Generate outcome Y using the linear regression model:
  Y <- beta0 + beta1*X1 + beta2*X2 + beta3*X3 + epsilon
  
  # Store this dataset as a data frame in the list
  sim_datasets[[i]] <- data.frame(
    sim_id = i,   # track which simulation this belongs to
    Y  = Y,
    X1 = X1,
    X2 = X2,
    X3 = X3
  )
}

### 2 ###

library(ggplot2)

all_data <- do.call(rbind, sim_datasets)  # stack all 20 datasets

# convert to a factor for ggplot
all_data$sim_id <- factor(all_data$sim_id, labels = paste("Dataset", 1:20))

# convert X1 to a factor
all_data$X1 <- factor(all_data$X1, levels = c(0,1), labels = c("X1=0", "X1=1"))


ggplot(all_data, aes(x = X2, y = Y, color = X1)) +
  geom_point(alpha = 0.4, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  facet_wrap(~ sim_id, nrow = 4, ncol = 5) +
  scale_color_manual(values = c("X1=0" = "forestgreen", "X1=1" = "darkorchid"))







