library(bayesplot)
library(posterior)
library(bayestestR)
library(mcmcse)
library(loo)
library(MASS)
library(knitr)
library(emmeans)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lme4)

hivdat <- read.csv("C:/Users/stein/OneDrive/Documents/School/2026 Spring/Advanced Methods/BIOS6624/Project 1/Data Raw/hiv_6624_final.csv")

# Some Prelim Data Preparation

# COnverting hard drugs to a factor 
hivdat$hard_drugs <- factor(hivdat$hard_drugs,
                          levels = c(0,1),
                          labels = c("No Hard Drugs", "Hard Drug User"))

# Check for missingness

colSums(is.na(hivdat)) # AGG_MENT/AGG_PHYS 19 # VLOAD 222 # LEU3N 209 # Hard_drugs 33

# Missingness by year

tapply(is.na(hivdat$VLOAD), hivdat$year, sum) 
# 0  1  2  3  4  5  6  7  8 
# 42 30 19 22 26 28 19 21 15 

tapply(is.na(hivdat$LEU3N), hivdat$year, sum) 
#  0  1  2  3  4  5  6  7  8 
# 24 25 19 24 27 30 23 20 17 

tapply(is.na(hivdat$hard_drugs), hivdat$year, sum) 
# 0 1 2 3 4 5 6 7 8 
# 0 8 4 1 7 3 7 2 1 

# Removing the missing rows
missing <- is.na(hivdat$VLOAD) |
  is.na(hivdat$LEU3N) |
  is.na(hivdat$AGG_PHYS) |
  is.na(hivdat$AGG_MENT)

hivclean <- hivdat[!missing, ]


# Looking at descriptive statistics
baseline <- hivclean[hivclean$year == 0, ]

aggregate(cbind(VLOAD, LEU3N, AGG_PHYS, AGG_MENT) ~ hard_drugs,
          data = baseline,
          FUN = function(x) c(mean = mean(x), median = median(x)))


yr2 <- hivclean[hivclean$year == 2, ]
aggregate(cbind(VLOAD, LEU3N, AGG_PHYS, AGG_MENT) ~ hard_drugs,
          data = yr2,
          FUN = function(x) c(mean = mean(x), median = median(x)))

# Creating graphics to visualize them

# Viral load
mean_vl <- tapply(hivclean$VLOAD,
                  list(hivclean$year, hivclean$hard_drugs),
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
mean_cd4 <- tapply(hivclean$LEU3N,
                  list(hivclean$year, hivclean$hard_drugs),
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
mean_pqol <- tapply(hivclean$AGG_PHYS,
                   list(hivclean$year, hivclean$hard_drugs),
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
mean_mqol <- tapply(hivclean$AGG_MENT,
                    list(hivclean$year, hivclean$hard_drugs),
                    mean)

matplot(as.numeric(rownames(mean_mqol)), mean_mqol,
        type = "l", lwd = 2, lty = 1,
        col = c("deeppink4", "red"),
        xlab = "Years Since HAART Initiation",
        ylab = "Mean Mental Quality of Life",
        main = "Mental Quality of Life Over Time by Drug Use")

legend("right", inset = -0.30, legend = colnames(mean_mqol),
       col = c("deeppink4", "red"), lwd = 2, xpd = TRUE)






