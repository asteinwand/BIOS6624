# Import Dataset

cort <- read.csv("C:/Users/stein/OneDrive/Documents/School/2026 Spring/Advanced Methods/BIOS6624/Project 0/DataRaw/Project0_Clean_v2.csv")


# Setting up libraries commonly used

library(MASS)
library(knitr)
library(emmeans)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lme4)
library(lmerTest)
library(nlme)
library(car)

# Data Cleaning/Inspection

colSums(is.na(cort)) # Cortisol..nmol.L. 5 missing 
                    # Booket..Clock.Time 0 missing 
                    # Booklet..Sample.interval.Decimal.Time..mins. 55 missing
                    # Booklet..Sample.interval 0 missing
                    # DHEA..nmol.L. 5 missing

sum(cort$Cortisol..nmol.L. >= 80, na.rm = T) # only 1 >= 80 
sum(cort$DHEA..nmol.L. >= 5.205, na.rm = T) # 6 >= 5.205


# Remove the missing values and the values outside range given

cort_clean <- cort[
  !is.na(cort$Cortisol..nmol.L.) &
    !is.na(cort$DHEA..nmol.L.) &
    cort$Cortisol..nmol.L. < 80 &
    cort$DHEA..nmol.L. < 5.205,
]

nrow(cort) - nrow(cort_clean) # 6 total rows removed

colSums(is.na(cort_clean)) # correctly removed missing cortisol and DHEA


# Boxplots to check for outliers

boxplot(cort_clean$Cortisol..nmol.L.,
        main = "Cortisol",
        ylab = "Cortisol (nmol/L)")

boxplot(cort_clean$DHEA..nmol.L.,
        main = "DHEA",
        ylab = "DHEA (nmol/L)")


# Histograms

hist(cort_clean$Cortisol..nmol.L.,
     breaks = 20,
     main = "Cortisol")

hist(cort_clean$DHEA..nmol.L.,
     breaks = 20,
     main = "DHEA")

###################### Research Question 1 ###############################


# Sleep wake time only recorded once per every 4 rows need to fill in missing ones

# Make sure Collection.Date is a Date, not a timestamp
cort_clean$Collection.Date <- as.Date(cort_clean$Collection.Date, format = "%m/%d/%Y")

# Missing Sleep diary times not reporting as NA so I can't fill missing values
cort_clean$Sleep.Diary.reported.wake.time[
  cort_clean$Sleep.Diary.reported.wake.time == ""
] <- NA

# Now can create the for loop to loop through the NAs and fill in missing sleep times

# Create a copy to fill
wake_filled <- cort_clean$Sleep.Diary.reported.wake.time

# Loop through each Subject × Date group
groups <- unique(cbind(cort_clean$SubjectID, cort_clean$Collection.Date))

for(i in seq_len(nrow(groups))) {
  
  # Find rows for this subject/day
  idx <- which(cort_clean$SubjectID == groups[i, 1] &
                 cort_clean$Collection.Date == groups[i, 2])
  
  # Extract the wake times for this group
  x <- wake_filled[idx]
  
  # Fill down: sample 1 → sample 2 → sample 3 → sample 4
  for(j in seq_along(x)) {
    if(is.na(x[j]) && j > 1) x[j] <- x[j - 1]
  }
  
  # Put filled values back
  wake_filled[idx] <- x
}

# Replace original column
cort_clean$Sleep.Diary.reported.wake.time <- wake_filled




# Convert our times to usable points and easier names

cort_clean$WakeTime <- as.POSIXct(cort_clean$Sleep.Diary.reported.wake.time,
                                  format = "%H:%M")

cort_clean$BookletTime <- as.POSIXct(cort_clean$Booket..Clock.Time,
                                     format = "%H:%M")

cort_clean$MEMsTime <- as.POSIXct(cort_clean$MEMs..Clock.Time,
                                  format = "%H:%M")




# Calculate the minutes since waking for both booklet and mems time

cort_clean$Booklet.MinutesSinceWaking <-
  as.numeric(difftime(cort_clean$BookletTime,
                      cort_clean$WakeTime,
                      units = "mins"))

cort_clean$MEMs.MinutesSinceWaking <-
  as.numeric(difftime(cort_clean$MEMsTime,
                      cort_clean$WakeTime,
                      units = "mins"))



# Create the LMM with our new variables and rand int for Subject

mod1 <- lmer(Booklet.MinutesSinceWaking ~ MEMs.MinutesSinceWaking +
               (1 | SubjectID), data = cort_clean)
summary(mod1)



ggplot(cort_clean, aes(x = MEMs.MinutesSinceWaking,
                       y = Booklet.MinutesSinceWaking,
                       color = SubjectID)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "MEMs Minutes Since Waking",
       y = "Booklet Minutes Since Waking",
       title = "Agreement Between Booklet and MEMs Sampling Times")



###################### Research Question 2 ###############################





