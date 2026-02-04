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
  labs(x = "Cap Minutes Since Waking",
       y = "Booklet Minutes Since Waking",
       title = "Figure 1: Agreement Between Booklet and Cap Times")



###################### Research Question 2 ###############################

# creating a variable for the target minutes of 30 and 600 for adherence

cort_clean$ScheduledTime <- NA
cort_clean$ScheduledTime[cort_clean$Collection.Sample == 2] <- 30
cort_clean$ScheduledTime[cort_clean$Collection.Sample == 4] <- 600


# computing the deviation from the scheduled time

cort_clean$Deviation <- abs(cort_clean$Booklet.MinutesSinceWaking -
                              cort_clean$ScheduledTime)

# Classifying what our adherence levels are

cort_clean$Adherence <- cut(
  cort_clean$Deviation,
  breaks = c(-Inf, 7.5, 15, Inf),
  labels = c("Good", "Adequate", "Poor")
)

# Getting percent of each samples in each category
prop.table(table(cort_clean$Adherence)) * 100

# PErcent of subjects within each category
subject_adherence <- cort_clean %>%
  group_by(SubjectID) %>%
  summarize(
    good = mean(Deviation <= 7.5, na.rm = TRUE),
    adequate = mean(Deviation >7.5 & Deviation <= 15, na.rm = TRUE) 
  )


# Sample-level adherence percentages
sample_tab <- round(prop.table(table(cort_clean$Adherence)) * 100, 1)

sample_df <- data.frame(
  Adherence = names(sample_tab),
  Percent = as.numeric(sample_tab)
)

# Subject-level adherence (using existing subject_adherence object)
subject_df <- data.frame(
  Metric = c("Subjects 100% Good", "Subjects 100% Adequate"),
  Percent = c(
    mean(subject_adherence$good == 1) * 100,
    mean(subject_adherence$adequate == 1) * 100
  )
)

# Adherence by scheduled sample (+30 min vs +10 hr)
sched_times <- sort(unique(na.omit(cort_clean$ScheduledTime)))

good <- adequate <- poor <- numeric(length(sched_times))

for (i in seq_along(sched_times)) {
  idx <- cort_clean$ScheduledTime == sched_times[i]
  devs <- cort_clean$Deviation[idx]
  
  good[i] <- mean(devs <= 7.5, na.rm = TRUE) * 100
  adequate[i] <- mean(devs > 7.5 & devs <= 15, na.rm = TRUE) * 100
  poor[i] <- mean(devs > 15, na.rm = TRUE) * 100
}

by_sample_df <- data.frame(
  ScheduledTime = ifelse(sched_times == 30, "+30 min", "+10 hr"),
  Good = round(good, 1),
  Adequate = round(adequate, 1),
  Poor = round(poor, 1)
)

# Combine into one table
final_table <- list(
  "Overall Sample-Level Adherence (%)" = sample_df,
  "Subject-Level Perfect Adherence (%)" = subject_df,
  "Adherence by Scheduled Sample (%)" = by_sample_df
)

kable(final_table, caption = "Table 1: Adherence Summary")



###################### Research Question 3 ###############################

# Time since waking
time <- cort_clean$Booklet.MinutesSinceWaking

# Piece 1: from 0 to 30 min
cort_clean$Time0_30 <- pmin(time, 30)

# Piece 2: time after 30 min
cort_clean$Time30plus <- pmax(time - 30, 0)

mod2 <- lmer(Cortisol..nmol.L. ~ Time0_30 + Time30plus + 
                   (1 | SubjectID), 
                 data = cort_clean)

mod3 <- lmer(DHEA..nmol.L. ~ Time0_30 + Time30plus + 
               (1 | SubjectID),
             data = cort_clean)

summary(mod2)
summary(mod3)


# More plots to visualize

model_data <- model.frame(mod2)

model_data$pred_cort <- predict(mod2)

cort_clean$row_id <- seq_len(nrow(cort_clean))
model_data$row_id <- cort_clean$row_id[as.numeric(rownames(model_data))]

cort_clean <- merge(cort_clean, model_data[, c("row_id", "pred_cort")],
                    by = "row_id", all.x = TRUE)

cort_clean <- cort_clean[order(cort_clean$Booklet.MinutesSinceWaking), ]

ggplot(cort_clean, aes(x = Booklet.MinutesSinceWaking,
                       y = Cortisol..nmol.L.)) +
  geom_point(aes(color = SubjectID), alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.5, se = FALSE, color = "green") +
  labs(title = "Cortisol Over the Day (Piecewise LMM Fit)",
       x = "Minutes Since Waking",
       y = "Cortisol (nmol/L)")

ggplot(cort_clean, aes(x = Booklet.MinutesSinceWaking,
                       y = DHEA..nmol.L.)) +
  geom_point(aes(color = SubjectID), alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.5, se = FALSE, color = "red") +
  labs(title = "DHEA Over the Day (Piecewise LMM Fit)",
       x = "Minutes Since Waking",
       y = "DHEA (nmol/L)")
















