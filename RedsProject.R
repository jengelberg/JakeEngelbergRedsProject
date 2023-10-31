install.packages("tidyverse")
install.packages("caTools")
library(readr)
library (dplyr)
library(caTools)
library(rpart)
library(tidyr)

#Load in data
data <- read.csv("/Users/Jake/Downloads/data.csv")

## Normalizing Velocity Data-----------------------------------

#Create and sort a subset of data with pitch id, pitcher key, pitch type, and velocity
Velo_subset_data <- data[, c("PID", "PITCHER_KEY", "PITCH_TYPE_TRACKED_KEY", "RELEASE_SPEED")]
velo_sorted_df <- Velo_subset_data %>%
  arrange(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY)

# Calculate AverageBreak and StandardDeviation within groups
velo_sorted_df <- velo_sorted_df %>%
  group_by(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY) %>%
  mutate(
    AverageVelo = cumsum(RELEASE_SPEED) / seq_along(RELEASE_SPEED),
    StandardDeviation = sd(RELEASE_SPEED)
  )

# The average velo calculated above is essentially a "running tally" so this adds the true average velo
# for each pitcher and pitch type to the data set

velo_sorted_df <- velo_sorted_df %>%
  group_by(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY) %>%
  mutate(AverageVelo = last(AverageVelo))

# Normalize the HORIZONTAL_BREAK data within each group
velo_sorted_df <- velo_sorted_df %>%
  mutate(Normalized = (RELEASE_SPEED - AverageVelo) / StandardDeviation)

# Plot the Normalized data
plot(velo_sorted_df$Normalized)


## Normalizing Horizontal Break Data-------------------


#Create and sort a subset of data with pitch id, pitcher key, pitch type, and horizontal break
HB_subset_data <- data[, c("PID", "PITCHER_KEY", "PITCH_TYPE_TRACKED_KEY", "HORIZONTAL_BREAK")]
HB_sorted_df <- HB_subset_data %>%
  arrange(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY)

# Calculate AverageBreak and StandardDeviation within groups
HB_sorted_df <- HB_sorted_df %>%
  group_by(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY) %>%
  mutate(
    AverageBreak = cumsum(HORIZONTAL_BREAK) / seq_along(HORIZONTAL_BREAK),
    StandardDeviation = sd(HORIZONTAL_BREAK)
  )

# The average break calculated above is essentially a "running tally" so this adds the true average horizontal
# break for each pitcher and pitch type to the data set
HB_sorted_df <- HB_sorted_df %>%
  group_by(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY) %>%
  mutate(AverageBreak = last(AverageBreak))

# Normalize the HORIZONTAL_BREAK data within each group
HB_sorted_df <- HB_sorted_df %>%
  mutate(Normalized = (HORIZONTAL_BREAK - AverageBreak) / StandardDeviation)

# Plot the Normalized data
plot(HB_sorted_df$Normalized)





## Normalizing Vertical Break Data-------------------


#Create and sort a subset of data with pitch id, pitcher key, pitch type, and vertical break
Vert_subset_data <- data[, c("PID", "PITCHER_KEY", "PITCH_TYPE_TRACKED_KEY", "INDUCED_VERTICAL_BREAK")]
vert_sorted_df <- Vert_subset_data %>%
  arrange(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY)

# Calculate AverageBreak and StandardDeviation within groups
vert_sorted_df <- vert_sorted_df %>%
  group_by(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY) %>%
  mutate(
    AverageVertBreak = cumsum(INDUCED_VERTICAL_BREAK) / seq_along(INDUCED_VERTICAL_BREAK),
    StandardDeviation = sd(INDUCED_VERTICAL_BREAK)
  )

# The average break calculated above is essentially a "running tally" so this adds the true average vertical
# break for each pitcher and pitch type to the data set
vert_sorted_df <- vert_sorted_df %>%
  group_by(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY) %>%
  mutate(AverageVertBreak = last(AverageVertBreak))

# Normalize the HORIZONTAL_BREAK data within each group
vert_sorted_df <- vert_sorted_df %>%
  mutate(Normalized = (INDUCED_VERTICAL_BREAK - AverageVertBreak) / StandardDeviation)

# Plot the Normalized data
plot(vert_sorted_df$Normalized)




## Normalizing Spin Rate Data-------------------


#Create and sort a subset of data with pitch id, pitcher key, pitch type, and spin rate
spin_subset_data <- data[, c("PID", "PITCHER_KEY", "PITCH_TYPE_TRACKED_KEY", "SPIN_RATE_ABSOLUTE")]
spin_sorted_df <- spin_subset_data %>%
  arrange(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY)

# Calculate AverageBreak and StandardDeviation within groups
spin_sorted_df <- spin_sorted_df %>%
  group_by(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY) %>%
  mutate(
    AverageSpin = cumsum(SPIN_RATE_ABSOLUTE) / seq_along(SPIN_RATE_ABSOLUTE),
    StandardDeviation = sd(SPIN_RATE_ABSOLUTE)
  )

# The average break calculated above is essentially a "running tally" so this adds the true average spin rate
# break for each pitcher and pitch type to the data set
spin_sorted_df <- spin_sorted_df %>%
  group_by(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY) %>%
  mutate(AverageSpin = last(AverageSpin))

# Normalize the HORIZONTAL_BREAK data within each group
spin_sorted_df <- spin_sorted_df %>%
  mutate(Normalized = (SPIN_RATE_ABSOLUTE - AverageSpin) / StandardDeviation)

# Plot the Normalized data
plot(spin_sorted_df$Normalized)




## Distribution of data ----------

# Calculates CDF for normal distribution of each of the four variables
velo_sorted_df$norm_col <- 1-pnorm(velo_sorted_df$Normalized)

# ifelse accounts for the fact that lefty and righty pitches break in different directions
HB_sorted_df$norm_col = ifelse(HB_sorted_df$HORIZONTAL_BREAK >= 0, (1-pnorm(HB_sorted_df$Normalized)), (pnorm(HB_sorted_df$Normalized)))

# ifelse accounts for the fact that pitches can break in the up and down direction
vert_sorted_df$norm_col = ifelse (vert_sorted_df$INDUCED_VERTICAL_BREAK >= 0, (1-pnorm(vert_sorted_df$Normalized)), pnorm(HB_sorted_df$Normalized))

spin_sorted_df$norm_col <- pnorm(spin_sorted_df$Normalized)

# Averages the CDFs for the four variables and adds it to the original data set
data$DEWPOINT_AFFECTED <- (velo_sorted_df$norm_col + HB_sorted_df$norm_col + vert_sorted_df$norm_col + spin_sorted_df$norm_col)/4

# Sets any null values to .50 (perfectly average)
data$DEWPOINT_AFFECTED[is.na(data$DEWPOINT_AFFECTED)] <- .50

#Plots and summarizes the dewpoint affected percentages
plot(data$DEWPOINT_AFFECTED)
summary(data$DEWPOINT_AFFECTED)

## Output----------------

# Creates a subset with just pitch ID and dewpoint_affected percentage
output <- data[, c("PID", "DEWPOINT_AFFECTED")]

# Specify the file path for the CSV file
file_path <- "/Users/Jake/Downloads/submission.csv"

# Write the data frame to a CSV file using write.csv
write.csv(output, file = file_path, row.names = FALSE)


