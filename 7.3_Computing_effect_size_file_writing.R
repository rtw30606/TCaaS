library(dplyr)
library(lubridate)
library(readr)

# Load the data
df_merged <- read_csv("D:\\Project thesis\\data\\data_with_location_info.csv")

# Convert 'time' column to POSIXct if not already done
df_merged <- df_merged %>%
  mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  filter(!is.na(time))

# Add date, hour, minute, is_weekend columns
df_merged <- df_merged %>%
  mutate(
    date = as.Date(time),
    hour = hour(time),
    minute = minute(time),
    is_weekend = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  )

### 1. Calculate Baseline CO2 for Weekends (9 AM - 5 PM)

# Filter for weekends between 9 AM and 5 PM
weekend_co2 <- df_merged %>%
  filter(unit_id == 69, is_weekend == "Weekend", hour >= 9, hour < 17)

# Calculate mean and SD for CO2 on weekends (baseline)
baseline_co2 <- weekend_co2 %>%
  summarise(
    baseline_mean = mean(value_v15, na.rm = TRUE),
    baseline_sd = sd(value_v15, na.rm = TRUE)
  )

### 2. Calculate Mean and SD for Each Room During Weekdays (9 AM - 5 PM)

# Filter for weekdays between 9 AM and 5 PM
weekday_co2 <- df_merged %>%
  filter(unit_id == 69, is_weekend == "Weekday", hour >= 9, hour < 17)

# Calculate the CO2 difference from the baseline for each 15-minute interval
co2_diff <- weekday_co2 %>%
  mutate(
    co2_diff = value_v15 - baseline_co2$baseline_mean,
    z_score = co2_diff / baseline_co2$baseline_sd,
    effect_size = abs(z_score)  # Absolute value of z-score as effect size
  ) %>%
  group_by(location_id, date, hour, minute) %>%
  summarise(
    mean_diff = mean(co2_diff, na.rm = TRUE),
    effect_size = mean(effect_size, na.rm = TRUE),  # Save effect size
    occupancy = sum(abs(z_score) > qnorm(1 - 0.001 / 2), na.rm = TRUE) > 0
  ) %>%
  ungroup()

### 3. Save the Results with Effect Size

# Save the results to a new CSV file including the effect size
output_file_path <- "D:\\Project thesis\\data\\analysis\\co2_occupancy_with_effect_size.csv"
write_csv(co2_diff, output_file_path)

print(paste("Results saved to:", output_file_path))
