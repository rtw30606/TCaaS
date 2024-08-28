library(dplyr)
library(readr)

# Step 1: Read the sensor locations file and ensure unique location_id values
sensor_locations <- read_csv("D:/Project thesis/sensor_locations_mapped_V2.csv") %>%
  select(location_id, area) %>%
  distinct()  # Ensure unique location_id values

# Step 2: Read the file with effect size data
effect_size_data <- read_csv("D:/Project thesis/Data/Analysis/co2_occupancy_with_effect_size.csv")

# Step 3: Merge the area information into the effect size data
merged_data <- effect_size_data %>%
  left_join(sensor_locations, by = "location_id")

# Step 4: Filter only the occupied times
occupied_data <- merged_data %>%
  filter(occupancy == TRUE)

# Step 5: Compute the correlation between room area and effect size
correlation_result <- cor(occupied_data$area, occupied_data$effect_size, use = "complete.obs")

# Display the correlation
print(paste("Correlation between room area and effect size during occupancy:", correlation_result))

# Optional: Write the merged data to a CSV file for future reference
write_csv(merged_data, "D:/Project thesis/Data/Analysis/merged_effect_size_with_area.csv")

library(arrow)

# Write the merged data to a Feather file
write_feather(merged_data, "D:/Project thesis/Data/Analysis/merged_effect_size_with_area.feather")

library(ggplot2)

# Create a scatter plot of room area vs. effect size
ggplot(merged_data, aes(x = area, y = effect_size)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear trend line
  labs(
    title = "Scatter Plot of Room Area vs. Effect Size During Occupancy",
    x = "Room Area (Square Meters)",
    y = "Effect Size"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Center and size title
    axis.text = element_text(size = 12),  # Increase axis text size
    axis.title = element_text(size = 14)  # Increase axis title size
  )

