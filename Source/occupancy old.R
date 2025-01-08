range(Measure$ppm)
# Drop values less than 350 ppm
ppmfixMeasure <- Measure |> 
  filter(ppm > 350) 

# Cluster into two groups
cluster <-  kmeans(ppmfixMeasure$ppm, 2)
ppmfixMeasure$occupancy <- cluster$cluster
ppmfixMeasure$occupancy |> 
  group_by(room, hour) |>
  
  
  ggplot(Measure, aes(ppm)) +
  geom_boxplot() 
ggplot(Measure, aes(effectsize)) +
  geom_boxplot() 

ggplot(Measure, aes(hour, ppm)) +
  geom_smooth() 
+
  ylim(0,max(ppm))

# ppm analysis for Sunday to get base
ppm_measure <- Measure |> 
  filter(day == 7) |>
  group_by(hour) |>
  summarize(mean_ppm = mean(ppm), sd_ppm = sd(ppm))
ggplot(ppm_measure, aes(hour, mean_ppm)) +
  geom_smooth() +
  ylim(0,max(ppm_measure$mean_ppm))

ppm_measure <- Measure |> 
  filter(day <= 6) |>
  group_by(hour) |>
  summarize(mean_ppm = mean(ppm), sd_ppm = sd(ppm))
ggplot(ppm_measure, aes(hour, mean_ppm)) +
  geom_line() +
  ylim(0,max(ppm_measure$mean_ppm))

# Add effect size to Measure
Measure$effectsize =  (Measure$ppm - base_ppm$mean_ppm)/base_ppm$sd_ppm
ppm_analysis
room <- unique(ppm_analysis$room)
room <- sort(room)
step <- unique(ppm_analysis$step)
min(ppm_analysis$step)
mat <- matrix(0, nrow = length(room), ncol = length(step))

# map room to an index
room <- rep(0,max(ppm_analysis$room))
temp <- unique(ppm_analysis$room)

# map room to an index
for (r in 1:length(ppm_analysis$room)) {
  room[temp[r]] <- r
}

for(i in 1:nrow(ppm_analysis))
{
  indx <- as.integer(ppm_analysis$room[i]) # room index
  j <- room[indx] # room
  k <- ppm_analysis$step[i]
  mat[j, k] = ppm_analysis$ppm[i]
}

heatmap(mat)

# Hourly analysis

hour_range= c(9:17)

ppm_analysis <- Measure |> 
  group_by(room,hour) |>
  filter(hour %in% hour_range) |>
  summarize(mean_ppm = mean(ppm))
ppm_analysis
mat <- matrix(0, nrow = max(ppm_analysis$room), ncol = max(ppm_analysis$hour))

for(i in 1:nrow(ppm_analysis))
{
  j <- ppm_analysis$room[i] # room index
  k <- ppm_analysis$hour[i]
  mat[j, k] = ppm_analysis$ppm[i]
}
mat <- remove_zero(mat,1, TRUE)
mat <- remove_zero(mat,2, TRUE)
valid_room <- filter_if(as_tibble(room), is.numeric, all_vars((.) != 0))
row_labels <- as_vector(valid_room[1])
Heatmap(mat,
        name = "CO2 parts per million",
        row_labels = row_labels,
        column_labels = hour_range,
        col = rev(rainbow(10)),
        column_title = "Hours",
        row_title = "Room",
        cluster_rows = FALSE, # turn off row clustering
        cluster_columns = FALSE
)