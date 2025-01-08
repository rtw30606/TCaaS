# Heat map analysis

CO2.analysis <- Measure |> 
  select(roomID, hour, occupancy) |> 
  group_by(roomID, hour) |>
  summarize(occupancy = mean(occupancy))
# Join with Room

CO2.analysis <- inner_join(CO2.analysis, Room, by = join_by(roomID == RoomID))
# Analyse for difference room types

room.parameter <- c('Office', 'Meeting room')
heat.map <- list()
for (p in 1:length(room.parameter)) {
  
  room.CO2.analysis <- CO2.analysis |> 
    filter(RoomType == room.parameter[p])
  
  temp <- unique(room.CO2.analysis$roomID)
  room.index <- rep(0,length(temp))

room.count <- max(room.CO2.analysis$roomID) - min(room.CO2.analysis$roomID)

mat <- matrix(0, nrow = room.count, ncol = max(room.CO2.analysis$hour) +1)


# map room to an index
temp <- unique(room.CO2.analysis$roomID)
room.index <- rep(0,length(temp))

for (r in 1:length(temp)) {
  room.index[temp[r]] <- r
}

mat <- matrix(0, nrow = length(temp), ncol = max(room.CO2.analysis$hour) +1)

for (i in 1:nrow(room.CO2.analysis))
{
  indx <- as.integer(room.CO2.analysis$roomID[i]) # room index
  j <- room.index[indx] # room
  #  j <- ppm_analysis$room[i] # room index
  k <- CO2.analysis$hour[i] + 1
  mat[j, k] = room.CO2.analysis$occupancy[i]
}

valid_room <- filter_if(as_tibble(room.CO2.analysis$roomID), is.numeric, all_vars((.) != 0))
room_labels <- as_vector(unique(valid_room[1]))
#colors = structure(1:2, names = c("1", "2")) # black

col_fun = colorRamp2(c(0, 0.25, 0.5, 0.75, 1), c("green", "white", "red", "blue", "yellow"))
heat.map[[p]] <- Heatmap(mat,
                    heatmap_legend_param = list(
                      title = "Occupancy", at = c(0, 0.25, 0.5, 0.75, 1)),
                    name = room.parameter[p],
                    row_labels = room_labels,
                    column_labels = hour_range,
                    col = col_fun,
                    column_title = "Hours",
                    row_title = "Room",
                    cluster_rows = FALSE, # turn off row clustering
                    cluster_columns = FALSE)
}



# Occupancy for each room 800-1800
room.occupancy.analysis  <-  MeasureRoom |>
  filter(hour >= 8 & hour <= 18) |> 
  filter(RoomType == 'Office') |> 
  select(roomID, occupancy) |> 
  group_by(roomID) |> 
  summarize(mean = mean(occupancy)) |> 
  mutate(RoomID = roomID)  |>
    arrange(mean, RoomID)

# include room type
occupancy.analysis <- inner_join(occupancy.analysis, Room, by = join_by (RoomID))  
occupancy.analysis <- occupancy.analysis |> 
  select(RoomID, RoomType, mean) |>
  arrange(mean)

# Office occupancy 
office.occupancy.analysis <- occupancy.analysis |> 
filter(RoomType == 'Office') |> 
  select(RoomID, mean)

office.occupancy.table <- gt(occupancy.analysis) |> 
  tab_header(
    title = md("**Office hourly occupancy 800-1600"), 
    subtitle = md("***Range is 0 (empty) to 1 (occupied) ***")
  ) |> 
  cols_label(RoomID = "Room", mean = "Mean") |> 
  fmt_number( columns = mean, decimals = 2)



office.occupancy.table <- gt(office.occupancy.analysis) |> 
  tab_header(
    title = md("**Hourly occupancy 800-1600"), 
    subtitle = md("***Range is 0 (empty) to 1 (occupied) ***")
    ) |> 
  cols_label(RoomID = "Room", mean = "Mean") |> 
  fmt_number( columns = mean, decimals = 2)


# Room type occupancy
room.occupancy.analysis  <-  occupancy.analysis |>
  group_by(RoomType) |> 
  summarize(mean = mean(mean)) |>
  arrange(mean)

room.occupancy.table <- gt(room.occupancy.analysis) |> 
  tab_header(
    title = md("**Hourly occupancy 800-1600"), 
    subtitle = md("***Range is 0 (empty) to 1 (occupied) ***")
  ) |> 
  cols_label(RoomType = "Type", mean = "Mean") |> 
  fmt_number( columns = mean, decimals = 2)
room.occupancy.table



