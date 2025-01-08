# Occupancy analysis
Measure <- dbGetQuery(conDuck, "SELECT * FROM Measure")

Occupancy <- Measure |>
 mutate(occupancy = ifelse(CO2_ppm < CO2cut, 0,1)) |> 
  filter(!(RoomID %in% exclusion_room$RoomID)) 

CO2.analysis <- Occupancy |> 
  mutate(Hour = hour(datetime)) |> 
  mutate(Day = day(datetime)) |> 
  filter(between(Hour,8, 18)) |>
  filter(Day < 6) |>
  mutate(Room = as.factor(RoomID)) |>
  select(Room, Hour, Occupancy = occupancy, CO2_ppm) |> 
  group_by(Room, Hour) |>
  summarize(Occupancy = mean(Occupancy, na.rm=T), CO2 = mean(CO2_ppm, na.rm=T))

ggplot(CO2.analysis, aes(y = Room, x = Hour, fill = Occupancy)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient(low = "white", high = "red")

ggplot(CO2.analysis, aes(y = Room, x = Hour, fill = CO2)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient(low = "white", high = "red")




# Occupancy for each room 800-1800
occupancy.analysis  <-  Occupancy |>
  mutate(Hour = hour(datetime)) |> 
  filter(Hour >= 8 & Hour <= 18) |> 
  group_by(RoomID) |> 
  summarize(mean = mean(occupancy)) |> 
    arrange(mean, RoomID)

# include room type
occupancy.analysis <- inner_join(occupancy.analysis, Room, by = join_by (RoomID))  
occupancy.analysis <- occupancy.analysis |> 
  select(RoomID, RoomType, mean) |>
  arrange(mean)

occupancy.table <- gt(occupancy.analysis) |> 
  tab_header(
    title = md("**Hourly occupancy 800-1600"), 
    subtitle = md("***Range is 0 (empty) to 1 (occupied) ***")
    ) |> 
  cols_label(RoomType = "Type", RoomID = "Room", mean = "Mean") |> 
  fmt_number( columns = mean, decimals = 2)
occupancy.table

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


# Min and max for C02

Temp <- Occupancy |> 
  mutate(Hour = hour(datetime)) |> 
  mutate(Day = day(datetime)) |> 
  filter(between(Hour,8, 18)) |>
  filter(Day < 6)
min(Temp$CO2_ppm, na.rm=T)
max(Temp$CO2_ppm, na.rm=T)
