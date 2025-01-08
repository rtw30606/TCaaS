# Temperature analysis by room
Measure <- dbGetQuery(conDuck, "SELECT * FROM Measure")
Temperature <- Measure |>
  filter(!(RoomID %in% exclusion_room$RoomID)) 

temperature.analysis <-  Temperature |>
  mutate(Hour = hour(datetime)) |>
  mutate(Room = as.factor(RoomID)) |>
#  filter(Hour >= 8 & Hour <= 18) |> 
  group_by(Room, Hour) |> 
  summarize(C = mean(Temp_C, na.omit=T)) |> 
  arrange(Room)

ggplot(temperature.analysis, aes(y = Room, x = Hour, fill = C)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient(low = "white", high = "red")

table <- gt(temperature.analysis) |> 
  tab_header(
    title = md("**Hourly temperature and occupancy analysis (ascending C)**"), 
    subtitle = md("***Range is 0 (empty) to 1 (occupied) ***")
  ) |> 
  cols_label(Room = "Room", C = "C") |> 
  fmt_number(columns = -Room, decimals = 2)

mean(Measure$C)
