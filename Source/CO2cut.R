# measure ppm on Sunday
Measure <- dbGetQuery(conDuck, "SELECT * FROM Measure")

CO2Sunday <- Measure |> 
  group_by(RoomID) |>
  mutate(mday = wday(datetime)) |>
  filter(mday == 7) |> 
  summarize(mean = mean(CO2_ppm), sd = sd(CO2_ppm)) |> 
  arrange(mean)

# Delete room 217 with high value.
CO2Sunday <- CO2Sunday |> 
  filter(roomID != 217)
CO2cut <- mean(na.omit(CO2Sunday$mean + 2*CO2Sunday$sd))



