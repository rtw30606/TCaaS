# data quality

Measure <- dbGetQuery(conDuck, "SELECT * FROM Measure")

measure.analysis  <-  Measure |>
  group_by(RoomID) |> 
  summarize(Temperature = mean(Temp_C), mean(Voltage_V), mean(Heat_activation), 
            mean(CO2_ppm), mean(Pressure_bar))

measure.analysis.table <- gt(measure.analysis) |> 
  tab_header(
    title = md("**Room by mean measure")) 

# Monthy variation
ggplot(Measure, aes(Temp_C, as_factor(month(datetime)))) +
  geom_boxplot()

# Temperature variation by occupancy hours
Temperature.analysis 

# Room type variation
MeasureRoom <- dbGetQuery(conDuck, "SELECT * FROM MeasureRoom")
roomtemp.analysis <- MeasureRoom |> 
  group_by(RoomID) |> 
  summarize(Count = n()) |>
  arrange(desc(Count)) |>
  select(Temp_C)
ggplot(MeasureRoom, aes(Temp_C, as_factor(RoomType))) +
  geom_boxplot()

t15_2024 |> 
  group_by(id) |>
  summarize(count = n())
