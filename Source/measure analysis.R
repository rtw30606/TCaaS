# Measure analysis

measure.analysis  <-  Measure |>
  group_by(RoomID) |> 
  summarize(Temperature = mean(Temp_C), mean(Voltage_V), mean(Heat_activation), 
            mean(CO2_ppm), mean(Pressure_bar))

measure.analysis.table <- gt(measure.analysis) |> 
  tab_header(
    title = md("**Room by mean measure")) 
