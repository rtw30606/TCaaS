# Clean the data

t15room <- read_feather('Files/t15room.feather')

ids <- t15room |>
  distinct(id)

symbols <- t15room |>
  distinct(symbol)

unit_ids <- t15room |>
  distinct(unit_id)

symbol_unit <- t15room |>
  distinct(symbol,unit_id)

# # code blank symbols
# symbol_unit  <-  symbol_unit |>
#   mutate (symbol = case_when (
#     unit_id == 57 ~ "State",
#     unit_id == 56 ~ "Consumption",
#     TRUE ~ symbol
#   )
#   )

# extract columns

V <- tibble()
V <- t15room |>
  filter(symbol == "V") |>
  select(id, datetime, V = value_v15, roomID)

bar <- tibble()
bar <- t15room |>
  filter(symbol == "bar") |>
  select(id, datetime, bar = value_v15, roomID)

ppm <- tibble()
ppm <- t15room |>
  filter(symbol == "ppm") |>
  select(id, datetime, ppm = value_v15, roomID)

loss <- tibble()
loss <- t15room |>
  filter(symbol == "%") |>
  select(id, datetime, loss = value_v15, roomID)

C <- tibble()
C <- t15room |>
  filter(grepl("C", symbol)) |>
  select(id, datetime, C = value_v15, roomID)
# Fix temperature calculation
C <- arrange(C, datetime)
summary(C)
Clag <- C |>
  mutate(prev_C = lag(C, 1))
Clag <- Clag |>
  mutate ( Cfix = ((prev_C*256 + C)/10)-100)

summary(Clag)
# 
# (((value_v15 * 256) + Temp2) / 10) - 100

# Remove duplicates before joining

V_unique <-  V |> 
  distinct(datetime, roomID, .keep_all = TRUE)
C_unique <-  C |> 
  distinct(datetime, roomID, .keep_all = TRUE)
loss_unique <-  loss |> 
  distinct(datetime, roomID, .keep_all = TRUE)
ppm_unique <-  ppm |> 
  distinct(datetime, roomID, .keep_all = TRUE)
bar_unique <-  bar |> 
  distinct(datetime, roomID, .keep_all = TRUE)

J1 <- left_join(C_unique, V_unique, by = join_by(datetime, roomID))
J2 <- left_join(J1, loss_unique, by = join_by(datetime, roomID))
J3 <- left_join(J2, ppm_unique, by = join_by(datetime, roomID))
J <- left_join(J3, bar_unique, by = join_by(datetime, roomID))


# drop unneeded columns
colnames(J)[1] <- c("id")
Measure <- J[,c(1:4,6,8,10,12)]
Measure <- na.omit(Measure)
min.time <- min(Measure$datetime)
min.date <- date(min.time)
max.time <- max(Measure$datetime)
max.date <- date(max.time)
Measure <- Measure |> 
  mutate(day = as.integer(as.duration(interval(min.time, datetime))/sec_days) + 1)
Measure <- Measure |> 
  mutate(step = hour(datetime)*4 + minute(datetime)/15 + 1)
Measure <- Measure |> 
  mutate(hour = hour(datetime))
Measure <- Measure |>
  relocate(roomID, .after = last_col())
colnames(Measure)[3:7] = c("Temp_C", "Voltage_V", "Heat_activation", 
                           "CO2_ppm", "Pressure_bar")
write_feather(Measure,'Files/Measure.feather')

measure.analysis <- Measure |>
  group_by(roomID) |>
  summarise(across(.cols = c(Temp_C:Pressure_bar), .fns = c(mean))) |> 
  mutate(RoomID = roomID)
measure.analysis <- inner_join(measure.analysis, Room, by = join_by(RoomID))

measure.table <-  gt(measure.analysis) |> 
  tab_header(
    title = md("**Measure analysis (means) **")) |>
  fmt_number(columns = everything(), decimals = 2) 

room.analysis <- Measure |>
  group_by(roomID) |>
  summarize(count = n())
colnames(room.analysis) <- c('RoomID', 'Observations')

room.analysis <- inner_join(room.analysis, Room, by = join_by(RoomID))
room.analysis <- room.analysis |> 
  select(RoomID, RoomType, Observations) |>
  arrange(RoomType)

room.table <- gt(room.analysis) |> 
  tab_header(
    title = md("**Room analysis**")) |>
  fmt_number(columns = "RoomID", decimals = 0) |>
  fmt_number(columns = "Observations", decimals = 0) |>
  grand_summary_rows(
    columns = Observations, 
    fns = list(Total ~ sum(., use_seps = FALSE)))

obs.per.room <-  round(sum(room.analysis$Observations)/dim(room.analysis)[1], -3)

room.table <- gt(room.analysis) |> 
  tab_header(
    title = md("**Room analysis**")) |>
  fmt_number(columns = "RoomID", decimals = 0) |>
  fmt_number(columns = "Observations", decimals = 0) |>
  grand_summary_rows(
    columns = c(Observations), 
    fns = list(Total = ~sum(.)),
    formatter = fmt_number,
    use_seps = TRUE, decimals = 0)
