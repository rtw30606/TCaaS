# Fix temperature

temp1 <-  dbGetQuery(conDuck, "Select * from MeasureDevice") |> 
  filter(DeviceTypeID == "FEB01") 
|> 
  arrange(datetime)|> 
  mutate(prev_C = lag(Temp_C, 1)) |> 
  mutate(Cfix1 = ((prev_C*256 + Temp_C)/10)-100)  |>
  mutate(Cfix2 = ((Temp_C*256 + prev_C)/10)-100)  |>
  group_by(month(datetime)) |> 
  summarize(m1 = mean(Cfix1), m2 = mean(Cfix2))
summary(temp)

temp2 <- dbGetQuery(conDuck, "Select * from MeasureDevice") |> 
  filter(DeviceTypeID != "FEB01") 

CFix = rbind(temp1,temp2)

t1 <-  dbGetQuery(conDuck, "Select * from Measure")
mean(t1$Temp_C)
summary(t1)
t2 <-  dbGetQuery(conDuck, "Select * from MeasureDevice") |> 
  filter(DeviceTypeID == "FEB01")
mean(t2$Temp_C)
temp <-  t15 |>
  filter(unit_id == 38 & device_type == "FEB01") |> 
  mutate(Temp_C = value_v15) |> 
  mutate(prev_C = lag(Temp_C, 1)) |> 
  mutate(Cfix = ((prev_C*256 + Temp_C)/10)-100)
summary(temp)

|> 
  filter(Cfix < 40) |>
  summarize(mean = mean(Cfix))

temp4 <-  t15 |> 
  filter(unit_id == 38)