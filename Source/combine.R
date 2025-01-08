# Combine measure and room
t15 <- read_feather("Files/t15.feather") |> 
  select(id, unit_id, symbol, time, value_v15)
t15$datetime <-  as_datetime(t15$time)
# min.time <- min(t15$datetime)
# max.time <- max(t15$datetime)

location <- read_delim("Files/sensor_location_mapping.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
location$roomID <- location$room + location$floor*100
loc <- location |> 
  select(roomID, id)
t15room  <-  inner_join(t15, loc, by = join_by(id))
t15room  <- t15room |> 
  select(id, unit_id, symbol, datetime, roomID, value_v15)
write_feather(t15room,'Files/t15room.feather')

Room <- location |> 
  select(RoomID = roomID, RoomArea = area, 
         RoomDirection = Direction, RoomType = Room_type,  BuildingID = building)
Room <- unique(Room)

exclusion_list <- c('First aid room', 'Kitchenette', 'Server room', 'Storage room', 'WC')

Room  <- Room |>
  filter(!RoomType %in% exclusion_list)

write_feather(Room, "Files/Room.feather")
