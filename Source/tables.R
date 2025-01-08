# Sensor ----
dbGetQuery(con, "SELECT * FROM Sensor")


# Sensor V2 ----

path <- "~/Documents/R/TCaaS/Files/sensors.xlsx"
sensor.v2 <- read_excel(path)
SensorV2 <- sensor.v2 |> 
  select(SensorID = id, DeviceId = device_id, Measured = isMeasuredMeter, 
         Diff_datapointID = diff_datapoint_id, Ref_time = sensor_ref_time, UnitID = unit_id) |>
  mutate(Measured = as.logical(Measured))
write_feather(Sensor, "Files/SensorV2.feather")

# match locationID and roomID
locationroom <- Sensor |> 
  select(LocationID, RoomID)
locationroom <- unique(locationroom)

# Building -----

Building <- tibble(18,3,'')
colnames(Building) = c('BuildingID', 'BuildingFloors', 'BuildingName')
write_feather(Building, "Files/Building.feather")
dbWriteTable(con, "Building", Building, overwrite = TRUE)
dbGetQuery(con, "SELECT * FROM Building LIMIT 1")

# Room ------
# path <- "~/Documents/R/TCaaS/Files/locations_meta.xlsx"
# meta <- read_excel(path)
# Room <- meta |> 
#   select(Room = room,  RoomFloor = floor, RoomArea = area, 
#          RoomDirection = Direction, RoomType = Room_type, LocationId = location_id, BuildingID = building)  |>
#   mutate(RoomID = as.integer(Room) + 100*RoomFloor)
# write_feather(Room, "Files/Room.feather")

# Device
path <- "~/Documents/R/TCaaS/Files/device_location.xlsx"
device <- read_excel(path)
Device <- device |> 
  select(DeviceID = device_id, LocationID = location_id, 
         Installation = installation_datetime, Removal = removement_datetime)
# include room
Device <- right_join(locationroom, Device, by = join_by (LocationID == LocationID))

write_feather(Device, "Files/Device.feather")
dbWriteTable(con, "Device", Device, overwrite = TRUE)
dbGetQuery(con, "SELECT * FROM Device LIMIT 1")


# Device info
path <- "~/Documents/R/TCaaS/Files/device_info.xlsx"
deviceInfo <- read_excel(path)
DeviceInfo <- deviceInfo |> 
  select(DeviceTypeID = device_type, DeviceID = device_id, Usage = in_usage) |> 
  mutate(Usage = as.logical(Usage))
devicetype_ids <- unique(DeviceInfo$DeviceTypeID)
write_feather(DeviceInfo, "Files/DeviceInfro.feather")

# Device type

# Read spreadsheet
path <- "~/Documents/R/TCaaS/Files/Spreadsheets.xlsx"
DeviceType <- read_excel(path, sheet = "device_types") |> 
  select(DeviceTypeID = DeviceID, Name, Manufacturer, Model_number)
write_feather(DeviceType, "Files/DeviceType.feather")

# Unit -----
Init <- dbGetQuery(conDuck, "SELECT * FROM Unit")

unit_analysis <- Unit |> 
  filter(ColumnName != "")

unit.table <- gt(unit_analysis) |>
  tab_header(
    title = md("**Measurement units**")) |>
   cols_hide(columns = c(UnitID,ColumnName)) |>
   cols_label(
    UnitNameDE = 'German',
    UnitNameEN = 'English',
    UnitSymbol = "Symbol")


# Cleaning
location_ids <- unique(Room$LocationId)
device_ids <- unique(Device$DeviceID)
