# Read spreadsheet
library(readxl)
xls_file 
sensors <-  ("Files/Spreadsheets.xlsx", sheet = "sensors")
devices <- read_excel("Files/Spreadsheets.xlsx", sheet = "devices")
device_types <- read_excel("Files/Spreadsheets.xlsx", sheet = "device_types")
device_location <- read_excel("Files/Spreadsheets.xlsx", sheet = "device_location")
# units <- read_excel("Files/Spreadsheets.xlsx", sheet = "units")
Room <- read_feather('Files/Room.feather')

location_ids <- unique(Room$location_id)

device_location
Sensor <- read_feather('Files/Sensor.feather')
summary(sensor)
