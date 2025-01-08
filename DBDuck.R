library(duckdb)
library(duckplyr)
con <- dbConnect(duckdb())
# or
con <- dbConnect(duckdb(), dbdir = ":memory:")
# to use a database file (not shared between processes)

Measure <- read_feather("Files/Measure.feather")
dbWriteTable(con, "Measure", Measure)
dbGetQuery(conDuck, "SELECT * FROM Measure LIMIT 1")

Device <- read_feather("Files/Device.feather")
dbWriteTable(con, "Device", Device)
dbGetQuery(con, "SELECT * FROM Device LIMIT 1")

Sensor <- read_feather("Files/Sensor.feather")
dbWriteTable(con, "Sensor", Device)
dbGetQuery(con, "SELECT * FROM Sensor LIMIT 1")
dbGetQuery(conDuck,"SHOW TABLES;")

Room <- read_feather("Files/Room.feather")
dbWriteTable(con, "Room", Room)
dbGetQuery(con, "SELECT * FROM Room LIMIT 1")

dbGetQuery(conDuck, "CREATE OR REPLACE VIEW MeasureRoom AS SELECT * FROM Measure Inner JOIN Room ON Measure.RoomID = Room.RoomID")

dbGetQuery(con, "SELECT * FROM MeasureRoom")
dbGetQuery(conDuck, "SHOW TABLES")
