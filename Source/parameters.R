# parameters

setwd("~/Documents/R/TCaaS")
start_datetime <-  "2024-05-01 00:00:00 CET"
start_date <- date(start_datetime)

sec_days = 60*60*24
step_range <-  c(1:96)
hour_range <-  c(0:23)
conDuck <- dbConnect(duckdb(), dbdir = "~/Library/CloudStorage/Dropbox/duckdb")