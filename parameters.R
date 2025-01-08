#| label: parameters
#| warning: false
#| message: true
#| eval: true

setwd("~/Documents/R/TCaaS")
start_date <-  "2024-05-01"
sec_days = 60*60*24
step_range <-  c(1:96)
hour_range <-  c(0:23)
conDuck <- dbConnect(duckdb(), dbdir = "~/Library/CloudStorage/Dropbox/duckdb")