# import and explore CMP temperature data
# must be connected to VPN or on Perennia wifi (faster on the wifi!)

# cmar data explorer: https://cmar-cmp-time-series.share.connect.posit.cloud

library(cmpr)           # get data from the CMAR database
library(qaqcmar)        # plot qc flags
library(sensorstrings)  # plot data

library(data.table)     # export csv file
library(dplyr)          # wrangling data
library(here)           # file paths
library(lubridate)      # dates & times
library(plotly)         # interactive figures

# open database connection
conn <- cmpr_connect_to_db(connection_config = "admin")


# extract measured depth data -------------------------------------------------------

stations <- c("Big Pond Point", "Wedgeport", "Beaver Point", 
              "Birchy Head", "Moose Point 1")

dat <- cmpr_get_station_data(
  conn,
  station_name = stations,
  variable_name = "sensor_depth_measured_m"
) 

# export data -------------------------------------------------------------

# if you want to save as a csv file
fwrite(dat, "data/station_measured_depth_data.csv")

# end database connection
DBI::dbDisconnect(conn)
