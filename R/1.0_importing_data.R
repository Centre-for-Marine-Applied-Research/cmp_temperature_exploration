
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


# multiple stations -------------------------------------------------------
# add or remove any stations of interest ----------------------------------

# this will take a few minutes, especially stations with a lot of data and 
#   multiple stations together -------------------------------------------

stations <- c("Big Pond Point", "Wedgeport", "Birchy Head", 
              "Spry Harbour", "Center Bay", "Moose Point 1")

dat <- cmpr_get_station_data(conn,
                             station_name = stations,
                             variable_type = "temperature")
data <- dat %>% 
  filter(
    sensor_depth_at_low_tide_m == c("2","5","10"))
#warning message will appear - "Wedgeport" does not have a 10m reading, just 2m & 5m


# filter for desired dates ------------------------------------------------
data <- data %>% 
  filter(
    timestamp_utc > as_datetime("2020-01-01"),
    timestamp_utc < as_datetime("2024-01-01")
  )

# export data -------------------------------------------------------------

# save data to analyse later
saveRDS(data, here("data/data_1.0.RDS"))

# end database connection
DBI::dbDisconnect(conn)

