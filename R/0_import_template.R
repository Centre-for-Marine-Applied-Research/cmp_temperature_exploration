# import and explore CMP temperature data
# must be connected to VPN or on Perennia wifi (faster on the wifi!)

# cmar data explorer: https://cmar-cmp-time-series.share.connect.posit.cloud


library(cmpr)           # get data from the CMAR database
library(sensorstrings)  # plot data

library(data.table)     # export csv file
library(dplyr)          # wrangling data
library(here)           # file paths
library(lubridate)      # dates & times
library(plotly)         # interactive figures

# open database connection
conn <- cmpr_connect_to_db(connection_config = "admin")


# station locations -------------------------------------------------------

# Look at all stations and their location data
station_metadata <- cmpr_get_station_metadata(conn) %>%
  filter(station_classification == "coastal") %>%
  select(
    station = station_name,
    waterbody = waterbody_name,
    county = county_name,
    latitude = station_latitude,
    longitude = station_longitude,
    classification = station_classification
  )

ss_leaflet_station_map(station_metadata)


# single station ----------------------------------------------------------

# Look at all the deployments at a station
station_depls <- cmpr_get_station_depl(conn, station_name = "Spry Harbour")

# Get all the data from a given station - this could take 5 - 6 minutes for
# the stations with a lot of data
cmpr_station_dat <- cmpr_get_station_data(
  conn,
  station_name = "Spry Harbour",
  variable_type = "temperature"
) 

station_dat <- cmpr_station_dat %>%
  select(
    station = station_name,
    depl_date,
    retrieval_date,
    sensor_serial_num,
    sensor_depth_at_low_tide_m = sensor_depth_m,
    variable = variable_name,
    timestamp_utc,
    value = variable_value
  ) %>% 
  ss_convert_depth_to_ordered_factor() %>% 
  ss_pivot_wider()

# plot
ss_ggplot_variables(station_dat)

# filter and make interactive plot
station_dat_filt <- station_dat %>% 
  filter(
    timestamp_utc > as_datetime("2020-06-01"),
    timestamp_utc < as_datetime("2020-11-12")
  )

p <- ss_ggplot_variables(station_dat_filt)

ggplotly(p)


# multiple stations -------------------------------------------------------

stations <- c("Spry Harbour", "Eddy Cove", "0001", "Shad Bay")

dat <- cmpr_get_station_data(
  conn,
  station_name = stations,
  variable_type = "temperature"
) 

# export data -------------------------------------------------------------

# save data to analyse later (e.g., calculate daily averages and export)
saveRDS(dat, here("data/file_name.RDS"))

# if you want to save as a csv file
fwrite(dat, "data/file_name.csv")

# end database connection
DBI::dbDisconnect(conn)
