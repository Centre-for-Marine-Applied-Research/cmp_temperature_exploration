# import and explore CMP temperature data
# must be connected to VPN or on Perennia wifi (faster on the wifi!)

# cmar data explorer: https://cmar-cmp-time-series.share.connect.posit.cloud


library(cmpr)           # get data from the CMAR database
library(sensorstrings)  # plot data

library(data.table)     # export csv file
library(dplyr)          # wrangling data
library(here)           # file paths
#library(plotly)         # interactive figures

# open database connection
conn <- cmpr_connect_to_db(connection_config = "admin")

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


# Look at all the deployments at a station
station_depls <- cmpr_get_station_depl(conn, station_name = "Spry Harbour")

# Get all the data from a given station - this could take 5 - 6 minutes for
# the stations with a lot of data
cmpr_station_data <- cmpr_get_station_data(
  conn,
  station_name = "Eddy Cove",
  variable_type = "temperature"
) 

station_data <- cmpr_station_data %>%
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


ss_ggplot_variables(station_data)

# save data to analyse later (e.g., calculate daily averages and export)
saveRDS(station_data, here("data/file_name.RDS"))

# if you want to save as a csv file
fwrite(station_data, "data/file_name.csv")

# end database connection
DBI::dbDisconnect(conn)
