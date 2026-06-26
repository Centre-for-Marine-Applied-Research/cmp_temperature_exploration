# June 24, 2026
# June 8, 2026
# May 19, 2026

# hourly meteorlogical data from https://climate.weather.gc.ca/index_e.html
# downloaded using cygwin
# technical details here: https://climate.weather.gc.ca/doc/Technical_Documentation.pdf

# import data
# format column names (variable_units) 
# convert wind speed and direction to proper units
# add coastline angle column: (330 for Yarmouth, 60 for the other stations)
# calculate bui
# export -> averages will be calculated elsewhere

# bui is calculated here so it is calculated at the most granular level,
# ie. before averaging wind speed and direction (following Socrasi 2025)

library(data.table)
library(dplyr)
library(here)
library(lubridate)
library(purrr)
library(tidyr)

source(here("functions/helpers.R"))
source(here("functions/calculate_bui.R"))

# import data -------------------------------------------------------------

dat_folders <- list_climate_data_folders()

dat <- map_df(.x = dat_folders, .f = fread_data_files)

# format ------------------------------------------------------------------

met_dat <- dat %>% 
  select(
    longitude = `Longitude (x)`,
    latitude = `Latitude (y)`,
    station = `Station Name`,
    climate_id = `Climate ID`,
    timestamp_utc = `Date/Time (UTC)`,
    air_temperature_degree_c = `Temp (°C)`,
    dew_point_temperature =  `Dew Point Temp (°C)`,
    relative_humidity_percent = `Rel Hum (%)`,
    precipitation_mm = `Precip. Amount (mm)`,
    wind_direction_10_degree = `Wind Dir (10s deg)`,
    wind_speed_km_per_hour = `Wind Spd (km/h)`,
    # visibility_km = `Visibility (km)`,
    pressure_kpa = `Stn Press (kPa)`,
    humidex = Hmdx,
    wind_chill = `Wind Chill`
    # weather = Weather
  ) %>% 
  mutate(
    timestamp_utc = if_else(
      nchar(timestamp_utc) == 16, paste0(timestamp_utc, ":00"), timestamp_utc),
    timestamp_utc = as_datetime(timestamp_utc),
    
    # convert wind data to correct units
    wind_from_direction_degree = wind_direction_10_degree * 10,
    wind_speed_m_s = round(wind_speed_km_per_hour * 1000 / 3600, digits = 3),
    
    coastline_angle_degree = if_else(station == "YARMOUTH RCS", 330, 60),
    
    # calculate upwelling index
    bui_m3_s = calculate_bui(
      wind_from_direction_degree = wind_from_direction_degree,
      wind_speed_m_s = wind_speed_m_s,
      coastline_angle_degree = coastline_angle_degree, 
      latitude_decimal_degree = latitude
    )
  ) %>% 
  select(-c(wind_direction_10_degree, wind_speed_km_per_hour)) %>% 
  pivot_longer(
    cols = 6:16, values_to = "value", names_to = "variable"
  ) %>% 
  filter(!is.na(value)) 

# average & export ------------------------------------------------------------------

met_dat %>% 
  fwrite(here(paste0("data/climate_data_1_hours.csv")))

met_dat %>% 
  calculate_binned_averages(round_int = "2 hours") %>% 
  fwrite(here(paste0("data/climate_data_2_hours.csv")))

met_dat %>% 
  calculate_binned_averages(round_int = "4 hours") %>% 
  fwrite(here(paste0("data/climate_data_4_hours.csv")))

met_dat %>% 
  calculate_binned_averages(round_int = "6 hours") %>% 
  fwrite(here(paste0("data/climate_data_6_hours.csv")))

met_dat %>% 
  calculate_binned_averages(round_int = "12 hours") %>% 
  fwrite(here(paste0("data/climate_data_12_hours.csv")))

met_dat %>% 
  calculate_binned_averages(round_int = "24 hours") %>% 
  fwrite(here(paste0("data/climate_data_24_hours.csv")))

# station locations -------------------------------------------------------

st_locations <- met_dat %>% 
  distinct(station, latitude, longitude)

fwrite(
  st_locations, 
  here(paste0("data/climate_station_locations.csv"))
)



# # average -----------------------------------------------------------------
# 
# scalar_avg <- met_dat %>% 
#   filter(
#     !variable %in% c("wind_speed_km_per_hour", "wind_direction_10_degree")
#   ) %>% 
#  summarise(
#     value = mean(value), 
#     n = n(),
#     .by = c(station, variable, round_timestamp_utc)
#   ) 
# 
# # wind speed & direction
# vec_avg <- met_dat %>% 
#   filter(
#     variable %in% c("wind_speed_km_per_hour", "wind_direction_10_degree")
#   ) %>%
#   pivot_wider(names_from = "variable", values_from = "value") %>% 
#   mutate(
#     wind_from_direction_degree = wind_direction_10_degree * 10,
#     
#     v_x = wind_speed_km_per_hour * sin(wind_from_direction_degree * pi / 180),
#     v_y = wind_speed_km_per_hour * cos(wind_from_direction_degree * pi / 180)
#   ) %>% 
#   summarise(
#     v_x_mean = mean(v_x),
#     v_y_mean = mean(v_y), 
#     n = n(),
#     .by = c(station, round_timestamp_utc)
#   ) %>% 
#   mutate(
#     wind_from_direction_degree = round(
#       atan2(v_x_mean, v_y_mean) * 180 / pi, digits = 2
#     ),
#     # maps from (-180 to 180) to (0, 359)
#     wind_from_direction_degree = (360 + wind_from_direction_degree) %% 360,
#     
#     wind_speed_km_per_hour = round(sqrt(v_y_mean^2 + v_x_mean^2), digits = 2)
#   ) %>% 
#   pivot_longer(
#     cols = c("wind_speed_km_per_hour", "wind_from_direction_degree"),
#     values_to = "value", names_to = "variable"
#   ) 
# 
# 
# met_dat_avg <- scalar_avg %>% 
#   bind_rows(vec_avg) %>% 
#   rename(timestamp_utc = round_timestamp_utc) %>% 
#   select(-c(v_x_mean, v_y_mean)) %>% 
#   arrange(station, timestamp_utc)
# 

