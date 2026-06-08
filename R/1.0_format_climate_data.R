# June 8, 2026
# May 19, 2026

# hourly meteorlogical data from https://climate.weather.gc.ca/index_e.html
# downloaded using cygwin
# technical details here: https://climate.weather.gc.ca/doc/Technical_Documentation.pdf


# From website:
# binned into 2-hour intervals
# took average of 2-hours intervals
# use vector averages for wind speed and direction

library(data.table)
library(dplyr)
library(here)
library(lubridate)
library(purrr)
library(tidyr)

source(here("functions/helpers.R"))

round_int <- "2 hours"

# import data -------------------------------------------------------------

dat_folders <- list_climate_data_folders()

dat <- map_df(.x = dat_folders, .f = foo)

# # beaver island
# beav <- map_df(
#   .x = list.files(here("data-raw/beaver_island_(aut)"), full.names = TRUE), 
#   .f = fread, fill = TRUE
# ) 
# 
# # hart_island_(aut) 
# hart <- map_df(
#   .x = list.files(here("data-raw/hart_island_(aut)"), full.names = TRUE), 
#   .f = fread, fill = TRUE
# ) 
# 
# # lunenburg met data 
# lune <- map_df(
#   .x = list.files(here("data-raw/lunenburg"), full.names = TRUE), 
#   .f = fread, fill = TRUE
# ) 
# 
# # yarmouth_rcs
# yar <- map_df(
#   .x = list.files(here("data-raw/yarmouth_rcs"), full.names = TRUE), 
#   .f = fread, fill = TRUE
# ) 


# format ------------------------------------------------------------------

met_dat <- dat %>% 
  select(
    longitude = `Longitude (x)`,
    latitude = `Latitude (y)`,
    station = `Station Name`,
    climate_id = `Climate ID`,
    timestamp_utc = `Date/Time (UTC)`,
    air_temperature = `Temp (°C)`,
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
    timestamp_utc = as_datetime(timestamp_utc)
  ) %>% 
  pivot_longer(
    cols = air_temperature:wind_chill, values_to = "value", names_to = "variable"
  ) %>% 
  filter(!is.na(value)) %>% 
  mutate(
    round_timestamp_utc = round_date(timestamp_utc, unit = round_int)
  ) 

# average -----------------------------------------------------------------

scalar_avg <- met_dat %>% 
  filter(
    !variable %in% c("wind_speed_km_per_hour", "wind_direction_10_degree")
  ) %>% 
 summarise(
    value = mean(value), 
    n = n(),
    .by = c(station, variable, round_timestamp_utc)
  ) 

# wind speed & direction
vec_avg <- met_dat %>% 
  filter(
    variable %in% c("wind_speed_km_per_hour", "wind_direction_10_degree")
  ) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  mutate(
    wind_direction_degree = wind_direction_10_degree * 10,
    
    v_x = wind_speed_km_per_hour * sin(wind_direction_degree * pi / 180),
    v_y = wind_speed_km_per_hour * cos(wind_direction_degree * pi / 180)
  ) %>% 
  summarise(
    v_x_mean = mean(v_x),
    v_y_mean = mean(v_y), 
    n = n(),
    .by = c(station, round_timestamp_utc)
  ) %>% 
  mutate(
    wind_direction_degree = round(
      atan2(v_x_mean, v_y_mean) * 180/pi, digits = 2
    ),
    # maps from (-180 to 180) to (0, 359)
    wind_direction_degree = (360 + wind_direction_degree) %% 360,
    
    wind_speed_km_per_hour = round(sqrt(v_y_mean^2 + v_x_mean^2), digits = 2)
  ) %>% 
  pivot_longer(
    cols = c("wind_speed_km_per_hour", "wind_direction_degree"),
    values_to = "value", names_to = "variable"
  ) 


met_dat_avg <- scalar_avg %>% 
  bind_rows(vec_avg) %>% 
  rename(timestamp_utc = round_timestamp_utc) %>% 
  select(-c(v_x_mean, v_y_mean)) %>% 
  arrange(station, timestamp_utc)


# station locations -------------------------------------------------------

st_locations <- met_dat %>% 
  distinct(station, latitude, longitude)

# export ------------------------------------------------------------------

fwrite(
  met_dat_avg, 
  here(paste0("data/climate_data_", gsub(" ", "_", round_int),  ".csv"))
)


fwrite(
  st_locations, 
  here(paste0("data/climate_station_locations.csv"))
)

