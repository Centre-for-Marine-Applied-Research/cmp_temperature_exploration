# May 19, 2026

# hourly meteorlogical data from https://climate.weather.gc.ca/index_e.html
# downloaded using cygwin

# From website:
# binned into 2-hour intervals
# took average of 2-hours intervals

library(data.table)
library(dplyr)
library(here)
library(lubridate)
library(purrr)
library(tidyr)


round_int <- "2 hours"

# import data -------------------------------------------------------------

# beaver island
beav <- map_df(
  .x = list.files(here("data-raw/beaver_island_(aut)"), full.names = TRUE), 
  .f = fread, fill = TRUE
) 

# hart_island_(aut) 
hart <- map_df(
  .x = list.files(here("data-raw/hart_island_(aut)"), full.names = TRUE), 
  .f = fread, fill = TRUE
) 

# lunenburg met data 
lune <- map_df(
  .x = list.files(here("data-raw/lunenburg"), full.names = TRUE), 
  .f = fread, fill = TRUE
) 

# yarmouth_rcs
yar <- map_df(
  .x = list.files(here("data-raw/yarmouth_rcs"), full.names = TRUE), 
  .f = fread, fill = TRUE
) 


# format ------------------------------------------------------------------

met_dat <- bind_rows(beav, hart, lune, yar) %>% 
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
    wind_direction_degree = `Wind Dir (10s deg)`,
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
  ) 

# average -----------------------------------------------------------------

met_dat_avg <- met_dat %>% 
  pivot_longer(
    cols = air_temperature:wind_chill, values_to = "value", names_to = "variable"
  ) %>% 
  filter(!is.na(value)) %>% 
  mutate(
    round_timestamp_utc = round_date(timestamp_utc, unit = round_int)
  ) %>% 
  summarise(
    value = mean(value), 
    n = n(),
    .by = c(station, variable, round_timestamp_utc)
  ) %>% 
  pivot_wider(
    names_from = "variable", values_from = "value"
  ) %>% 
  rename(timestamp_utc = round_timestamp_utc)


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

