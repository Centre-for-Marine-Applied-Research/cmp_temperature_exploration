
# list all of the climate station data folders (i.e., remove folders that do not hold
# met data)
list_climate_data_folders <- function(path) {
  dat_folders <- list.dirs(here("data-raw/"), full.names = TRUE, recursive = FALSE)
  dat_folders <- dat_folders[-which(grepl("ast", dat_folders))]
  dat_folders <- dat_folders[-which(grepl("cygwin", dat_folders))]
}

# read in all files in path and bind them together
fread_climate_data_files <- function(path) {
  map_df(
    .x = list.files(path, full.names = TRUE), .f = fread, fill = TRUE, 
    data.table = FALSE
  ) 
}



fread_water_level_files <- function(path) {
  
  dat <- fread(path, header = FALSE, data.table = FALSE)
  
  station_i <- dat[1, 2]
  latitude_i <- as.numeric(dat[3, 2])
  longitude_i <- as.numeric(dat[4,2])
  tz_i <- dat[5,2]
  
  if(tz_i != "UTC") warning("time zone is in ", tz_i)
  
  dat %>% 
    slice(-c(1:7)) %>% 
    mutate(
      station = station_i,
      latitude = latitude_i,
      longitude = longitude_i,
      timestamp_utc = parse_date_time(V1, orders = "%Y/%m/%d %h:%M"),
      water_level_m = as.numeric(V2) 
    ) %>% 
    select(-c(V1, V2))
  
}
  
  
  

# calculate met data averages
# https://www.ndbc.noaa.gov/faq/wndav.shtml#:~:text=The%20average%20wind%20speed%20is,the%20orientation%20of%20the%20vector.

# dat: data frame with columns xyz
# wind_vars: character vector of wind variable (speed and direction). Vector average
# will be calculated for these variables. Wind direction must be in degrees
# round_int: time interval for each bin, e.g., "2 hours", "24 hours", "1 day"

calculate_binned_averages <- function(
    dat, 
    round_int = "2 hours",
    wind_vars = c("wind_speed_m_s", "wind_from_direction_degree")
) {
  
  dat <- dat %>% 
    mutate(
      round_timestamp_utc = round_date(timestamp_utc, unit = round_int)
    ) 
  
  scalar_avg <- dat %>% 
    filter(!variable %in% wind_vars) %>% 
    summarise(
      value = mean(value), 
      n = n(),
      .by = c(station, variable, round_timestamp_utc)
    ) 
  
  if(all_of(wind_vars %in% dat$variable)) {
  # wind speed & direction
  vec_avg <- dat %>% 
    filter(variable %in% wind_vars) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>% 
    rename(
      wind_direction = contains("direction"), wind_speed = contains("speed")
    ) %>% 
    # when wind speed = 0, direction = NA
    filter(!is.na(wind_direction)) %>% 
    mutate(
      u = wind_speed * sin(wind_direction * pi / 180),
      v = wind_speed * cos(wind_direction * pi / 180)
    ) %>% 
    summarise(
      u_mean = mean(u),
      v_mean = mean(v), 
      n = n(),
      .by = c(station, round_timestamp_utc)
    ) %>% 
    mutate(
      wind_direction = round(
        atan2(u_mean, v_mean) * 180 / pi, digits = 2
      ),
      # maps from (-180 to 180) to (0, 359)
      wind_direction = (360 + wind_direction) %% 360,
      
      wind_speed = round(sqrt(u_mean^2 + v_mean^2), digits = 2)
    ) %>% 
    pivot_longer(
      cols = c("wind_speed", "wind_direction"),
      values_to = "value", names_to = "variable"
    ) %>% 
    mutate(
      variable = case_when(
        variable == "wind_direction" ~ wind_vars[grep("direction", wind_vars)],
        variable == "wind_speed" ~ wind_vars[grep("speed", wind_vars)],
        TRUE ~ NA
      )
    )
  } else vec_avg = data.frame(NULL)
  
  scalar_avg %>% 
    bind_rows(vec_avg) %>% 
    rename(timestamp_utc = round_timestamp_utc) %>% 
   # select(-c(u_mean, v_mean)) %>% 
    arrange(station, timestamp_utc)
  
}

# add checks here