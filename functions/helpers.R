
# list all of the climate station data folders (i.e., remove folders that do not hold
# met data)
list_climate_data_folders <- function(path) {
  dat_folders <- list.dirs(here("data-raw/"), full.names = TRUE, recursive = FALSE)
  dat_folders <- dat_folders[-which(grepl("ast", dat_folders))]
  dat_folders <- dat_folders[-which(grepl("cygwin", dat_folders))]
}

# read in all files in path and bind them together
fread_data_files <- function(path) {
  map_df(
    .x = list.files(path, full.names = TRUE), .f = fread, fill = TRUE, 
    data.table = FALSE
  ) 
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
      v_x = wind_speed * sin(wind_direction * pi / 180),
      v_y = wind_speed * cos(wind_direction * pi / 180)
    ) %>% 
    summarise(
      v_x_mean = mean(v_x),
      v_y_mean = mean(v_y), 
      n = n(),
      .by = c(station, round_timestamp_utc)
    ) %>% 
    mutate(
      wind_direction = round(
        atan2(v_x_mean, v_y_mean) * 180 / pi, digits = 2
      ),
      # maps from (-180 to 180) to (0, 359)
      wind_direction = (360 + wind_direction) %% 360,
      
      wind_speed = round(sqrt(v_y_mean^2 + v_x_mean^2), digits = 2)
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
  
  scalar_avg %>% 
    bind_rows(vec_avg) %>% 
    rename(timestamp_utc = round_timestamp_utc) %>% 
    select(-c(v_x_mean, v_y_mean)) %>% 
    arrange(station, timestamp_utc)
  
}

# add checks here