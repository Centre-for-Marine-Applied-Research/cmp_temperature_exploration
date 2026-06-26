library(dplyr)
library(ggplot2)
library(here)
library(lubridate)
library(sensorstrings)
library(purrr)

# move this to sensorstrings
ss_extract_trimdates <- function(path) {
  r_txt <- readLines(path)
  
  start_date_txt <- strsplit(
    r_txt[grep("depl_start <- ", r_txt)], split = " <- ")[[1]][2]
  
  end_date_txt <- strsplit(
    r_txt[grep("depl_end <- ", r_txt)], split = " <- ")[[1]][2]
  
  start_date <- eval(parse(text = start_date_txt))
  end_date <- eval(parse(text = end_date_txt))
  
  data.frame(start_date = start_date, end_date = end_date)
  
}


# Moose Point 1 -------------------------------------------------------------

station <- "Moose Point 1"

station <- gsub(" ", "_", tolower(station))

depl_dates <- paste0(
  "R:/data_branches/water_quality/station_folders/", station
) %>% 
  list.files(pattern = station) %>% 
  substr(nchar(station) + 2, nchar(station) + 11) 

trim_dates <- depl_dates[1:length(depl_dates) - 1] %>% 
  map(\(x) paste0(
    ss_import_path(station, x), "/compile_", station, "_", x, ".R")
    ) %>% 
  unlist() %>% 
  map(\(x) ss_extract_trimdates(x)) 

dat <- depl_dates[1:length(depl_dates) - 1] %>% 
  map(\(x) ss_import_path(station, x)) %>% 
  unlist() %>% 
  map2(trim_dates, \(x, y)
       ss_compile_deployment_data(x) %>% 
         filter(timestamp_utc >= y$start_date, timestamp_utc <= y$end_date)
  ) %>% 
  list_rbind()

saveRDS(dat, here("data-raw", paste0(station, ".rds")))


dat %>% 
  select(-contains("dissolved_oxygen"), 
         -contains("salinity")
  ) %>% 
  filter(tilt_degree < 100) %>% 
  ss_ggplot_variables()

ggsave(
  here("figures", paste0(station, "_tilt.png")),
  device = "png",
  dpi = 600,
  width = 20, height = 15, unit = "cm"
)

dat %>% 
  select(-contains("dissolved_oxygen"), 
         -contains("salinity")
  ) %>% 
  ss_pivot_longer() %>% 
  filter(!(value > 100 & variable == "tilt_degree")) %>% 
  ss_ggplot_variables()

ggsave(
  here("figures", paste0(station, "_tilt2.png")),
  device = "png",
  dpi = 600,
  width = 20, height = 15, unit = "cm"
)


# for center bay
# ignore_sensors = c(20405703, 20397532, 20397526)

# Center Bay -------------------------------------------------------------
  
station <- "Center Bay"

station <- gsub(" ", "_", tolower(station))

depl_dates <- paste0(
  "R:/data_branches/water_quality/station_folders/", station
) %>% 
  list.files(pattern = station) %>% 
  substr(nchar(station) + 2, nchar(station) + 11) 

depl_dates <- depl_dates[1:length(depl_dates) - 1]
depl_dates <- depl_dates[-1]

trim_dates <- depl_dates %>% 
  map(\(x) paste0(
    ss_import_path(station, x), "/compile_", station, "_", x, ".R")
  ) %>% 
  unlist() %>% 
  map(\(x) ss_extract_trimdates(x)) 

dat <- depl_dates %>% 
  map(\(x) ss_import_path(station, x)) %>% 
  unlist() %>% 
  map2(trim_dates, \(x, y)
       ss_compile_deployment_data(x) %>% 
         filter(timestamp_utc >= y$start_date, timestamp_utc <= y$end_date)
  ) %>% 
  list_rbind()

# depl_date <- "2018-08-07"
# path <- ss_import_path(station, depl_date)
# dat2 <- ss_compile_deployment_data(path, ignore_sensors = c(20405703, 20397532, 20397526)) %>% 
#   filter(
#     timestamp_utc >= as_datetime("2018-08-07 23:30:00"), 
#     timestamp_utc <= as_datetime('2019-04-18 15:10:00')
#   )

# dat <- bind_rows(dat, dat2) %>% 

saveRDS(dat, here("data-raw", paste0(station, ".rds")))

p <- dat %>% 
  select(-contains("dissolved_oxygen"), 
         -contains("salinity"), -contains("depth_measured")
  ) %>% 
  ss_ggplot_variables()

library(plotly)
ggplotly(p)

ggsave(
  here("figures", paste0(station, "_tilt.png")),
  device = "png",
  dpi = 600,
  width = 20, height = 15, unit = "cm"
)



# Spry Harbour -------------------------------------------------------------

station <- "Spry Harbour"

station <- gsub(" ", "_", tolower(station))

depl_dates <- paste0(
  "R:/data_branches/water_quality/station_folders/", station
) %>% 
  list.files(pattern = station) %>% 
  substr(nchar(station) + 2, nchar(station) + 11) 

trim_dates <- depl_dates[1:length(depl_dates) - 1] %>% 
  map(\(x) paste0(
    ss_import_path(station, x), "/compile_", station, "_", x, ".R")
  ) %>% 
  unlist() %>% 
  map(\(x) ss_extract_trimdates(x)) 

dat <- depl_dates[1:length(depl_dates) - 2] %>% 
  map(\(x) ss_import_path(station, x)) %>% 
  unlist() %>% 
  map2(trim_dates, \(x, y)
       ss_compile_deployment_data(x) %>% 
         filter(timestamp_utc >= y$start_date, timestamp_utc <= y$end_date)
  ) %>% 
  list_rbind()


depl_date <- "2024-09-12"

path <- ss_import_path(station, depl_date)
dat2 <- ss_compile_deployment_data(path, ignore_sensors = 20820351) %>% 
  filter(
    timestamp_utc >= as_datetime("2024-09-12 14:45:01"), 
    timestamp_utc <= as_datetime("2025-09-24 14:24:14")
  )


dat <- bind_rows(dat, dat2)

saveRDS(dat, here("data-raw", paste0(station, ".rds")))


dat %>% 
  select(-contains("dissolved_oxygen"), 
         -contains("salinity")
  ) %>% 
  ss_ggplot_variables()

ggsave(
  here("figures", paste0(station, "_tilt.png")),
  device = "png",
  dpi = 600,
  width = 20, height = 15, unit = "cm"
)

# Birchy Head -------------------------------------------------------------

station <- "Birchy Head"

station <- gsub(" ", "_", tolower(station))

depl_dates <- paste0(
  "R:/data_branches/water_quality/station_folders/", station
) %>% 
  list.files(pattern = station) %>% 
  substr(nchar(station) + 2, nchar(station) + 11) 

depl_dates <- depl_dates[1:length(depl_dates) - 1]
depl_dates <- depl_dates[-9]

trim_dates <- depl_dates %>% 
  map(\(x) paste0(
    ss_import_path(station, x), "/compile_", station, "_", x, ".R")
  ) %>% 
  unlist() %>% 
  map(\(x) ss_extract_trimdates(x)) 

dat <- depl_dates %>% 
  map(\(x) ss_import_path(station, x)) %>% 
  unlist() %>% 
  map2(trim_dates, \(x, y)
       ss_compile_deployment_data(x) %>% 
         filter(timestamp_utc >= y$start_date, timestamp_utc <= y$end_date)
  ) %>% 
  list_rbind()


depl_date <- "2022-11-03"

path <- ss_import_path(station, depl_date)
dat2 <- ss_compile_deployment_data(path, ignore_sensors = 600008) %>% 
  filter(
    timestamp_utc >= as_datetime("2022-11-03 17:30:00"), 
    timestamp_utc <= as_datetime('2023-05-15 13:45:00')
  )


dat3 <- bind_rows(dat, dat2) %>% 
  ss_pivot_longer() %>% 
  filter(!(variable == "temperature_degree_c" & value < -5)) %>% 
  ss_pivot_wider()


saveRDS(dat3, here("data-raw", paste0(station, ".rds")))


dat3 %>% 
  select(-contains("dissolved_oxygen"), 
         -contains("salinity")
  ) %>% 
  ss_ggplot_variables()

ggsave(
  here("figures", paste0(station, "_tilt.png")),
  device = "png",
  dpi = 600,
  width = 20, height = 15, unit = "cm"
)


dat3 %>% 
  select(-contains("dissolved_oxygen"), 
         -contains("salinity")
  ) %>% 
  ss_pivot_longer() %>% 
  filter(!(value > 100 & variable == "tilt_degree")) %>% 
  ss_ggplot_variables()

ggsave(
  here("figures", paste0(station, "_tilt2.png")),
  device = "png",
  dpi = 600,
  width = 20, height = 15, unit = "cm"
)


# Big Pond Point ----------------------------------------------------------

station <- "Big Pond Point"

station <- gsub(" ", "_", tolower(station))

depl_dates <- paste0(
  "R:/data_branches/water_quality/station_folders/", station
) %>% 
  list.files(pattern = station) %>% 
  substr(nchar(station) + 2, nchar(station) + 11) 

depl_dates <- depl_dates[1:length(depl_dates) - 1]
depl_dates <- depl_dates[-c(2, 3, 5)]

trim_dates <- depl_dates %>% 
  map(\(x) paste0(
    ss_import_path(station, x), "/compile_", station, "_", x, ".R")
  ) %>% 
  unlist() %>% 
  map(\(x) ss_extract_trimdates(x)) 

dat <- depl_dates %>% 
  map(\(x) ss_import_path(station, x)) %>% 
  unlist() %>% 
  map2(trim_dates, \(x, y)
       ss_compile_deployment_data(x) %>% 
         filter(timestamp_utc >= y$start_date, timestamp_utc <= y$end_date)
  ) %>% 
  list_rbind()


depl_date <- "2021-11-09"
path <- ss_import_path(station, depl_date)
dat2 <- ss_compile_deployment_data(path, ignore_sensors = 671021) %>% 
  filter(
    timestamp_utc >= as_datetime("2021-11-09 14:00:00"), 
    timestamp_utc <= as_datetime('2022-06-21 11:16:00')
  )


depl_date <- "2022-06-21"
path <- ss_import_path(station, depl_date)
dat3 <- ss_compile_deployment_data(path, ignore_sensors = 671046) %>% 
  filter(
    timestamp_utc >= as_datetime("2022-06-21 12:00:00"), 
    timestamp_utc <= as_datetime('2023-08-03 11:21:40')
  )


dat4 <- bind_rows(dat, dat2) %>% 
  bind_rows(dat3) 

saveRDS(dat4, here("data-raw", paste0(station, ".rds")))


dat4 %>% 
  select(-contains("dissolved_oxygen"), 
         -contains("salinity")
  ) %>% 
  ss_ggplot_variables()

ggsave(
  here("figures", paste0(station, "_tilt.png")),
  device = "png",
  dpi = 600,
  width = 20, height = 15, unit = "cm"
)


dat4 %>% 
  select(-contains("dissolved_oxygen"), 
         -contains("salinity")
  ) %>% 
  ss_pivot_longer() %>% 
  filter(!(value > 100 & variable == "tilt_degree")) %>% 
  ss_ggplot_variables()

ggsave(
  here("figures", paste0(station, "_tilt2.png")),
  device = "png",
  dpi = 600,
  width = 20, height = 15, unit = "cm"
)


# Wedgeport ---------------------------------------------------------------

station <- "Wedgeport"

station <- gsub(" ", "_", tolower(station))

depl_dates <- paste0(
  "R:/data_branches/water_quality/station_folders/", station
) %>% 
  list.files(pattern = station) %>% 
  substr(nchar(station) + 2, nchar(station) + 11) 

depl_dates <- depl_dates[1:length(depl_dates) - 1]
depl_dates <- depl_dates[-c(2, 3, 5)]

trim_dates <- depl_dates %>% 
  map(\(x) paste0(
    ss_import_path(station, x), "/compile_", station, "_", x, ".R")
  ) %>% 
  unlist() %>% 
  map(\(x) ss_extract_trimdates(x)) 

dat <- depl_dates %>% 
  map(\(x) ss_import_path(station, x)) %>% 
  unlist() %>% 
  map2(trim_dates, \(x, y)
       ss_compile_deployment_data(x) %>% 
         filter(timestamp_utc >= y$start_date, timestamp_utc <= y$end_date)
  ) %>% 
  list_rbind()


saveRDS(dat, here("data-raw", paste0(station, ".rds")))


dat %>% 
  select(-contains("dissolved_oxygen"), 
         -contains("salinity")
  ) %>% 
  ss_ggplot_variables()

ggsave(
  here("figures", paste0(station, "_tilt.png")),
  device = "png",
  dpi = 600,
  width = 20, height = 15, unit = "cm"
)

