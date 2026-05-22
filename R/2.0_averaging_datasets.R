
library(data.table)     # export csv file
library(dplyr)          # wrangling data
library(here)           # file paths
library(lubridate)      # dates & times
library(plotly)         # interactive figures
library(sensorstrings)  # plot data

library(tidyverse)

data_1.0 <- readRDS("data/data_1.0.rds")

round_unit <- "24 hours"

# averaging by desired amount of time (indicated in round_unit) --------------
data_avg <- data_1.0 %>% 
  filter(qc_flag_value != "Fail") %>% 
  mutate(ts_round = round_date(timestamp_utc, unit = round_unit)) %>%
  summarise(
    value_avg = mean(value), .by = c(station, sensor_depth_at_low_tide_m, ts_round)
  )

# calculating stratification indices between 2m-5m and 2m-10m ----------------

new_data <- data_avg %>% 
  pivot_wider(names_from = sensor_depth_at_low_tide_m, values_from = value_avg)

new_data <- new_data %>% 
  rename(depth_2 = `2`,depth_5 = `5`, depth_10 = `10`) %>%
  mutate(Index.2_5 = depth_2 - depth_5, 
         Index.2_10 = depth_2 - depth_10,
         Index.5_10 = depth_5 - depth_10)

#save as RDS file
#saveRDS(full_data_set, file = "R/data")


# to export data
file_name <- paste0("2.0_averaging_dataset_",  gsub(" ", "_", round_unit), ".RDS")

saveRDS(new_data, file = here("data", file_name))
