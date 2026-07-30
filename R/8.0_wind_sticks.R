library(ggplot2)
library(oce)
library(lubridate)
library(dplyr)
library(here)
library(tidyr)
library(signal)


# reading and cleaning the data  ------------------------------------------

data <- readRDS("data/climate_data_2_hours.rds") 

extra <- data %>% select(station, timestamp_utc, u_mean, v_mean) %>%
  na.omit(c(u_mean, v_mean))

extra <- unique(extra)

data <- pivot_wider(data, id_cols = c(station,timestamp_utc), 
                    names_from = "variable", 
                    values_from = "value") %>% 
  select(station, timestamp_utc, wind_from_direction_degree, 
         wind_speed_m_s, pressure_kpa) 



data <- merge(data, extra)


# filtering by time and station ------------------------------------------------

data <- data %>% 
  dplyr::filter((timestamp_utc >= as.Date("2022-01-15 00:00:00") & 
          timestamp_utc <= as.Date("2022-02-01 00:00:00")))

data <- data %>% dplyr::filter(station == "LUNENBURG")



# raw ----------------------------------

t <- data$timestamp_utc
u <- data$u_mean
v <- data$v_mean

plotSticks(t, 0, u, v, yscale = 25, length = 0)

# smoothed and filtered -----------------

bw <- butter(1, 0.1) # low pass with normalized cutoff of 0.1
uf <- filtfilt(bw, u)
vf <- filtfilt(bw, v)

plotSticks(t, 0, uf, vf, yscale = 25, length = 0)


# example data ------------------------------------------------------------

data(met)
t <- met[["time"]]
u <- met[["u"]]
v <- met[["v"]]
p <- met[["pressure"]]


oce.plot.ts(t, p)
plotSticks(t, 100, u, v, yscale = 25, add = TRUE)


# # -----------------------------------------------------------------------
#saveRDS(climate_data_1_hours, file = "data/climate_data_1_hours.rds")
#saveRDS(climate_data_2_hours, file = "data/climate_data_2_hours.rds")
#saveRDS(climate_data_4_hours, file = "data/climate_data_4_hours.rds")
#saveRDS(climate_data_6_hours, file = "data/climate_data_6_hours.rds")
#saveRDS(climate_data_12_hours, file = "data/climate_data_12_hours.rds")
#saveRDS(climate_data_24_hours, file = "data/climate_data_24_hours.rds")

#saveRDS(water_level_data, file = "data/water_level_data.rds")
