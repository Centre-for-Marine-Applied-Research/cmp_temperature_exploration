
library(DT)
library(ggplot2)
library(readr)
library(data.table)     # export csv file
library(dplyr)          # wrangling data
library(here)           # file paths
library(lubridate)      # dates & times
library(plotly)         # interactive figures
library(sensorstrings)  # plot data
library(rmarkdown)

library(htmltools)
library(RColorBrewer)
library(stringr)
library(tidyr)

library(paletteer)






# Plotting air and water temp together ------------------------------------
data24 <- readRDS(("all_data/data/data_24_hours.RDS")) %>% 
  mutate(station=ordered(station, levels=c("Big Pond Point", "Wedgeport", "Birchy Head", "Spry Harbour", "Center Bay", "Moose Point 1"))) %>% 
  mutate(
    station = case_when(
      station == "Moose Point 1" ~ "1 - Moose Point 1", 
      station == "Center Bay" ~ "2 - Center Bay",
      station == "Spry Harbour" ~ "3 - Spry Harbour",
      station == "Birchy Head" ~ "4 - Birchy Head",
      station == "Wedgeport" ~ "5 - Wedgeport",
      station == "Big Pond Point" ~ "6 - Big Pond Point"
    ))


data_temps <- data24 %>% 
  filter(ts_round >= as.Date("2022-05-15 00:00:00") & 
           ts_round <= as.Date("2022-10-15 00:00:00")) %>% 
  pivot_longer(
    cols = c(depth_2, depth_5, depth_10, avg.air.temp),
    names_to = "variable",
    values_to = "value")


# plotting the data
p <- data_temps %>%
  ggplot(aes(x=ts_round, y=value, colour = variable)) +
  geom_line(linewidth =0.8) +
  scale_color_paletteer_d("vangogh::Chaise") +
  facet_wrap(~station, ncol = 2, shrink = FALSE) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) 
  

plot(p)


# plotting pressure -------------------------------------------------------

data <- readRDS("data/climate_data_24_hours.rds") %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  select(station, timestamp_utc, pressure_kpa, air_temperature_degree_c) %>% 
  na.omit()
  

data_pressure <- data %>% 
  filter(timestamp_utc >= as.Date("2020-01-01 00:00:00") & 
           timestamp_utc <= as.Date("2020-12-31 00:00:00"))


# plotting the data
p <- data_pressure %>%
  ggplot(aes(x=timestamp_utc, y=pressure_kpa)) +
  geom_line(linewidth =0.8) +
  scale_color_paletteer_d("vangogh::Chaise") +
  facet_wrap(~station, ncol = 2, shrink = FALSE) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  
  geom_vline(xintercept = as.Date("2020-08-13 00:00:00"), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-08-20 00:00:00"), color = "red", linetype = "solid") +
  geom_vline(xintercept = as.Date("2020-08-29 00:00:00"), color = "blue", linetype = "solid") +
  geom_vline(xintercept = as.Date("2020-09-06 00:00:00"), color = "red", linetype = "dashed")
  
  

plot(p)



# tidal height  ------------------------------------------------------------


tidal_data <- readRDS("data/water_level_data.rds") %>% 
  filter(timestamp_utc >= as.Date("2022-08-01 00:00:00") & 
           timestamp_utc <= as.Date("2022-08-20 00:00:00"))


p <- tidal_data %>%
  ggplot(aes(x=timestamp_utc, y=water_level_m)) +
  geom_line(linewidth =0.8) +
  
  geom_vline(xintercept = as.Date("2022-08-09 00:00:00"), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-08-20 00:00:00"), color = "red", linetype = "dashed") +
  
  scale_color_viridis_d(option = "turbo") +
  facet_wrap(~station, ncol = 1, shrink = FALSE) +
  theme(legend.position = "bottom") +
  labs(
    x = "Time",
    y = "Water level (meters)",
  )

plot(p)

#---------------------------------------------------------

round_unit = "2 hours"

tidal_data_round <- readRDS("data/water_level_data.rds") %>% 
  mutate(ts_round = round_date(timestamp_utc, unit = round_unit)) %>%
  summarise(
    water_avg = mean(water_level_m), .by = c(station, ts_round)) %>% 
  filter(ts_round >= as.Date("2022-06-11 00:00:00") & 
           ts_round <= as.Date("2022-06-25 00:00:00"))


p <- tidal_data_round %>%
  ggplot(aes(x=ts_round, y=water_avg)) +
  geom_line(linewidth =0.8) +
  scale_color_viridis_d(option = "turbo") +
  facet_wrap(~station, ncol = 1, shrink = FALSE) +
  theme(legend.position = "bottom") +
  labs(
    x = "Time",
    y = "Water level (meters)",
  )

plot(p)

# wind rose plots ---------------------------------------------------------



# wind arrow diagram ------------------------------------------------------






# stratification index ----------------------------------------------------
# see file 3.0_Temp_plots_and_stratification_index


# Hov-moller diagram -------------------------------------------------------
# see file 7.0_Hovmoller_diagrams







