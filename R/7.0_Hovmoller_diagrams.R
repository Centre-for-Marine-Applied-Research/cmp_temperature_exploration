library(ggplot2)
library(dplyr)
library(tidyr)
library(here)           # file paths
library(lubridate)

library(stxplore)
library(rasterVis)
library(forcats)

# ## ----------------------------------------------------------------------

#1.0 accurate lats and lons
data24 <- readRDS(("all_data/data/data_24_hours.RDS")) %>% 

mutate( 
       latitude = case_when(
         station == "Big Pond Point" ~ 43.77039,
         station == "Spry Harbour" ~ 44.81793,
         station == "Birchy Head" ~ 44.57001,
         station == "Moose Point 1" ~ 45.39500,
         station == "Center Bay" ~ 45.22565,
         station == "Wedgeport" ~ 43.69402,
         TRUE                 ~ NA  # Default value for anything else
       ), 
       longitude = case_when(
         station == "Big Pond Point" ~ -66.13518,
         station == "Spry Harbour" ~ -62.61899,
         station == "Birchy Head" ~ -64.03446,
         station == "Moose Point 1" ~ -61.40951,
         station == "Center Bay" ~ -61.29616,
         station == "Wedgeport" ~ -65.95942,
         TRUE                 ~ NA  # Default value for anything else
       )) %>% 
  
  select(station, ts_round, depth_2, depth_5, depth_10, latitude, longitude) %>% 
  
  mutate(
    station = case_when(
      station == "Moose Point 1" ~ "1 - Moose Point 1", 
      station == "Center Bay" ~ "2 - Center Bay",
      station == "Spry Harbour" ~ "3 - Spry Harbour",
      station == "Birchy Head" ~ "4 - Birchy Head",
      station == "Wedgeport" ~ "5 - Wedgeport",
      station == "Big Pond Point" ~ "6 - Big Pond Point"
    )) 
  
  
  
#____________________________________
#1.1 rounded lats and lons.. 
data24_broad <- readRDS(("all_data/data/data_24_hours.RDS")) %>% 
  
  mutate( 
    latitude = case_when(
      station == "Big Pond Point" ~ 44,
      station == "Spry Harbour" ~ 45,
      station == "Birchy Head" ~ 45,
      station == "Moose Point 1" ~ 45,
      station == "Center Bay" ~ 45,
      station == "Wedgeport" ~ 44,
      TRUE                 ~ NA  # Default value for anything else
    ), 
    longitude = case_when(
      station == "Big Pond Point" ~ -66,
      station == "Spry Harbour" ~ -63,
      station == "Birchy Head" ~ -64,
      station == "Moose Point 1" ~ -62,
      station == "Center Bay" ~ -61,
      station == "Wedgeport" ~ -66,
      TRUE                 ~ NA  # Default value for anything else
    )) %>% 
  
  select(station, ts_round, depth_2, depth_5, depth_10, latitude, longitude) %>% 
  
  mutate(
    station = case_when(
      station == "Moose Point 1" ~ "1 - Moose Point 1", 
      station == "Center Bay" ~ "2 - Center Bay",
      station == "Spry Harbour" ~ "3 - Spry Harbour",
      station == "Birchy Head" ~ "4 - Birchy Head",
      station == "Wedgeport" ~ "5 - Wedgeport",
      station == "Big Pond Point" ~ "6 - Big Pond Point"
    )) 

#_______________________________________________________________________________

#2.  removing nas and grouping and filtering _> by latitude
data_lat <- data24 %>%
  group_by(latitude, ts_round) %>%
  summarize(mean_d2 = mean(depth_2, na.rm = TRUE)) %>% 
  dplyr::filter(ts_round >= as.Date("2020-08-01 00:00:00") & 
                  ts_round <= as.Date("2020-08-31 00:00:00"))


#2.1 removing nas and grouping and filtering _> by station name
data_stat <- data24 %>% 
  group_by(station, ts_round) %>% 
  summarize(mean_d2 = mean(depth_10, na.rm = TRUE)) %>% 
  dplyr::filter(ts_round >= as.Date("2022-05-15 00:00:00") & 
           ts_round <= as.Date("2022-10-15 00:00:00"))
 

#3. plotting the data

#specify which one you want - _lat or _stat
data <- data_stat

# change y to station or latitude 

ggplot(data, aes(x = ts_round, y = station, fill = mean_d2)) +
  geom_tile(interpolate = TRUE) +
  scale_fill_viridis_c() +
  theme(
    axis.text.x = element_text(size = 12), # Change X-axis marker size
    axis.text.y = element_text(size = 12),  # Change Y-axis marker size
    legend.key.size = unit(1, "cm") # Changes labels font size
  )






