
library(data.table)     # export csv file
library(dplyr)          # wrangling data
library(here)           # file paths
library(lubridate)      # dates & times
library(plotly)         # interactive figures
library(sensorstrings)  # plot data


# importing data - specify which time average you want (2h, 12h or 24)
data <- readRDS(("all_data/data/2.0_averaging_dataset_2_hours.rds")) %>% 
  mutate(
    station = case_when(
      station == "Moose Point 1" ~ "1 - Moose Point 1", 
      station == "Center Bay" ~ "2 - Center Bay",
      station == "Spry Harbour" ~ "3 - Spry Harbour",
      station == "Birchy Head" ~ "4 - Birchy Head",
      station == "Wedgeport" ~ "5 - Wedgeport",
      station == "Big Pond Point" ~ "6 - Big Pond Point"
    )) %>% 
  
  filter(ts_round >= as.Date("2022-05-15 00:00:00") & 
           ts_round <= as.Date("2022-10-15 00:00:00")) 



# stratification index ---------------------------------------------------

# difference between 2m and 10m
p <- data %>% 
  ggplot(aes(x = ts_round, y = Index.2_10)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~station, ncol = 1, shrink = FALSE) +
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 12), # Change X-axis marker size
        axis.text.y = element_text(size = 12)) + # Change Y-axis marker size) +
  
  geom_vline(xintercept = as.Date("2022-08-09 00:00:00"), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-08-20 00:00:00"), color = "red", linetype = "solid") +
  geom_vline(xintercept = as.Date("2022-08-31 00:00:00"), color = "blue", linetype = "solid") +
  geom_vline(xintercept = as.Date("2022-09-06 00:00:00"), color = "red", linetype = "dashed") +
  
  ggtitle("Stratification Index: 2m - 10m") +
  xlab("Time(UTC)") +
  ylab("Temperature(degrees C)")

plot(p)

# difference between 2m and 5m
 p <- data %>% 
   ggplot(aes(x = ts_round, y = Index.2_5)) +
   geom_line(linewidth = 0.5) +
   
   facet_wrap(~station, ncol = 1, shrink = FALSE) +
   theme(legend.position = "bottom", 
         strip.text = element_text(size = 12),
         axis.text.x = element_text(size = 12), # Change X-axis marker size
         axis.text.y = element_text(size = 12)) + # Change Y-axis marker size) +
   
   geom_vline(xintercept = as.Date("2022-08-09 00:00:00"), color = "red", linetype = "dashed") +
   geom_vline(xintercept = as.Date("2022-08-20 00:00:00"), color = "red", linetype = "solid") +
   geom_vline(xintercept = as.Date("2022-08-31 00:00:00"), color = "blue", linetype = "solid") +
   geom_vline(xintercept = as.Date("2022-09-06 00:00:00"), color = "red", linetype = "dashed") +
   
   ggtitle("Stratification Index: 2m - 5m") +
   xlab("Time(UTC)") +
   ylab("Temperature(degrees C)")
 
plot(p)
   
# difference between 5m and 10m
 p <- data %>% 
   ggplot(aes(x = ts_round, y = Index.5_10)) +
   geom_line(linewidth = 0.5) +
   
   facet_wrap(~station, ncol = 1, shrink = FALSE) +
   theme(legend.position = "bottom", 
         strip.text = element_text(size = 12),
           axis.text.x = element_text(size = 12), # Change X-axis marker size
           axis.text.y = element_text(size = 12)) + # Change Y-axis marker size) +
   
   geom_vline(xintercept = as.Date("2022-08-09 00:00:00"), color = "red", linetype = "dashed") +
   geom_vline(xintercept = as.Date("2022-08-20 00:00:00"), color = "red", linetype = "solid") +
   geom_vline(xintercept = as.Date("2022-08-31 00:00:00"), color = "blue", linetype = "solid") +
   geom_vline(xintercept = as.Date("2022-09-06 00:00:00"), color = "red", linetype = "dashed") +
   
   ggtitle("Stratification Index: 5m - 10m") +
   xlab("Time(UTC)") +
   ylab("Temperature(degrees C)")

plot(p)
 
 #temperature averages ------------------------------------------------------

 # this will change the formatting to make the 
 data_tr <- data %>%
   pivot_longer(cols = c(3:5), names_to = "sensor_depth", values_to = "value")
 
#Full set - isolated at "X" depth 
 data %>%
  ggplot(aes(x = ts_round, y = depth_10, colour = station)) +
  geom_point(size=0.5) +
  ggtitle("10 meters") +
  xlab("Time(UTC)") +
  ylab("Temperature(degrees C)")

# one or more stations - isolated at "X" depth 
data %>%
  filter(station == c("Spry Harbour", "Birchy Head", "Moose Point 1")) %>% 
  ggplot(aes(x=ts_round, y=depth_2, colour=station)) +
  geom_point(size=0.5) +
  xlab("Time(UTC)") +
  ylab("Temperature(degrees C)")

# one station - all depths - Interactive
p <- data_tr %>% 
  filter(ts_round >= as.Date("2020-09-02 00:00:00") & 
           ts_round <= as.Date("2021-01-02 00:00:00")) %>%
  filter(station == "Moose Point 1") %>%
  ggplot(aes(x=ts_round, y = value, colour = sensor_depth)) +
  geom_point(size=0.8) +
  xlab("Time(UTC)") +
  ylab("Temperature(degrees C)")

ggplotly(p)
