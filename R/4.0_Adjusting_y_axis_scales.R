library(data.table)     # export csv file
library(dplyr)          # wrangling data
library(here)           # file paths
library(lubridate)      # dates & times
library(plotly)         # interactive figures
library(sensorstrings)  # plot data

library(patchwork)
library(hrbrthemes)
library(scales)

# plots with two Y axis

climate_data_2 <- readRDS(("data/climate_data_2_hours.RDS"))
climate_data_12 <- readRDS(("data/climate_data_12_hours.RDS"))
climate_data_24 <- readRDS(("data/climate_data_24_hours.RDS"))

average_temp_2 <- readRDS(("data/2.0_averaging_dataset_2_hours.rds"))
average_temp_12 <- readRDS(("data/2.0_averaging_dataset_12_hours.rds"))
average_temp_24 <- readRDS(("data/2.0_averaging_dataset_24_hours.rds"))

#-----------------------------------------------------------------------------

# This would work for plotting things with wildly different scales since.
# It's adjusting them to display nicely (ex: plotting 10s and 10000s on the same scale)



climate_data_24 %>% 
  filter(w_station == "LUNENBURG") %>% 
  ggplot(aes(x=ts_round)) +
  geom_point(aes(y=avg.air.temp, colour = "avg.air.temp")) + 
  geom_point(aes(y=avg.wind.speed_kmph/76.46 * 24.942, colour = "avg.wind.speed_kmph")) +
            #ex: wind speed / max wind speed * max air temp 
  
# factor you want to scale / maximum value of that factor * maximum of the second factor
# right y-axis / max value right y-axis * max value left axis  

  
  
  scale_y_continuous(name = "avg temperature(degrees C)", 
                     sec.axis = sec_axis(~.*76.46/24.942, name = "avg wind speed (kmph)"))+
  # flip the values do adjust the other axis
  #this ensures that both sides are correctly scaled to eachother
  
  scale_colour_manual(values=c("avg.air.temp"="red", "avg.wind.speed_kmph"="blue"))





#this would work when plotting thing on the same approximate scale, but- 
# they represent different things 
#(ex: air temp and wind speed) or maybe (ex: air temp vs water temp vs wind speed)
#the main different is that you are not adjusting the scales, just plotting them


# air temp and wind speed
climate_data_24 %>% 
  filter(w_station == "LUNENBURG") %>% 
  ggplot(aes(x=ts_round)) +
  geom_point(aes(y=avg.air.temp, colour = "avg.air.temp")) + 
  geom_point(aes(y=avg.wind.speed_kmph, colour = "avg.wind.speed_kmph")) +
  
  scale_y_continuous(name = "avg temperature(degrees C)", 
                     sec.axis = sec_axis(~., name = "avg wind speed (kmph)")) +
  scale_colour_manual(values=c("avg.air.temp"="red", "avg.wind.speed_kmph"="blue"))

# air temp and water temp
climate_data_24 %>% 
  filter(w_station == "LUNENBURG") %>% 
  ggplot(aes(x=ts_round)) +
  geom_point(aes(y=avg.air.temp, colour = "avg.air.temp")) + 
  geom_point(aes(y=avg.wind.speed_kmph, colour = "depth_10")) +
  
  scale_y_continuous(name = "avg temperature(degrees C)", 
                     sec.axis = sec_axis(~., name = "avg wind speed (kmph)")) +
  scale_colour_manual(values=c("avg.air.temp"="red", "depth_10"="blue"))


