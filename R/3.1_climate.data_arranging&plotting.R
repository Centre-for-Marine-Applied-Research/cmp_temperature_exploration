
library(data.table)     # export csv file
library(dplyr)          # wrangling data
library(here)           # file paths
library(lubridate)      # dates & times
library(plotly)         # interactive figures
library(sensorstrings)  # plot data
library(ggplot2)        # station plotting
library(openair)        # wind roses

climate_data <- readRDS(("climate_data/climate_data.rds"))

# climate data has already been rounded to 2 hours
# the next lines can round it to a lower resolution (ex: 12hours, 24hours, etc)

round_unit <- "24 hours"

climate_data <- climate_data %>% 
  mutate(Timestamp_utc = round_date(Timestamp_utc, unit = round_unit)) %>%
  summarise(
    avg.air.temp = mean(air_Temperature, na.rm=TRUE), 
    avg.wind.speed_kmph = mean(wind_speed_km_per_hour, na.rm=TRUE), 
    avg.wind.direction.degree = mean(wind_direction_degree, na.rm=TRUE),
    .by = c(station, Timestamp_utc))

colnames(climate_data)[1] <- "w_station"
colnames(climate_data)[2] <- "ts_round"

# to export data
file_name <- paste0("climate_data_",  gsub(" ", "_", round_unit), ".RDS")

saveRDS(climate_data, file = here("data", file_name))


# plotting ----------------------------------------------------------------

#re-reading in data sets that were just made. Make sure to specify which data set
      # you want to use when plotting 
climate_data_2 <- readRDS(("data/climate_data_2_hours.RDS"))
climate_data_12 <- readRDS(("data/climate_data_12_hours.RDS"))
climate_data_24 <- readRDS(("data/climate_data_24_hours.RDS"))


#air temperature data - Full set 

climate_data_24 %>%
  ggplot(aes(x = ts_round, y = avg.air.temp, colour = station)) + 
  geom_line(linewidth = 0.5) +
  ggtitle("Average air temp") +
  xlab("Time(UTC)") +
  ylab("Temperature(degrees C)")

# air temperature data - one or more stations

climate_data_24 %>%
  filter(w_station == c("LUNENBURG", "BEAVER ISLAND (AUT)")) %>% 
  ggplot(aes(x = ts_round, y = avg.air.temp, colour = w_station)) + 
  geom_line(linewidth = 0.5) +
  ggtitle("Average air temp") +
  xlab("Time(UTC)") +
  ylab("Temperature(degrees C)")

# wind speed - Full set 

climate_data_24 %>% 
  ggplot(aes(x = ts_round, y = avg.wind.speed_kmph, colour = w_station)) + 
  geom_line(linewidth = 0.5) +
  ggtitle("Average wind speed") +
  xlab("Time(UTC)") +
  ylab("Wind Speed(kmph)")

# wind speed data - one or more stations

climate_data_24 %>%
  filter(w_station == "BEAVER ISLAND (AUT)") %>% 
  ggplot(aes(x = ts_round, y = avg.wind.speed_kmph, colour = w_station)) + 
  geom_line(linewidth = 0.5) +
  ggtitle("Average wind speed") +
  xlab("Time(UTC)") +
  ylab("Wind Speed(kmph)")

#-----------------------------------------------------------------------------
# wind rose 
windRose(climate_data_24, ws = "avg.wind.speed_kmph", wd = "avg.wind.direction.degree", 
           ws.int = 2, angle = 30, 
           key.position = "bottom") 

  
  # wind direction at a given station - NEEDS WORK

#Kaplan et al, 2003 paper for reference
    #time on the x (hour or the day)
    #wind direction on the y

# S = top
# E = middle up (+)
# N = 0
# W = middle down (-)
# S = bottom


#climate_data_24 %>% 
  #filter(w_station == "LUNENBURG") %>% 
  #ggplot(aes(x= ts_round, y = avg.wind.direction.degree)) +
  #geom_point()
