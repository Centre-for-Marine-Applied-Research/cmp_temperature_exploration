
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
library(tidyr)
library(oce)


# reading in data ---------------------------------------------------------

data_raw <- read_csv("climate_data_2_hours .csv")

data <- data_raw %>% pivot_wider(
  names_from = variable,   # Column containing values that will become new column names
  values_from = value   # Column containing values that will populate the new columns
) %>% 
  select(station, timestamp_utc, air_temperature, 
         wind_direction_degree, wind_speed_km_per_hour) %>% 
  filter_out(is.na(wind_direction_degree), is.na(wind_speed_km_per_hour)) %>% 
  
  mutate(speed_mps = wind_speed_km_per_hour/3.6, 
         wind_rad = wind_direction_degree *pi/180, 
         v = -speed_mps * cos(wind_rad), 
         u = -speed_mps * sin(wind_rad), 
         latitude = case_when(
           station == "YARMOUTH RCS" ~ 43,
           station == "LUNENBURG" ~ 44,
           station == "BEAVER ISLAND (AUT)" ~ 44,
           station == "HART ISLAND (AUT)" ~ 45,
           TRUE                 ~ NA  # Default value for anything else
         ), 
         coastline_angle = case_when(
           station == "YARMOUTH RCS" ~ 110,
           station == "LUNENBURG" ~ 30,
           station == "BEAVER ISLAND (AUT)" ~ 30,
           station == "HART ISLAND (AUT)" ~ 45,
           TRUE                 ~ NA  # Default value for anything else
         ),
    Mx = v / (p_water * f),
    My = -u / (p_water * f),
    
         
         
         )




#---------------------------------------------------

#CALCULATING BUI #

#--------------------------------------------------

# 1. Define physical constants
p_water <- 1026         # Seawater density (kg/m^3)
latitude <- 43

# Convert angles from degrees to radians
#lat_rad <- latitude * pi / 180
#coast_rad <- BUI_data$coastline_angle * pi / 180

# 2. Calculate Coriolis parameter (f)
f <- coriolis(43)

# 3. Calculate Ekman Transport components (M_x and M_y)
# Units: kg / (m * s)

#Mx <- v / (p_water * f)
#My <- -u / (p_water * f)


#4. upwelling function

upwell <- function(Mx, My, coast_angle) {
  degtorad <- pi/180.
  alpha <- (360 - coast_angle) * degtorad
  s1 <- cos(alpha)
  t1 <- sin(alpha)
  s2 <- -1 * t1
  t2 <- s1
  perp <- (s1 * Mx) + (t1 * My)
  para <- (s2 * Mx) + (t2 * My)
  return(perp/10)
}

#5. calculating the index!
BUI_value <- upwell(data$Mx, data$My, data$coastline_angle)

table <- cbind(BUI_value, data)

#plotting




p <- table %>% 
  filter(timestamp_utc >= as_date("2020-01-01 00:00:00") &
           timestamp_utc <= as_date("2020-12-31 00:00:00")) %>% 
  ggplot(aes(x = timestamp_utc, y = BUI_value, colour = station)) +
  geom_line() +
  ylab("BUI (m^3 / s)")



ggplotly(p)



# # -----------------------------------------------------------------------

# averaging the dataset to daily ------------------------------------------

round_unit <- "24 hours"

# averaging by desired amount of time (indicated in round_unit) --------------

TEST <- table %>% 
  mutate(timestamp_utc = round_date(timestamp_utc, unit = round_unit)) %>%
  summarise(BUI_avg = mean(BUI_value), .by = c(station, timestamp_utc)
  )

p <- TEST %>% 
  ggplot(aes(x = timestamp_utc, y = BUI_avg, colour = station)) +
  geom_line() +
  ylab("BUI (m^3 / s)")

ggplotly(p)





