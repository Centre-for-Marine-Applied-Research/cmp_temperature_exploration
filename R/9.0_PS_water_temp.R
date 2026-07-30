library(oce)
library(tidyr)
library(ggplot2)
library(sensorstrings)
library(lubridate)
library(repr)
library(plotly)
library(dplyr)

library(imputeTS)
library(padr)
library(scales)
library(geomtextpath)

# interpolating the data --------------------------------------------------
round_unit = "1 hour"
data <- readRDS("data/data_1.0.rds")

data <- data %>% 
  filter(qc_flag_value != "Fail") %>% 
  mutate(ts_round = round_date(timestamp_utc, unit = round_unit)) %>%
  summarise(
    value_avg = mean(value), .by = c(station, sensor_depth_at_low_tide_m, ts_round)) %>% 
  pivot_wider(values_from = value_avg,
              names_from = sensor_depth_at_low_tide_m)

data <- data %>%
  rename(depth_2 = `2`, 
         depth_5 = `5`,
         depth_10 =`10`) %>% 
  mutate(month_day = format(ts_round, "%m-%d"))


# START HERE --------------------------------------------------------------

data <- pad(data)

#adding data to the new rows

imp_2 <- data %>% arrange(ts_round) %>% pull(depth_2) %>% ts()
imp_2 <- na_seadec(imp_2, algorithm = "interpolation", find_frequency = TRUE)
data$interp_02 <- imp_2

imp_5 <- data %>% arrange(ts_round) %>% pull(depth_5) %>% ts()
imp_5 <- na_seadec(imp_5, algorithm = "interpolation", find_frequency = TRUE)
data$interp_05 <- imp_5

imp_10 <- data %>% arrange(ts_round) %>% pull(depth_10) %>% ts()
imp_10 <- na_seadec(imp_10, algorithm = "interpolation", find_frequency = TRUE)
data$interp_10 <- imp_10


#plotting 
ggplot(data, aes(ts_round, interp_10)) +
  geom_line() +
  facet_wrap(~station)

#saving
saveRDS(data, file = "data/interpolated_temp_data.rds")


# - -----------------------------------------------------------------------
data <- readRDS("data/interpolated_temp_data.rds") %>% 
  mutate(
    station = case_when(
      station == "Moose Point 1" ~ "1 - Moose Point 1", 
      station == "Center Bay" ~ "2 - Center Bay",
      station == "Spry Harbour" ~ "3 - Spry Harbour",
      station == "Birchy Head" ~ "4 - Birchy Head",
      station == "Wedgeport" ~ "5 - Wedgeport",
      station == "Big Pond Point" ~ "6 - Big Pond Point")) %>% 
  filter(ts_round >= as.Date("2022-01-01 00:00:00") & 
           ts_round <= as.Date("2023-01-01 00:00:00")) %>% 
  pivot_longer(
    cols = c("interp_02", "interp_05", "interp_10"),
    names_to = "depth", 
    values_to = "value") %>%
  select(station, ts_round, depth, value)

# the sampling frequency is in samples per day 
# 2 hours = 12 samples per day, 1 hour = 24 samples, 12 hours = 2 samples, etc

depth_x = "interp_02"

frequency_x = 24


# using spectrum ----------------------------------------------------------

# 1. Calculate spectra per site and depth 
spec_data <- data %>%
  #filter(depth == depth_x) %>% 
  group_by(station, depth) %>%
  do({

    s <- spectrum(.$value, log = "yes", spans= c(21,11,5), plot = F) 
    
    data.frame(
      frequency = s$freq,
      spectral_density = s$spec
    )
  })

spec_data_fixed <- spec_data %>% 
  mutate(label = paste(
    station, depth))

# 2. Plot 
ggplot(spec_data_fixed, aes(x=frequency, y=spectral_density)) +
  geom_line(linewidth = 0.5) +
  scale_x_log10(labels = label_log(), guide = "axis_logticks") +
  scale_y_log10(labels = label_log(), guide = "axis_logticks") +
  facet_wrap(.~label, ncol = 3) +
  
  #geom_textvline(xintercept =  0.0805, label = "M2", color = "black", linetype = "dashed", size = 1) +
  geom_textvline(xintercept = 0.0418, label = "K1", color = "grey", linetype = "dashed", size = 1) +
  geom_textvline(xintercept = 0.1610, label = "M4", color = "grey", linetype = "dashed", size = 1) +
  
  
    theme(strip.text = element_text(size = 8), strip.placement = "outside") +
  
  guides(y = guide_axis_logticks()) +
  #annotation_logticks() +
  labs(
    x = "Frequency (cph)",
    y = "Spectral Density (degrees C^2 / cph)"
  )


# using pwelch ------------------------------------------------------------

# need to seasonally interpolate the NA values or pwelch will break 
# and look like garbage 


stations <- sort(unique(data$station))

for(i in seq_along(stations)) {
  
  station_i <- stations[i]
  
  cat('\n###', station_i, '\n')
  
  dat_i <- data %>% filter(station == station_i)
  
  dd_i <- dat_i %>% 
    filter(depth == depth_x) %>% 
    select(value) %>% 
    unlist()
  
  
  temp_ts <- ts(dd_i, frequency = frequency_x)
  
specw <- pwelch(temp_ts, spans = c(5), plot = FALSE)

plot(specw,
     xlab = "Frequency (cycles per day)", 
     ylab = "Spectral Density (degrees C^2 / cycles per day)",
     main = station_i,
     log = "yes")

}



