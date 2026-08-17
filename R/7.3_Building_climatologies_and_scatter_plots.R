library(oce)
library(tidyr)
library(ggplot2)
library(sensorstrings)
library(lubridate)
library(repr)
library(plotly)
library(dplyr)
library(padr)
library(imputeTS)


# ------------------------------------------------------------------

#1 loading the data
round_unit = "24 hour"

data <- readRDS("data/data_1.0.rds") %>% 
  dplyr::filter(qc_flag_value != "Fail") %>% 
  mutate(ts_round = round_date(timestamp_utc, unit = round_unit)) %>%
  summarise(
    value_avg = mean(value), .by = c(station, sensor_depth_at_low_tide_m, ts_round)) %>% 
  pivot_wider(values_from = value_avg,
              names_from = sensor_depth_at_low_tide_m) %>%
  rename(depth_02 = `2`, 
         depth_05 = `5`,
         depth_10 =`10`) %>% 
  mutate(month_day = format(ts_round, "%m-%d")) %>% 
  mutate(
    station = case_when(
      station == "Moose Point 1" ~ "1 - Moose Point 1", 
      station == "Center Bay" ~ "2 - Center Bay",
      station == "Spry Harbour" ~ "3 - Spry Harbour",
      station == "Birchy Head" ~ "4 - Birchy Head",
      station == "Wedgeport" ~ "5 - Wedgeport",
      station == "Big Pond Point" ~ "6 - Big Pond Point"))

#2 padding the data and checking to make sure it worked properly (object A)
data <- data %>% group_by(station) %>% 
  pad() %>% 
  ungroup() %>% 
  arrange(ts_round) 

object_A <- data %>% 
  group_by(station) %>% 
  mutate(column_1 = difftime(ts_round, lag(ts_round)),
         column_2 = as.numeric(column_1)) %>% 
  ungroup() %>% 
  summarize(n=n(), .by = c(station, column_1))
# there should be an NA under all the stations as you can't count backwards from -
#- a day that does not "exist"


# check grouping ----------------------------------------------------------

#3 adding data to the new rows
imp_2 <- data %>% group_by(station) %>% arrange(ts_round) %>% pull(depth_02) %>% ts() 
imp_2 <- na_seadec(imp_2, algorithm = "interpolation", find_frequency = TRUE)
data$interp_02 <- imp_2

imp_5 <- data %>% group_by(station) %>% arrange(ts_round) %>% pull(depth_05) %>% ts()
imp_5 <- na_seadec(imp_5, algorithm = "interpolation", find_frequency = TRUE)
data$interp_05 <- imp_5

imp_10 <- data %>% group_by(station) %>% arrange(ts_round) %>% pull(depth_10) %>% ts()
imp_10 <- na_seadec(imp_10, algorithm = "interpolation", find_frequency = TRUE)
data$interp_10 <- imp_10


#4 plotting to check
ggplot(data, aes(ts_round, depth_02)) +
  geom_line() +
  facet_wrap(~station)

ggplot(data, aes(ts_round, interp_02)) +
  geom_line() +
  facet_wrap(~station)


#5 assigning values to be yes/no based on interpolated data 
data_labelled <- data %>% 
  group_by(station) %>% 
  pivot_longer(cols = c(depth_02, depth_05, depth_10),
               names_to = "depth_obs",
               values_to = "depth_values") %>% 
  mutate(interp_status = if_else(is.na(depth_values), "yes", "no")) %>% 
  ungroup() 

data_labelled$station_depth <- paste(data_labelled$station, 
                                     data_labelled$depth_obs)


#6 count the number of observation that were interpolated 
data_labelled %>%
  ggplot(aes(x=interp_status)) +
  geom_bar() +
  facet_wrap(~station_depth, ncol = 3) +
  scale_colour_viridis_c()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


#7 plotting the values when they're imputed/not imputed
data_labelled <- data_labelled %>% 
  mutate(station_depth = paste(station, depth_obs, sep = " ")) %>% 
  pivot_longer(cols = c(interp_02, interp_05, interp_10),
               names_to = "interp_obs",
               values_to = "interp_values")
  

ggplot(data_labelled, aes(x=ts_round, y=interp_values, colour = interp_status)) +
  geom_point() +
  facet_wrap(~station_depth, ncol= 3)


saveRDS(data_labelled, file = "data/data_interp.rds")

# -----------------------------------------------------------

data <- readRDS("data/data_interp.rds")

# calculating climatologies

daily_climatology <- data %>%
  group_by(month_day, station, depth_obs) %>%
  summarise(
  climatology_mean = mean(depth_values, na.rm = TRUE),
sd_climate = sd(depth_values, na.rm = TRUE),
upper_sd = climatology_mean + sd_climate,
lower_sd = climatology_mean - sd_climate) %>% 
  ungroup()

# - -----------------------------------------------------------------------
#Calculate historical anomalies

anomalies_df <- data %>%
  left_join(daily_climatology, by = c("month_day", "station", "depth_obs")) %>% 
  group_by(station, depth_obs, interp_obs) %>%  
  mutate(anomaly_value = interp_values - climatology_mean,
         anomaly_matched = climatology_mean + anomaly_value) %>% 
  ungroup()

saveRDS(anomalies_df, file = "data/anomalies_df.rds")

anomalies_df <- readRDS("data/anomalies_df.rds") %>%
  pivot_longer(cols=c(depth_values, interp_values, 
                      climatology_mean, upper_sd, lower_sd, sd_climate, 
                      anomaly_value, anomaly_matched),
               names_to = "variables",
               values_to = "values")

#plotting this will take a LONG time 
ggplot(anomalies_df, aes(x=ts_round, y=values, colour = variables)) +
  geom_point() +
  facet_warp(~station_depth)


# scatter plots -----------------------------------------------------------

#re-working data for a scatter plot
round_unit <- "24 hour"

Strat_index <- readRDS("data/data_1.0.rds") %>% 
  dplyr::filter(qc_flag_value != "Fail") %>% 
  mutate(ts_round = round_date(timestamp_utc, unit = round_unit)) %>%
  summarise(
    value_avg = mean(value), .by = c(station, sensor_depth_at_low_tide_m, ts_round)) %>% 
  mutate(month_day = format(ts_round, "%m-%d")) %>% 
  arrange(ts_round) %>% 

  pivot_wider(
    values_from = value_avg,
    names_from = sensor_depth_at_low_tide_m) %>% 
  rename(depth_2 = `2`,depth_5 = `5`, depth_10 = `10`) %>%
  mutate(Index.2_5 = depth_2 - depth_5, 
         Index.2_10 = depth_2 - depth_10,
         Index.5_10 = depth_5 - depth_10) %>% 
  
  pivot_longer(cols= c(Index.2_10, Index.2_5, Index.5_10),
               names_to = "index_obs",
               values_to = "index_values") %>% 
  pivot_longer(cols = c(depth_2, depth_5, depth_10),
               names_to = "depth_obs",
               values_to = "depth_values") 


scatter_data <- merge(anomalies_df, Strat_index, by = c("station", "ts_round", "month_day", 
                                                        "depth_obs"))
saveRDS(scatter_data, file = "data/scatter_data.rds")

# rework the scatter data  ---------------------------------------------------------

data <- readRDS("data/scatter_data.rds") %>% 
  mutate(
    station = case_when(
      station == "Moose Point 1" ~ "1 - Moose Point 1", 
      station == "Center Bay" ~ "2 - Center Bay",
      station == "Spry Harbour" ~ "3 - Spry Harbour",
      station == "Birchy Head" ~ "4 - Birchy Head",
      station == "Wedgeport" ~ "5 - Wedgeport",
      station == "Big Pond Point" ~ "6 - Big Pond Point"))

# climatology and SD alone ------------------------------------------------

data_x <- data %>% 
  select(station, ts_round, month_day, variables, values, station_depth) %>% 
  filter(variables == c("climatology_mean", "upper_sd", "lower_sd"))

ggplot(data_x, aes(x= ts_round, y=values, colour = variables)) +
  geom_line(linewidth = 0.5)+
  facet_wrap(~station_depth, ncol=3, scales = "free")

# anomalies---------------------------------------------------------------

# as a histogram

# separate by depth and facet by station 

data_y = data %>% 
  select(station, ts_round, month_day, variables, values, station_depth) %>% 
  filter(variables == "anomaly_value")
  
hist.fig <- ggplot(data_y, aes(x=values, fill = variables)) +
  geom_histogram(binwidth = 0.5, position = "dodge") +
  scale_colour_viridis_c()+
  facet_wrap(~station_depth, ncol = 2, scales = "free") +
  scale_x_continuous(breaks = seq(-10, 8, by = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("hist.fig.pdf", width= 21, height= 13.5, units = "in", dpi = 300)

# as a line
ggplot(data_y, aes(x=ts_round, y=values, colour = variables)) +
  geom_line(linewidth = 1) +
  facet_wrap(~station, ncol = 3,
             scales = "free")


# climatology and anomalies plotted together  -----------------------------
# could also be "depth_values" instead of "anomaly_matched"
data_z <- data %>% 
  select(station, ts_round, month_day, variables, values, station_depth) %>% 
  filter(variables == c("climatology_mean", "anomaly_matched"))


ggplot(data_z, aes(x=ts_round, y=values, colour = variables)) +
  geom_line(linewidth = 1) +
  facet_wrap(~station_depth, ncol = 3,
             scales = "free")



