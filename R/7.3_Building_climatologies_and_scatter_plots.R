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

# - -----------------------------------------------------------------------
# skip to test 3 ----------------------------------------------------------
# TEST 1 ------------------------------------------------------------------

#1 loading the data AND checking where/how large the missing time values are 
round_unit <- "24 hour"

data <- readRDS("data/data_1.0.rds") %>% 
  filter(qc_flag_value != "Fail") %>% 
  mutate(ts_round = round_date(timestamp_utc, unit = round_unit)) %>%
  summarise(
    value_avg = mean(value), .by = c(station, sensor_depth_at_low_tide_m, ts_round))
  

object_1 <- data %>% 
  group_by(station, sensor_depth_at_low_tide_m) %>% 
  mutate(column_1 = difftime(ts_round, lag(ts_round)),
         column_2 = as.numeric(column_1)) %>% 
  ungroup() %>% 
  summarize(n=n(), .by = c(station, sensor_depth_at_low_tide_m, column_1))


#plot(x=data$ts_round[-1], y=diff(data$ts_round))

  #pivot_wider(values_from = value_avg, names_from = sensor_depth_at_low_tide_m) %>%
  
  #rename(depth_2 = `2`, 
         #depth_5 = `5`,
         #depth_10 =`10`) %>% 
  
  #mutate(Month = month(ts_round), Day = day(ts_round)) %>%  
  #mutate(month_day = format(ts_round, "%m-%d")) %>% 
  #select(station, ts_round, depth_2, depth_5, depth_10)

#2 padding the data / filling in missing dates and a check 
data <- data %>% 
  group_by(station, sensor_depth_at_low_tide_m) %>% 
  pad(interval = "day") %>% 
  ungroup()

object_2 <- data %>% 
  group_by(station, sensor_depth_at_low_tide_m) %>% 
  mutate(column_1 = difftime(ts_round, lag(ts_round)),
         column_2 = as.numeric(column_1)) %>% 
  ungroup() %>% 
  summarize(n=n(), .by = c(station, sensor_depth_at_low_tide_m, column_1))

# reformatting the data
data_wide <- data %>% arrange(station, ts_round) %>% 
  
  pivot_wider(values_from = value_avg, 
                             names_from = sensor_depth_at_low_tide_m) %>%
  rename(depth_2 = `2`, 
  depth_5 = `5`,
  depth_10 =`10`) %>% 
  mutate(month_day = format(ts_round, "%m-%d")) %>% 
  arrange(station, ts_round) 

dat <- data_wide %>% filter(station == "Center Bay")

object_3 <- dat %>% 
  group_by(station, depth_2, depth_5, depth_10) %>% 
  mutate(column_1 = difftime(ts_round, lag(ts_round)),
         column_2 = as.numeric(column_1)) %>% 
  ungroup() %>% 
  summarize(n=n(), .by = c(station, depth_2, depth_5, depth_10, column_1))


#3 filling in the data gaps by depth
imp_2 <- data_wide %>% arrange(station, ts_round) %>% pull(depth_2) %>% ts()
imp_2 <- na_seadec(imp_2, algorithm = "interpolation", find_frequency = TRUE)
data_wide$interp_2 <- imp_2

imp_5 <- data_wide %>% arrange(station, ts_round) %>% pull(depth_5) %>% ts()
imp_5 <- na_seadec(imp_5, algorithm = "interpolation", find_frequency = TRUE)
data_wide$interp_5 <- imp_5

imp_10 <- data_wide %>% arrange(station, ts_round) %>% pull(depth_10) %>% ts()
imp_10 <- na_seadec(imp_10, algorithm = "interpolation", find_frequency = TRUE)
data_wide$interp_10 <- imp_10

#data_wide <- data_wide %>% arrange(station, ts_round)

#4 testing by station

data_long <- data_wide %>% 
  pivot_longer(
  cols = c(depth_2, depth_5, depth_10),
  names_to = "sensor_depth",
  values_to = "depth_values") %>% 
  pivot_longer(
    cols = c(interp_2, interp_5, interp_10),
    names_to = "interp_depth",
    values_to = "interp_values")


object_4 <- data_long %>% 
  arrange(station, ts_round) %>% 
  group_by(station, interp_depth) %>% 
  mutate(column_1 = difftime(ts_round, lag(ts_round)),
         column_2 = as.numeric(column_1)) %>% 
  ungroup() %>% 
  summarize(n=n(), .by = c(station, interp_depth, column_1))


#5 classification 

data_wide[is.na(data_wide)] <- 0

data_test <- data_wide %>% 
  group_by(station) %>% 
  mutate(imputed_2 = case_when(
    interp_2 == depth_2 ~ "no", 
    interp_2 != depth_2 ~ "yes")) %>% 
  mutate(imputed_5 = case_when(
    interp_5 == depth_5 ~ "no",
    interp_5 != depth_5 ~ "yes")) %>% 
  mutate(imputed_10 = case_when(
    interp_10 == depth_10 ~ "no",
    interp_10 != depth_10 ~ "yes")) %>% ungroup()


#6 plotting to check
ggplot(data_test, aes(x=ts_round, y=interp_2, colour = imputed_2)) +
  geom_line(linewidth = 0.5)+
  facet_wrap(~station, ncol = 2)

#ggplotly(p)


# TEST 2 ------------------------------------------------------------------

#1 - loading the data - WORKS 
round_unit <- "24 hour"

data <- readRDS("data/data_1.0.rds") %>% 
  filter(qc_flag_value != "Fail") %>% 
  mutate(ts_round = round_date(timestamp_utc, unit = round_unit)) %>%
  summarise(
    value_avg = mean(value), .by = c(station, sensor_depth_at_low_tide_m, ts_round)) %>% 
  pivot_wider(values_from = value_avg,
              names_from = sensor_depth_at_low_tide_m) %>%
  rename(depth_2 = `2`, 
         depth_5 = `5`,
         depth_10 =`10`) %>% 
  #mutate(Month = month(ts_round), Day = day(ts_round)) %>%  
  mutate(month_day = format(ts_round, "%m-%d")) %>% 
  select(station, ts_round, depth_2, depth_5, depth_10)

#2 filling in the missing dates and re-arranging thing - WORKS
#object A is a test to make sure that the pad() was applied properly
data <- data %>% 
  group_by(station) %>% 
  pad() %>% 
  ungroup()%>% 
  mutate(month_day = format(ts_round, "%m-%d"))

data <- data %>% arrange(station, ts_round)

object_A <- data %>% 
  group_by(station) %>% 
  mutate(column_1 = difftime(ts_round, lag(ts_round)),
         column_2 = as.numeric(column_1)) %>% 
  ungroup() %>% 
  summarize(n=n(), .by = c(station, column_1))


#3 filling in the data gaps by depth - WORKS
imp_2 <- data %>% arrange(station, ts_round) %>% pull(depth_2) %>% ts()
imp_2 <- na_seadec(imp_2, algorithm = "interpolation", find_frequency = TRUE)
data$interp_2 <- imp_2

imp_5 <- data %>% arrange(station, ts_round) %>% pull(depth_5) %>% ts()
imp_5 <- na_seadec(imp_5, algorithm = "interpolation", find_frequency = TRUE)
data$interp_5 <- imp_5

imp_10 <- data %>% arrange(station, ts_round) %>% pull(depth_10) %>% ts()
imp_10 <- na_seadec(imp_10, algorithm = "interpolation", find_frequency = TRUE)
data$interp_10 <- imp_10


#4 classifying the data as interpolated or not - WORKS
data_label <- data
data_label[is.na(data_label)] <- 0

data_label <- data_label %>% 
  group_by(station) %>% 
  mutate(imputed_2 = case_when(
    interp_2 == depth_2 ~ "no", 
    interp_2 != depth_2 ~ "yes")) %>% 
  mutate(imputed_5 = case_when(
    interp_5 == depth_5 ~ "no",
    interp_5 != depth_5 ~ "yes")) %>% 
  mutate(imputed_10 = case_when(
    interp_10 == depth_10 ~ "no",
    interp_10 != depth_10 ~ "yes")) %>% 
  ungroup()


#5 plotting to check - WORKS
dat <- data_label %>% filter(station == "Center Bay")

p <- ggplot(data_label, aes(x=ts_round, y=interp_10)) +
  geom_line() +
  facet_wrap(~station)
 
ggplotly(p)

#6 will need to maunally interpolated some of the values

#interp_2
#Spry Harbour: 2018-03-01 = -1.3
data_label$depth_2[X] <- -1.3

#Birchy Head: 2019-09-01= 20
data_label$depth_2[X] <- 20

#Center Bay: 2018-08-12 = 18
data_label$depth_2[X] <- 18

#interp_5
# Moose Point 1: 2020-08-21 = 16
#Spry Harbour: 2018-03-04 = 1
#Birchy Head: 2023-08-13 = 14
#Birhcy Head: 2023-11-06 = 10
#Center Bay: 2022-03-03 = 0.3

#interp_10
#Center Bay: 2018-08-08 = 15
#Spry Harbour: 2012-12-28 = 5


# TEST 3 ------------------------------------------------------------------

#1 loading the data
round_unit = "24 hour"

data <- readRDS("data/data_1.0.rds") %>% 
  dplyr::filter(qc_flag_value != "Fail") %>% 
  mutate(ts_round = round_date(timestamp_utc, unit = round_unit)) %>%
  summarise(
    value_avg = mean(value), .by = c(station, sensor_depth_at_low_tide_m, ts_round)) %>% 
  pivot_wider(values_from = value_avg,
              names_from = sensor_depth_at_low_tide_m) %>%
  rename(depth_2 = `2`, 
         depth_5 = `5`,
         depth_10 =`10`) %>% 
  mutate(month_day = format(ts_round, "%m-%d"))

#2 padding the data and checking to make sure it worked properly
data <- data %>% group_by(station) %>% 
  pad() %>% 
  ungroup()
    
data <- data %>% arrange(ts_round) 

object_A <- data %>% 
  group_by(station) %>% 
  mutate(column_1 = difftime(ts_round, lag(ts_round)),
         column_2 = as.numeric(column_1)) %>% 
  ungroup() %>% 
  summarize(n=n(), .by = c(station, column_1))


# check grouping ----------------------------------------------------------

#3 adding data to the new rows
imp_2 <- data %>% group_by(station) %>% arrange(ts_round) %>% pull(depth_2) %>% ts() 
imp_2 <- na_seadec(imp_2, algorithm = "interpolation", find_frequency = TRUE)
data$interp_02 <- imp_2

imp_5 <- data %>% group_by(station) %>% arrange(ts_round) %>% pull(depth_5) %>% ts()
imp_5 <- na_seadec(imp_5, algorithm = "interpolation", find_frequency = TRUE)
data$interp_05 <- imp_5

imp_10 <- data %>% group_by(station) %>% arrange(ts_round) %>% pull(depth_10) %>% ts()
imp_10 <- na_seadec(imp_10, algorithm = "interpolation", find_frequency = TRUE)
data$interp_10 <- imp_10


#4 plotting to check
ggplot(data, aes(ts_round, interp_02)) +
  geom_line() +
  facet_wrap(~station)


#5 assigning values to be yes/no based on interpolated data 
data_labelled <- data %>% 
  group_by(station) %>% 
  pivot_longer(cols = c(depth_2, depth_5, depth_10),
               names_to = "depth_obs",
               values_to = "depth_values") %>% 
  mutate(interp_status = if_else(is.na(depth_values), "yes", "no")) %>% 
  ungroup()

#6 count the number of observation that were interpolated 
  
  

#7 plotting the values when they're imputed/not imputed
#interp_02 or _05 or _10

ggplot(data_labelled, aes(x=ts_round, y=interp_10, colour = interp_status)) +
  geom_point() +
  facet_wrap(~station)


saveRDS(data_labelled, file = "data/data_interp.rds")

data <- readRDS("data/data_interp.rds")

# End of TEST 3 -----------------------------------------------------------

# calculating climatologies

data <- readRDS("data/data_interp.rds")

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

# start here --------------------------------------------------------------

anomalies_df <- data %>%
  left_join(daily_climatology, by = c("month_day", "station", "depth_obs")) %>%
  pivot_longer(cols = c(interp_02, interp_05, interp_10),
               names_to = "interp_obs",
               values_to = "interp_values") %>% 
  group_by(station, depth_obs, interp_obs) %>%  
  mutate(anomaly_value = interp_values - climatology_mean,
         anomaly_obs = interp_obs) %>% 
  #add another line to have names to associate with anomalies (ie: anomaly_obs)
  ungroup()

saveRDS(anomalies_df, file = "data/anomalies_df.rds")

anomalies_df <- readRDS("data/anomalies_df.rds")

#pivot_longer for legend
anomalies_df %>% filter(depth_obs = "interp_10")%>% 
  ggplot(aes(x=ts_round)) +
  geom_point(aes(y=climatology_mean), col = "darkblue") +
  geom_line(aes(y=interp), col = "red") +
  facet_wrap(~station)

# scatter plots -----------------------------------------------------------


#plotting the anomalies vs stratification index values
round_unit <- "24 hour"

Strat_index <- readRDS("data/data_1.0.rds")

Strat_index <- Strat_index %>% 
  dplyr::filter(qc_flag_value != "Fail") %>% 
  mutate(ts_round = round_date(timestamp_utc, unit = round_unit)) %>%
  summarise(
    value_avg = mean(value), .by = c(station, sensor_depth_at_low_tide_m, ts_round, sensor_type)) %>% 
  mutate(month_day = format(ts_round, "%m-%d")) %>% 
  pivot_wider(
    values_from = value_avg,
    names_from = sensor_depth_at_low_tide_m) %>% 
  rename(depth_2 = `2`,depth_5 = `5`, depth_10 = `10`) %>%
  mutate(Index.2_5 = depth_2 - depth_5, 
         Index.2_10 = depth_2 - depth_10,
         Index.5_10 = depth_5 - depth_10) %>% 
  pivot_longer(cols = c(depth_2, depth_5, depth_10),
               names_to = "depth_obs",
               values_to = "depth_values")


scatter_data <- anomalies_df %>%
  left_join(Strat_index, by = c("month_day", "station", "ts_round",
                                "depth_obs", "depth_values"), 
            relationship = "many-to-many")

# saving for later
saveRDS(scatter_data, file = "data/scatter_data.rds")

data_2 <- readRDS("data/scatter_data.rds")

# comparison 1

#pivot_longer to make legend default
scatter_data %>% ggplot(aes(x=ts_round)) +
  geom_point(aes(y=Index.2_10), colour = "purple") +
  geom_point(aes(y=Anomaly_d2), colour = "orange" ) +
  facet_wrap(~station) +
  labs(title = "stratification(purple) vs anomalies(orange)", 
       subtitle= "x and y are both in degrees C")

#comparison 2
scatter_data %>% ggplot(aes(x=Index.2_10, y=Anomaly_d10)) +
  geom_point() +
  facet_wrap(~station) +
  labs(title = "stratification vs anomalies", subtitle= "x and y are both in degrees C")


# re-reading data ---------------------------------------------------------

data <- readRDS("data/scatter_data.rds") %>% 
  pivot_longer(
    cols = c(Anomaly_d2, Anomaly_d5, Anomaly_d10),
      names_to = "anomaly",
      values_to = "anomaly_value") %>% 
  pivot_longer(
    cols = c(climatology_mean_d2, climatology_sd_d2,
             climatology_mean_d5, climatology_sd_d5,
             climatology_mean_d10, climatology_sd_d10),
    names_to = "climatology", 
    values_to = "climate_value") %>% 
  mutate(
    station = case_when(
      station == "Moose Point 1" ~ "1 - Moose Point 1", 
      station == "Center Bay" ~ "2 - Center Bay",
      station == "Spry Harbour" ~ "3 - Spry Harbour",
      station == "Birchy Head" ~ "4 - Birchy Head",
      station == "Wedgeport" ~ "5 - Wedgeport",
      station == "Big Pond Point" ~ "6 - Big Pond Point"))

# climatology and SD alone ------------------------------------------------

ggplot(data, aes(x= ts_round, y= climate_value, colour = climatology)) +
  geom_line(linewidth = 0.5)+
  facet_wrap(~station, ncol=2, scales = "free")

# anomalies ---------------------------------------------------------------

# as a histogram

# seperate by depth and facet by station 
ggplot(data, aes(x=anomaly_value, fill = anomaly)) +
  geom_histogram(binwidth = 0.5, position = "dodge") +
  scale_colour_viridis_c()+
  facet_wrap(~station, ncol = 2, scales = "free") +
  scale_x_continuous(breaks = seq(-10, 8, by = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

# as a line
ggplot(data, aes(x=ts_round, y=anomaly_value, colour = anomaly)) +
  geom_line(linewidth = 1) +
  facet_wrap(~station, ncol = 2,
             scales = "free")




