library(oce)
library(tidyr)
library(ggplot2)
library(sensorstrings)
library(lubridate)
library(repr)
library(plotly)
library(dplyr)

library(scales)
library(geomtextpath)
library(imputeTS)
library(padr)


# - -----------------------------------------------------------------------

# base template and data from the oce package  ----------------------------

data(sealevel)
plot(sealevel)

tide_model <- tidem(sealevel)
plot(tide_model)

plot(sealevel, which = 2)
predicted_tide <- predict(tide_model)
lines(sealevel[["time"]], predicted_tide, col = 2)

detided <- sealevel
detided[["elevation"]] <- detided[["elevation"]] - predicted_tide
plot(detided)

t <- sealevel[["time"]]
dt <- diff(t)[1] |> as.numeric() # in hours, assuming the data is already in 1 hour intervals 
#dt <- diff(t)[1] |> as.numeric() / 60 # if the data is in 15 minute sampling intervals 
#dt <- dt/24 # if you want the data to be treated in days

sl <- ts(sealevel[["elevation"]], deltat = dt)
res <- ts(detided[["elevation"]], deltat = dt)
spec <- spectrum(sl, spans = c(21,11,5), log = "yes")
grid()
spec_detided <- spectrum(res, spans = c(21,11,5), add = TRUE, col = 2, log = "yes")


# my code ----------------------------------------------------------------

#1. extracting the data and padding for time
round_unit <- "1 hour"

data <- readRDS("data/water_level_data.rds") %>% 
  dplyr::filter(station == "Bedford Institute") %>% 
  mutate(ts_round = round_date(timestamp_utc, unit = round_unit)) %>%
  summarise(
    avg_water_level_m = mean(water_level_m), .by = ts_round) %>% 
  dplyr::filter(ts_round >= as.Date("2022-01-01 00:00:00"), 
           ts_round <= as.Date("2023-01-01 00:00:00")) 
   

stopifnot(data$ts_round==365*24)
table(diff(data$ts_round))

plot(x=data$ts_round[-2], y=diff(data$ts_round))

# padding the data 
data <- pad(data)

data <- data %>% arrange(ts_round)

table(diff(data$ts_round))


sl_obj <- as.sealevel(elevation = data$avg_water_level_m, time = data$ts_round)
tidal_model <- tidem(sl_obj)

predictions <- predict(tidal_model, newdata = data$ts_round)

data <- data %>%
  mutate(modelled_elevation = coalesce(avg_water_level_m, predictions))

#checking to that everything in in 1 hours time-steps, with no gaps or NAs
table(diff(data$ts_round))
plot(x=data$ts_round[-2], y=diff(data$ts_round))

# - -----------------------------------------------------------------------


#2. making the data a sealevel object and plotting
data <- as.sealevel(elevation = data$modelled_elevation, time = data$ts_round)
plot(data)

#3. creating a tidal model and plotting
tide_model <- tidem(data)
plot(tide_model)

#3.5 plotting the actual tidal elevation (raw data) and tidal model together
plot(data, which = 2)
predicted_tide <- predict(tide_model) #creating a tidal model
lines(data[["time"]], predicted_tide, col = 2)

#4. calculating the de-tided elevations 
detided_data <- data

# detided data = raw observations - predictions 
detided_data[["elevation"]] <- detided_data[["elevation"]] - predicted_tide
plot(detided_data)


# 4.5 plotting observed data and de-tided data together in a ts
de.tided.data.numbers <- detided_data[["elevation"]] 

plot(data, which = 2)                         # observed data 
lines(data[["time"]], de.tided.data.numbers, col = 2) # detided data 
lines(data[["time"]], predicted_tide, col = 3) # predicted tides

data_reworked <- as.data.frame(data@data)
data_reworked <- data_reworked %>% rename(raw = elevation)
data_reworked$detided <- de.tided.data.numbers

data_reworked <- pivot_longer(data_reworked, cols = c(raw, detided), 
                                          names_to = "data",
                                          values_to = "elevation")

data_reworked <- data_reworked %>% filter(time >= as.Date("2022-08-01 00:00:00") & 
                                            time <= as.Date("2022-09-01 00:00:00"))


ggplot(data_reworked, aes(x=time, y= elevation, colour = data)) +
  geom_line()


#5. isolating time 
t <- data[["time"]]
dt <- diff(t)[1] %>% as.numeric() # in hours

table(dt)

stopifnot(all(dt==1))


 #6. plotting the spectral data as is
sl <- ts(data[["elevation"]], deltat = dt)
sl_detide <- ts(detided_data[["elevation"]], deltat = dt)

spec <- spectrum(sl, spans = c(21,11,5), taper = 0.5)
#grid()
spec_detided <- spectrum(sl_detide, spans = c(21,11,5),  taper = 0.5,
                         add = TRUE, col = 2)


#6.5 Extract the frequencies and spectrum values into a data.frame
df_raw <- data.frame(
  frequency = spec$freq,
  raw = spec$spec)

df_detide <- data.frame(
  frequency = spec_detided$freq,
  detided = spec_detided$spec)

df_merge <- merge(df_raw, df_detide)
df_merge <- pivot_longer(df_merge, cols = c(raw, detided), 
                         names_to = "data",
                         values_to = "spectral_density")


#7. re-plotting tidal data in better format 
ggplot(df_merge, aes(x=frequency, y=spectral_density, colour = data)) +
  geom_line(linewidth = 0.5) +
  geom_textvline(xintercept =  0.0805, label = "M2", color = "black", linetype = "dashed", size = 3) +
  geom_textvline(xintercept = 0.0418, label = "K1", color = "grey", linetype = "dashed", size = 3) +
  geom_textvline(xintercept = 0.1610, label = "M4", color = "grey", linetype = "dashed", size = 3) +
  
  scale_x_log10(labels = label_log(), guide = "axis_logticks") +
  scale_y_log10(labels = label_log(), guide = "axis_logticks") +
  guides(y = guide_axis_logticks()) +
  annotation_logticks() +
  theme(aspect.ratio = 1/2) +
  labs(
    x = "Frequency (cph)",
    y = "Spectral Density (m2 / cph)"
  )




