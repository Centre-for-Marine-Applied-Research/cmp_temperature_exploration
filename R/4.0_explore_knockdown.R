library(adcp)
library(dplyr)
library(mooring)
library(readxl)

# current speed -----------------------------------------------------------

adcp_stations <- c("Big Pond Point", 
                   "Shut-In Island",
                   "Center Bay",
                   "Eddy Cove NE",
                   "Spry Harbour",
                   "Angus Shoal")

dat_raw <- adcp_import_data()

dat <- dat_raw |> 
  filter(
    station %in% adcp_stations,
    tidal_bin_height_flag %in% c(1, 2, NA)
  ) |> 
  summarise(.by = c(station), 
            mean_current_speed_m_s = round(mean(sea_water_speed_m_s), digits = 3),
            # med_current_speed_m_s = median(sea_water_speed_m_s),
            # min_current_speed_m_s = min(sea_water_speed_m_s),
            max_current_speed_m_s = max(sea_water_speed_m_s)
  ) |> 
  rename(adcp_station = station)

# knockdown example -------------------------------------------------------

rope_type <- "3/8in leaded polypropylene"

# birchy head
# sensor height is added to the string (reduce wire length)
m <- mooring(waterDepth = 44,
             anchor(model = "7 rotor"),
             
             # 40 m
             wire(model = rope_type, length = 1.5),
             instrument(model = "VR2AR reciever"),
             
             # 30 m
             wire(model = rope_type, length = 10),
             instrument(model = "Hobo Temp U22"),
             
             # 20 m
             wire(model = rope_type, length = 10),
             instrument(model = "Hobo Temp U22"),
             
             # 15 m 
             wire(model = rope_type, length = 5),
             instrument(model = "Hobo Temp U22"),
             
             # 10 m
             wire(model = rope_type, length = 5),
             instrument(model = "Hobo Temp U22"),
             
             # 5
             wire(model = rope_type, length = 5),
             instrument(model = "Hobo DO U26"),
             
             # 2 m
             wire(model = rope_type, length = 3),
             instrument(model = "Hobo Temp U22"),
             
             # buoy
             wire(model = rope_type, length = 1),
             float(model = "14in centre hole tfloat"))

plot(m)

# Segment the wire and apply the current (u) in m/s
msk <- segmentize(m, by = 0.5) %>%
  knockdown(u = 0.75)

plot(m, fancy = TRUE, showDepths = TRUE)

plot(msk, fancy = TRUE, showDepths = FALSE)

plot(msk, which = "knockdown", fancy = TRUE, showDepths = TRUE)

# Print all four plots
par(mfrow = c(2, 2))
plot(msk, which = "tension", fancy = TRUE, showDepths = TRUE)
plot(msk, which = "shape", fancy = TRUE, showDepths = FALSE)
plot(msk, which = "knockdown", fancy = TRUE, showDepths = TRUE)
plot(msk, which = "velocity", fancy = TRUE)








