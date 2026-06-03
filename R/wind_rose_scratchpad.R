library(adcp)
library(dplyr)
library(ggplot2)

n_speed_bins <- 10
n_dir_bins <- 8

beav <- dat %>% 
  filter(
    station == "BEAVER ISLAND (AUT)", 
    !is.na(wind_direction_degree)
  ) %>% 
  adcp_label_direction(
    n_petals = n_dir_bins, 
    column = wind_direction_degree
  ) %>%
  adcp_label_speed(
    n_ints = n_speed_bins,
    column = wind_speed_km_per_hour
  )

adcp_plot_current_rose(
  beav,
  speed_col = wind_speed_km_per_hour_labels,
  direction_col = wind_direction_degree_labels,
  speed_label = "Wind Speed (km/h)"
) 


# facet by station --------------------------------------------------------

dat %>% 
  filter(!is.na(wind_direction_degree)) %>% 
  group_by(station) %>% 
  mutate(n_station = n()) %>% 
  ungroup() %>% 
  adcp_label_direction(n_petals = n_dir_bins, column = wind_direction_degree) %>%
  adcp_label_speed(column = wind_speed_km_per_hour, n_ints = n_speed_bins) %>% 
  summarise(
    n = n(),
    .by = c(wind_direction_degree_labels, wind_speed_km_per_hour_labels,
            station, n_station)
  ) %>%
  mutate(n_prop = n / n_station) %>% 
  adcp_plot_current_rose(
    speed_col = wind_speed_km_per_hour_labels,
    direction_col = wind_direction_degree_labels,
    speed_label = "Wind Speed (km/h)",
    calculate_prop = FALSE, ncol_legend = 5
  ) +
  facet_wrap(~station) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey50")
  )




