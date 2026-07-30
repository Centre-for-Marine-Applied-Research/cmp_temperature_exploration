# July 28, 2026
# DD
# Function to calculate Bakun's Upwelling Index


calculate_bui <- function(
    wind_from_direction_degree, # direction wind is blowing from (degrees), clockwise from the North
    wind_speed_m_s,             # corresponding wind speed in m/s
    coastline_angle_degree,     # angle of the coast (degrees), clockwise from the North
    latitude_decimal_degree,    # latitude in decimal degrees (used to calculate Coriolis parameter)
    
    p_air = 1.28,   # density of air
    cd = 0.0015,    # wind drag coefficient
    p_water = 1026, # density of sea water
    
    return_calcs = FALSE
) {
  
  f = oce::coriolis(latitude_decimal_degree)    # coriolis parameter
  
  # direction wind is blowing *to* (clockwise from the North)
  wind_to_direction_degree = if_else(
    wind_from_direction_degree < 180,
    wind_from_direction_degree + 180,
    wind_from_direction_degree - 180)
  
  tau = p_air * cd * wind_speed_m_s^2     # wind stress
  ekman = tau / (p_water * f)             # Ekman transport
  
  phi = (coastline_angle_degree - wind_to_direction_degree) * pi / 180 # angle between coastline and wind direction
  ekman_perp = ekman * cos(phi)          # cross-shore Ekman transport 
  bui =  100 * ekman_perp                # bakun index
  
  
  if(isTRUE(return_calcs)) {
    data.frame(
      f = f,
      wind_to_direction_degree = wind_to_direction_degree,
      tau = tau,
      ekman = ekman,
      phi = phi,
      ekman_perp = ekman_perp,
      bui = bui
    )
  } else {  round(bui, digits = 2) }
}

# tests -------------------------------------------------------------------
library(ggplot2)

# # should be 181.4619 (max value)
# calculate_bui(
#   wind_speed_m_s = 10, 
#   wind_from_direction_degree = 240,
#   latitude_decimal_degree = 45, 
#   coastline_angle_degree = 60
# )
# 
# # should be -181.4619
# calculate_bui(
#   wind_speed_m_s = 10, 
#   wind_from_direction_degree = 60,
#   latitude_decimal_degree = 45, 
#   coastline_angle_degree = 60
# )
# 
# # should be 0
# calculate_bui(
#   wind_speed_m_s = 10, 
#   wind_from_direction_degree = 150,
#   latitude_decimal_degree = 45, 
#   coastline_angle_degree = 60
# )
# 
# # should be 0
# calculate_bui(
#   wind_speed_m_s = 10, 
#   wind_from_direction_degree = 330,
#   latitude_decimal_degree = 45, 
#   coastline_angle_degree = 60
# )
# 
# # plot

# dat <- data.frame(
#   wind_speed_m_s = 10,
#   wind_from_direction_degree = seq(0, 360, 30),
#   latitude_decimal_degree = 45,
#   coastline_angle_degree = 60
# ) |>
#   mutate(
#     coastline_angle = 60,
#     bui_m3_s = calculate_bui(
#       wind_from_direction_degree = wind_from_direction_degree,
#       wind_speed_m_s = wind_speed_m_s,
#       coastline_angle_degree = coastline_angle_degree,
#       latitude_decimal_degree = latitude_decimal_degree
#     )
#   )
# 
# ggplot(dat, aes(wind_from_direction_degree, bui_m3_s)) +
#   geom_point() +
#   geom_line() +
#   geom_hline(yintercept = 0, linetype = 2) +
#   scale_x_continuous(breaks = seq(0, 360, 30)) +
#   scale_y_continuous(limits = c(-200, 200))
# 
# 
# # yarmouth
# calculate_bui(
#   wind_speed_m_s = 10, 
#   wind_from_direction_degree = 240,
#   latitude_decimal_degree = 45, 
#   coastline_angle_degree = 340
# )
# 
# dat2 <- data.frame(
#   wind_speed_m_s = 10,
#   wind_from_direction_degree = seq(0, 360, 30),
#   latitude_decimal_degree = 45,
#   coastline_angle_degree = 330
# ) |>
#   mutate(
#     coastline_angle = 330,
#     bui_m3_s = calculate_bui(
#       wind_from_direction_degree = wind_from_direction_degree,
#       wind_speed_m_s = wind_speed_m_s,
#       coastline_angle_degree = coastline_angle_degree,
#       latitude_decimal_degree = latitude_decimal_degree
#     )
#   )
# 
# ggplot(dat2, aes(wind_from_direction_degree, bui_m3_s)) +
#   geom_point() +
#   geom_line() +
#   geom_hline(yintercept = 0, linetype = 2) +
#   scale_x_continuous(breaks = seq(0, 360, 30)) +
#   scale_y_continuous(limits = c(-200, 200))
# 
# 
# 
# dat |> 
#   bind_rows(dat2) |> 
#   ggplot(aes(wind_from_direction_degree, bui_m3_s, 
#              col = factor(coastline_angle_degree))) +
#   geom_point() +
#   geom_line() +
#   geom_hline(yintercept = 0, linetype = 2) +
#   scale_x_continuous(breaks = seq(0, 360, 30)) +
#   scale_y_continuous(limits = c(-200, 200)) +
#   scale_color_manual("coastline_angle_degree", values = c("#1B9E77FF", "#D95F02FF"))



