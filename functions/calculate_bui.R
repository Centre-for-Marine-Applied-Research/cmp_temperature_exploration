calculate_bui <- function(
    wind_from_direction_degree,
    wind_speed_m_s,
    coastline_angle_degree,
    latitude_decimal_degree,
    
    p_air = 1.28,   # density of air
    cd = 0.0015,    # wind drag coefficient
    p_water = 1026,
    
    return_calcs = FALSE
) {
  
  f = oce::coriolis(latitude_decimal_degree)             # coriolis parameter
  wind_to_direction_degree = wind_from_direction_degree - 180 
  
  tau = p_air * cd * wind_speed_m_s^2     # wind stress
  ekman = tau / (p_water * f)             # Ekman transport
  
  phi = (coastline_angle_degree - wind_to_direction_degree) * pi / 180
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

# should be 181.4619
calculate_bui(
  wind_speed_m_s = 10, 
  wind_from_direction_degree = 240,
  latitude_decimal_degree = 45, 
  coastline_angle_degree = 60
)

# should be -181.4619
calculate_bui(
  wind_speed_m_s = 10, 
  wind_from_direction_degree = 60,
  latitude_decimal_degree = 45, 
  coastline_angle_degree = 60
)

# should be 0
calculate_bui(
  wind_speed_m_s = 10, 
  wind_from_direction_degree = 150,
  latitude_decimal_degree = 45, 
  coastline_angle_degree = 60
)

# should be 0
calculate_bui(
  wind_speed_m_s = 10, 
  wind_from_direction_degree = 330,
  latitude_decimal_degree = 45, 
  coastline_angle_degree = 60
)



# dat: data frame with columns wind_to_direction_degree and wind_speed_m_s
# option to include latitude and coastline angle
# 
# calculate_bui <- function(
    #     dat, 
#     p_air = 1.28,   # density of air
#     cd = 0.0015,    # wind drag coefficient
#     p_water = 1026,
#     
#     coastline_angle_degree = NULL,
#     latitude_decimal_degree = NULL,
#     
#     return_calcs = FALSE
# ) {
#   
# 
#   if(!is.null(coastline_angle_degree)) {
#     if("coastline_angle_degree" %in% colnames(dat)) {
#       warning("the coastline_angle_degree column in dat was replaced by the coastline_angle_degree argument")
#     }
#     dat$coastline_angle_degree <- coastline_angle_degree
#   }
#   
#   if(!is.null(latitude_decimal_degree)) {
#     if("latitude" %in% colnames(dat)) {
#       warning("the latitude column in dat was replaced by the latitude_decimal_degree argument")
#     }
#     dat$latitude <- latitude_decimal_degree
#   }
#   
#   dat <- dat %>% 
#     mutate(
#       f = oce::coriolis(latitude),             # coriolis parameter
#       wind_to_direction_degree = wind_from_direction_degree - 180, 
#       
#       tau = p_air * cd * wind_speed_m_s^2,     # wind stress
#       ekman = tau / (p_water * f),             # Ekman transport
#       
#       phi = (coastline_angle_degree - wind_to_direction_degree) * pi / 180,
#       ekman_perp = ekman * cos(phi),          # cross-shore Ekman transport 
#       bui =  100 * ekman_perp                 # bakun index
#     )
#   
#   if(isFALSE(return_calcs)) {
#     
#     dat <- dat %>% 
#       select(-c(f, wind_to_direction_degree, tau, ekman, phi, ekman_perp))
#   }
#   
#   dat
# }
# 

# tests -------------------------------------------------------------------

# # should be 181.4619
# calculate_bui(
#   data.frame(wind_speed_m_s = 10, wind_from_direction_degree = 240), 
#   latitude_decimal_degree = 45, coastline_angle_degree = 60
# )
# 
# # should be -181.4619
# calculate_bui(
#   data.frame(wind_speed_m_s = 10, wind_from_direction_degree = 60), 
#   latitude_decimal_degree = 45, coastline_angle_degree = 60
# )
# 
# # should be 0
# calculate_bui(
#   data.frame(wind_speed_m_s = 10, wind_from_direction_degree = 150), 
#   latitude_decimal_degree = 45, coastline_angle_degree = 60
# )
# 
# # should be 0
# calculate_bui(
#   data.frame(wind_speed_m_s = 10, wind_from_direction_degree = 330), 
#   latitude_decimal_degree = 45, coastline_angle_degree = 60
# )
