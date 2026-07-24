library(dplyr)
library(lubridate)
library(mooring)
library(readxl)
library(stringr)

station <- "Birchy Head"
depl_date <- "2025-12-06"

station <- "Wedgeport"
depl_date <- "2025-07-31"
rope_type = "3/8in leaded polypropylene"


model_mooring <- function(
    station, 
    depl_date, 
    metadata = NULL,
    rope_type = "3/8in leaded polypropylene", 
    segmentize_length = 1,
    with_current = TRUE
) {
  
  if(is.null(metadata)) {
    metadata <- read_excel(
      "R:/tracking_sheets/metadata_tracking/water_quality_deployment_tracking.xlsx"
    ) |> 
      select(county, station, deployment_date, 
             deployment_latitude_n_ddm, deployment_longitude_w_ddm,
             sensor_type, sensor_serial_number, sensor_depth_m, sounding_m,
             vr2ar_lug_height_above_seafloor_m,
             primary_buoy_type, secondary_buoy_type, bottom_buoy_type,
             anchor_type, anchor_weight_kg
      ) |> 
      filter(station == !!station, deployment_date == as_date(depl_date)) |> 
      # remove hobo attached to VR2AR
      group_by(sensor_depth_m) |> 
      mutate(n = n()) |> 
      ungroup() |> 
      filter(!(n == 2 & sensor_type == "HOBO Pro V2")) |> 
      mutate(
        instrument = case_when(
          sensor_type == "HOBO Pro V2" ~ "Hobo Temp U22",
          sensor_type == "HOBO DO" ~ "Hobo DO U26",
          str_detect(sensor_type, "VR2AR") ~ "VR2AR reciever",
          TRUE ~ sensor_type
        ),
        
        # might make more sense to do this after unique() so only making the conversiom
        # once
        primary_buoy_type = gsub("\"", "in", primary_buoy_type),
        float_type = case_when(
          primary_buoy_type == "14in hard vinyl" ~ "14in centre hole tfloat",
          primary_buoy_type == "11in hard vinyl" ~ "11in centre hole tfloat",
        )
      )
  }
  
  # make sure sensors are ordered from deepest to most shallow
  metadata <- metadata |> 
    arrange(desc(sensor_depth_m))
  
  # add warning if converted to NA or more than one value
  sounding_m <- unique(as.numeric(metadata$sounding_m))
  first_rope_length_m <- unique(as.numeric(metadata$vr2ar_lug_height_above_seafloor_m))
  
  #anchor_type <- gsub("[s]$", "", unique(metadata$anchor_type))
  anchor_type <- anchor(gsub("[s]$", "", unique(metadata$anchor_type)))
  
  float_type <- float(unique(metadata$float_type))
  
  

  ss <- NULL
  ss[[1]] <- anchor_type

  j <- 2
  # starting from BOTTOM UP
  for (i in 1:nrow(metadata)) {
    
    inst_i <- instrument(model = metadata[i, ]$instrument) 
    
    # from anchor to vr2
    if (i == 1) {
      wire_i <- wire(model = rope_type, length = first_rope_length_m)
    } else {
      
      # from sensor i-1 to sensor i
      wire_i <- wire(
        model = rope_type, 
        length = metadata[i-1, ]$sensor_depth_m - metadata[i, ]$sensor_depth_m - inst_i@height)
    }
    
    ss[[j]] <- wire_i
    j <- j+ 1
    
    ss[[j]] <- inst_i
    j <- j + 1
  }

ss[[j]] <- wire(model = rope_type, length = 0.05)
ss[[j + 1]] <- float_type
ss[["waterDepth"]] <- sounding_m

m <- do.call(mooring, ss)
  
plot(m, fancy = TRUE, showDepths = TRUE) 
  
  
  
  ss <- list(
    waterDepth = 42,
    anchor_type,
    rope[[1]], inst[[1]],
    
    rope[[2]], inst[[2]],
    
    rope[[3]], inst[[3]],
    
    rope[[4]], inst[[4]],
    float_type
  )

  msk <- m |> 
    segmentize(by=1) |> 
    knockdown(u = 0.5)
  plot(msk, fancy = TRUE)  
  
}
