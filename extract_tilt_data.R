library(dplyr)
library(lubridate)
library(sensorstrings)
library(purrr)

# move this to sensorstrings
ss_extract_trimdates <- function(path) {
  r_txt <- readLines(path)
  
  start_date_txt <- strsplit(
    r_txt[grep("depl_start <- ", r_txt)], split = " <- ")[[1]][2]
  
  end_date_txt <- strsplit(
    r_txt[grep("depl_end <- ", r_txt)], split = " <- ")[[1]][2]
  
  start_date <- eval(parse(text = start_date_txt))
  end_date <- eval(parse(text = end_date_txt))
  
  data.frame(start_date = start_date, end_date = end_date)
  
}


# Moose Point 1 -------------------------------------------------------------

station <- "Moose Point 1"

station <- gsub(" ", "_", tolower(station))

depl_dates <- paste0(
  "R:/data_branches/water_quality/station_folders/", station
) %>% 
  list.files(pattern = station) %>% 
  substr(nchar(station) + 2, nchar(station) + 11) 
  
trimdates <- depl_dates[1:length(depl_dates) - 1] %>% 
  map(\(x) ss_import_path(station, x)) %>% l



dat_moose_point_1 <- depl_dates[1:length(depl_dates) - 1] %>% 
  map(\(x) ss_import_path(station, x)) %>% 
  unlist() %>% 
  map(\(x) ss_compile_deployment_data(x)) %>% 
  list_rbind()

dat_moose_point_1 %>% 
  select(-contains("dissolved_oxygen"), 
         -contains("salinity"),
         -contains("depth_measured")
         ) %>% 
  ss_ggplot_variables()



path <- ss_import_path(station, "2020-02-24")
path <- paste0(path, "/compile_", station, "_2020-02-24.R")

ss_extract_trimdates(path)










grep("depl_start <- ", r_txt)

strsplit(r_txt[58], split = " <- ")[[1]][2]


path <- ss_import_path(station, "2020-02-24")

dat1 <- ss_compile_deployment_data(path) %>%
  filter(
    timestamp_utc >= '2020-02-24 19:00:00', 
    timestamp_utc <= '2020-10-21 16:30:00'
  )




