
# list all of the climate station data folders (i.e., remove folders that do not hold
# met data)
list_climate_data_folders <- function(path) {
  dat_folders <- list.dirs(here("data-raw/"), full.names = TRUE, recursive = FALSE)
  dat_folders <- dat_folders[-which(grepl("ast", dat_folders))]
  dat_folders <- dat_folders[-which(grepl("cygwin", dat_folders))]
}

# read in all files in path and bind them together
foo <- function(path) {
  map_df(
    .x = list.files(path, full.names = TRUE), .f = fread, fill = TRUE
  ) 
}