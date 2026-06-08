library(here)

# SECTION 1: SET UP ---------------------------------------------

# number of direction binds/ options are 8 and 16
n_dir <- 8

# SECTION 2: GENERATE REPORTS --------------------------------------------------------

report <- here("R/1.2_explore_wind_data.qmd")

quarto::quarto_render(
  input = report,
  output_file = paste0("1.2_explore_wind_data_", n_dir, "dirs.html"),
  execute_params = list(n_dir_bins = n_dir)
)
