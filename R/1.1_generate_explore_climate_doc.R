library(here)

# SECTION 1: SET UP ---------------------------------------------

# number of direction binds/ options are 8 and 16
round_int <- "2 hours"

# SECTION 2: GENERATE REPORTS --------------------------------------------------------

report <- here("R/1.1_explore_climate_data.qmd")

quarto::quarto_render(
  input = report,
  output_file = paste0(
    "1.1_explore_climate_data_", gsub(" ", "_", round_int), ".html"
  ),
  execute_params = list(round_int = round_int)
)
