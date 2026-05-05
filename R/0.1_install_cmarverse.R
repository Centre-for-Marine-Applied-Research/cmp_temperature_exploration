# April 28, 2026

# run this code to install the CMAR R packages that you will be using
# to work with the CMP data

# these packages are not on CRAN, so you will install them from GitHub

# you only need to run this script once. If any of the packages are updated,
# just re-run the code to install that package for the most recent version

install.packages("devtools")
library(devtools) # to install packages from GitHub


# sensorstrings -----------------------------------------------------------

install_github("Centre-for-Marine-Applied-Research/sensorstrings")

# qaqcmar -----------------------------------------------------------

install_github("Centre-for-Marine-Applied-Research/qaqcmar")

# cmpr -----------------------------------------------------------

install_github("Centre-for-Marine-Applied-Research/cmpr")






