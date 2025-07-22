# Load packages

library(sf)
library(terra)
library(stars)

sf_use_s2(FALSE)

fi_dir <- "L:/Project-AgWeather/data/int/N_fertilizer_intensity/"

fi_files <- list.files(
    fi_dir, 
    pattern = "fi_[0-9]*_2011*.*tif", 
    full.names = TRUE
)

rasts <- lapply(fi_files, rast)

ag_land_means <- lapply(rasts, global, "mean", na.rm = TRUE)

