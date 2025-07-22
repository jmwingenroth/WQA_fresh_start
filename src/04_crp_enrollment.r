# Load packages
library(terra)
library(tidyverse)

# Load parameters
crp_dir <- "L:/Project-AgWeather/data/int/rasterized_crp/states/"
crp_years <- 2012:2022
enroll_crp_write_dir <- "L:/Project-AgWeather/data/int/rasterized_crp/states/enroll_year/"
disenroll_crp_write_dir <- "L:/Project-AgWeather/data/int/rasterized_crp/states/disenroll_year/"

# Load CRP path names
crp_files <- list.files(crp_dir, pattern = "crp_[0-9]*.*tif", full.names = TRUE)

# Extract distinct CRP FIPS strings
crp_fips <- str_extract(crp_files, "crp_[0-9]*") %>% unique()

# Make rasters showing the year of CRP disenrollment
for (i in 1:length(crp_fips)) {

    print(paste0("Calculating disenrollment year for CRP raster ",i," of ",length(crp_fips)))
    
    temp_stack <- crp_files[str_detect(crp_files, crp_fips[i])] %>%
        rast() 

    max(temp_stack*crp_years, na.rm = TRUE) %>%
        writeRaster(paste0(disenroll_crp_write_dir,"ras_",crp_fips[i],"_disenroll_50m.tif"))

}
