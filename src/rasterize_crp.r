# This script rasterizes the CRP data at 100m resolution by state (10km buffer)
# Authors: Penny L. and Jordan W.
# Last modified: 7/14/2025

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               sf,
               raster,
               fasterize,
               tigris,
               tictoc,
               exactextractr)

# wq_practices to filter CRP polygons
wq_practices <- c("cp08","cp21","cp22","cp23","cp27","cp28","cp30","cp39","cp40","cp41")

`%ni%` <- Negate(`%in%`)  # "not in" function

### preliminary -------------------------------------------------------------

# paths
trans_path <- "processing/corelogic/merge_trim/"
crp_path <- "L:/Project-AgWeather/data/raw/crp/national_years_rasterization/" # updated to match line 4


# set projection for buffering
proj <- 4326

# download states and counties
state_shp <- states() %>% 
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) %>%
  st_transform(proj)
  
county_shp <- counties() %>%  st_transform(proj)

# set CRP year
#crp_year <- 2012


### function to rasterize -----------------------------------------------------

# function to rasterize CRP for given state and year
rast_crp_state <- function(stfips, buffer, res) {
  
  # select focal state
  state <- state_shp[state_shp$STATEFP == stfips,]
  
  # find counties overlapping with state buffer
  state_buffer <- st_buffer(state_shp[state_shp$STATEFP == stfips,], buffer)
  buffer_counties <- county_shp[state_buffer, ]
  
  # select CRP polygons in adjacent counties and filter by wq_practices
  crp_state <- crp[crp$census_county_fips %in% buffer_counties$GEOID & crp$crp_practice_number %in% wq_practices,]
  
  if (dim(crp_state)[1] > 0) {
  
    crp_state$crp_pn_class2 <- factor(crp_state$crp_pn_class2, 
                                      levels = c("Grass","Trees","Wetland","Various","noclass",NA))
    crp_state$crp_landcover <- as.numeric(as.factor(crp_state$crp_pn_class2))
    
    # define the raster template
    state_buffer <- st_transform(state_buffer, st_crs(crp_state))
    raster_template <- raster(extent(state_buffer), resolution = res, crs = st_crs(state_buffer)$proj4string)
    
    # rasterize
    rasterized <- fasterize(crp_state, raster_template, field = "crp_landcover", fun = "first", background = NA)
    
    # output
    return(rasterized)
    
  } else {
    return(NULL)
  }
  

}


### rasterize in a loop ----------------------------------------------------------------

buffer <- 1e4
resolution <- 50

for (crp_year in c(2012:2022)) {
  # load CRP using updated folder location
  crp <- st_read(paste0(crp_path, sprintf("crp_%s.gpkg", crp_year)))
  
  for (st in unique(state_shp$STATEFP)) {
    
    # print
    print(paste0(crp_year, ": ", st))
    
    # rasterize
    rast_50 <- rast_crp_state(st, buffer, resolution)
    
    if (!is.null(rast_50)) {
    
    # write
    output_file = paste0("L:/Project-AgWeather/data/int/rasterized_crp/riparian/", "ras_crp_", st, "_", crp_year, "_50m.tif")
    writeRaster(rast_50, filename = output_file, format = "GTiff", overwrite = TRUE)
    
    }
    
    }
}

