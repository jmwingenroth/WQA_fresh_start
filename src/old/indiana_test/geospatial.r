# Load packages
library(terra)
library(sf)
library(tidyverse)
library(exactextractr)

# Set parameters
sf_use_s2(FALSE)
crp_crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Load data
cdl_path <- "L:/Project-AgWeather/data/raw/nass/cdl/2011_30m_cdls/2011_30m_cdls.tif"
crp_paths <- list.files("L:/Project-AgWeather/data/int/rasterized_crp/states/", pattern = "crp_18", full.names = TRUE)
fi_paths <- list.files("L:/Project-AgWeather/data/int/N_fertilizer_intensity/", pattern = "fi_18", full.names = TRUE)

huc_12 <- st_read(
    "L:/Project-AgWeather/data/raw/wbd/WBD_National_GPKG.gpkg",
    layer = "WBDHU12"
) %>%
    st_transform(crp_crs) %>%
    st_simplify(dTolerance = 50) # meters (CRP pixel size)

cdl <- rast(cdl_path) # All have same effective CRS despite different IDs
crp <- rast(crp_paths)
fi <- rast(fi_paths)

# Trim data
cdl_indiana <- resample(cdl, fi)
huc_12_indiana <- filter(huc_12, str_detect(states, "IN"))

# Convert CDL to ag vs. non-ag based on CDL classes
cdl_int <- as.numeric(cdl_indiana)
cdl_ag <- cdl_int %in% c(1:80, 200:256) # these vectors include all crops and fallow cropland

# Extract Ag area to HUC-12s
cdl_ag_huc12 <- exact_extract(
    x = cdl_ag,
    y = huc_12_indiana,
    fun = "sum",
    append_cols = c("huc12", "tohuc", "name", "areasqkm", "states"),
    max_cells_in_memory = 1e9
)

ag_huc12_tidy <- cdl_ag_huc12 %>%
    as_tibble() %>%
    mutate(area_sqkm = sum/400) # Convert from 50m x 50m cells to sq. km.

# Calculate "CRP upstream N density"
crp_fi <- crp * fi

# Extract crp_fi
crp_fi_huc12 <- exact_extract(
    x = crp_fi,
    y = huc_12_indiana,
    fun = "sum",
    append_cols = c("huc12", "tohuc", "name", "areasqkm", "states"),
    max_cells_in_memory = 1e9
)

# Save data
write_csv(ag_huc12_tidy, "./data/intermediate/indiana_ag_sqkm_by_huc12.csv")
write_csv(crp_fi_huc12, "./data/intermediate/indiana_crp_nitrogen_mass_by_huc12.csv")
