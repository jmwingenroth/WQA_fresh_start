# Load packages
library(terra)
library(tidyverse)
library(sf)
library(exactextractr)
library(fst)

# Turn off spherical geometry
sf_use_s2(FALSE)

# Set parameters
crp_crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
fi_dir <- "L:/Project-AgWeather/data/int/N_fertilizer_intensity/"
cdl_path <- "L:/Project-AgWeather/data/raw/nass/cdl/2011_30m_cdls/2011_30m_cdls.tif"

# Load FI path names (just using FI outlines)
fi_files <- list.files(
    fi_dir, 
    pattern = "fi_[0-9]*_2023*.*tif", 
    full.names = TRUE
)
fi_files <- fi_files[-18] # Maine causes an error

# Load CDL
cdl <- rast(cdl_path)

# Load HUC-12 data
huc_12 <- st_read(
    "L:/Project-AgWeather/data/raw/wbd/WBD_National_GPKG.gpkg",
    layer = "WBDHU12"
    ) %>%
    st_transform(crp_crs) %>%
    st_simplify(dTolerance = 50) # meters (CRP pixel size)
gc() # otherwise R keeps big WBD file in memory

# Extract CDL FIPS integers to match with states
fi_fips <- fi_files %>%
    str_extract("fi_[0-9]*") %>% 
    unique() %>%
    str_extract("[0-9]+") %>%
    as.integer()

# Load FIPS key
st_fips_key <- read_csv("./data/input/state_fips_key.csv") %>%
    mutate(st = as.numeric(st)) %>%
    filter(st %in% fi_fips) %>%
    arrange(st)

# Process each state
ag_huc12_list <- list()
for (i in seq_along(fi_files)) {
    print(paste0("Processing state ", i, " of ", length(fi_files)))
    
    # Get state abbreviation for filtering
    state_fips <- str_extract(fi_files[i], "[0-9]+")
    state_abbr <- st_fips_key$stusps[st_fips_key$st == as.numeric(state_fips)]
    
    # Filter HUC12s for current state
    huc12_state <- huc_12 %>%
        filter(str_detect(states, state_abbr))
    
    # Load and resample CDL to match FI raster
    fi_rast <- rast(fi_files[i])
    cdl_state <- resample(cdl, fi_rast)
    
    # Convert CDL to ag vs. non-ag 
    cdl_int <- as.numeric(cdl_state)
    cdl_ag <- cdl_int %in% c(1:80, 200:256)
    
    # Extract ag area for state's HUC12s
    ag_huc12_list[[i]] <- exact_extract(
        x = cdl_ag,
        y = huc12_state,
        fun = "sum",
        append_cols = c("huc12", "tohuc", "name", "areasqkm", "states"),
        max_cells_in_memory = 3e8
    ) %>%
        as_tibble() %>%
        mutate(ag_area_sqkm = sum/400) # Convert from 50m x 50m cells to sq. km
}

# Combine results and handle state boundary overlaps
ag_area_by_huc12 <- bind_rows(ag_huc12_list) %>%
    group_by(across(c(huc12:states))) %>%
    summarise(ag_area_sqkm = mean(ag_area_sqkm))

# Save output
write_fst(ag_area_by_huc12, "./data/intermediate/ag_area_by_huc12.fst")
