# Load packages
library(terra)
library(tidyverse)
library(sf)
library(exactextractr)
library(fst)

# Turn off spherical geometry
sf_use_s2(FALSE)

# Set parameters
crp_disenroll_dir <- "L:/Project-AgWeather/data/int/rasterized_crp/states/disenroll_year/"
crp_crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Load CRP files
crp_files <- list.files(
    crp_disenroll_dir, 
    pattern = "crp_[0-9]*.*tif", 
    full.names = TRUE
)
crp_files <- crp_files[-18] # Maine causes issues

# Extract CRP FIPS integers
crp_fips <- crp_files %>%
    str_extract("crp_[0-9]*") %>% 
    unique() %>%
    str_extract("[0-9]+") %>%
    as.integer()

# Load HUC-12 data
huc_12 <- st_read(
    "L:/Project-AgWeather/data/raw/wbd/WBD_National_GPKG.gpkg",
    layer = "WBDHU12"
    ) %>%
    st_transform(crp_crs) %>%
    st_simplify(dTolerance = 50) # meters (CRP pixel size)
gc() # otherwise R keeps big WBD file in memory

# Load FIPS key
st_fips_key <- read_csv("./data/input/state_fips_key.csv") %>%
    mutate(st = as.numeric(st)) %>%
    filter(st %in% crp_fips) %>%
    arrange(st)

if (!all(crp_fips == st_fips_key$st)) {
    stop("Issue with state FIPS codes")
}

huc12_disenroll_area <- list()
for (i in seq_along(crp_files)) {

    print(paste0("Processing state ", i, " of ", length(crp_files)))

    crp <- rast(crp_files[i])
    hucs <- filter(huc_12, str_detect(states, st_fips_key$stusps[i]))

    huc12_disenroll_area[[i]] <- exact_extract(crp, hucs, include_cols = "huc12") %>%
        lapply(filter, !is.na(value)) %>%
        lapply(group_by, value, huc12) %>%
        lapply(summarize, sqkm = sum(coverage_fraction)/400) %>%
        lapply(pivot_wider, names_from = value, values_from = sqkm) %>%
        bind_rows()

}

huc12_disenroll_area %>% bind_rows() %>% write_fst("data/intermediate/crp_disenroll_by_huc12_messy.fst")
