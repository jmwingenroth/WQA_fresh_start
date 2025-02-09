# Load libraries
library(tidyverse)
library(fst)
library(sf)

# Set parameters
sf_use_s2(FALSE)
crp_crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Load data
wqp_data <- read_fst("./data/intermediate/wqp_pull.fst") %>% as_tibble()
mli_data <- read_fst("./data/intermediate/mli_pull.fst") %>% as_tibble()

fertilizer <- read_fst("./data/intermediate/nitrogen_mass_by_huc12.fst") %>% as_tibble()

huc_12 <- st_read(
    "L:/Project-AgWeather/data/raw/wbd/WBD_National_GPKG.gpkg",
    layer = "WBDHU12"
    ) %>%
    st_transform(crp_crs) %>%
    st_simplify(dTolerance = 50) # meters (CRP pixel size)
gc() # high-res WBD is time- and memory-intensitve

fips_key <- read_csv("./data/input/state_fips_key.csv")

# Calculate HUC-12 centroids and tie to fertilizer data
fertilizer_sf <- huc_12 %>%
    select(
        name,
        huc12,
        tohuc,
        areasqkm,
        states
    ) %>%
    st_centroid() %>%
    right_join(fertilizer) %>%
    pivot_wider(names_from = year, values_from = fertilizer_mass)

# Subset MLIs
# NOTE: Focusing on a sizable chunk of the WQP dataset with consistent SOP for now
# currently, n = 391,159
conus_fips <- fips_key %>%
    filter(!stusps %in% c("AK", "HI")) %>%
    .$st

mli_subset <- wqp_data %>%
    filter(
        CharacteristicName == "Ammonia",
        ResultMeasure.MeasureUnitCode == "mg/L",
        ProviderName == "STORET",
        ResultSampleFractionText == "Total"
    ) %>%
    .$MonitoringLocationIdentifier %>%
    unique()

mli_data_subset <- mli_data %>%
    filter(
        MonitoringLocationIdentifier %in% mli_subset,
        StateCode %in% conus_fips,
        as.numeric(LongitudeMeasure) != 0 # Drop some MLIs without lat-long data
    )

# Convert to project-standard CRS
mli_sf <- mli_data_subset %>%
    st_as_sf(
        coords = c("LongitudeMeasure", "LatitudeMeasure"), 
        crs = 4269 # From horizontal CRS column (NAD83)
    ) %>%
    st_transform(crp_crs)

# Calculate combined fertilizer intensity of nearby HUC-12s
mli_fi_big <- mli_sf %>% 
    select(
        Name = MonitoringLocationName, 
        MLI = MonitoringLocationIdentifier, 
        MLI_HUC8 = HUCEightDigitCode
    ) %>%
    st_buffer(dist = 5e4) %>% # 50 km
    st_join(fertilizer_sf) %>%
    st_drop_geometry()

mli_fi_nb <- mli_fi_big %>%
    group_by(MLI) %>%
    summarise(across(c(areasqkm, `0`:`2022`), sum))


# Find HUC-12 in which each MLI lies
mli_huc12 <- mli_sf %>%
    st_join(huc_12) %>%
    st_drop_geometry() %>%
    filter(!is.na(huc12)) %>%
    select(
        Name = MonitoringLocationName, 
        MLI = MonitoringLocationIdentifier, 
        MLI_HUC8 = HUCEightDigitCode,
        huc12,
        tohuc,
        areasqkm,
        states
    ) %>%
    group_by(MLI) %>%
    mutate(n = n()) %>%
    filter(n == 1)

# Find all upstream HUCs among those nearby
nb_hucs_list <- mli_fi_big %>%
    select(MLI, huc12, tohuc) %>%
    filter(MLI %in% mli_huc12$MLI) %>%
    group_by(MLI) %>%
    group_split()

upstream_hucs_list <- list()
upstream_error_tally <- 0
for (i in seq_along(nb_hucs_list)) {
    if (i %% 100 == 0) {
        print(paste0(
            "Found upstream HUC-12s for ",
            i,
            " of ",
            length(nb_hucs_list),
            " sites"
        ))
    }

    temp <- nb_hucs_list[[i]]
    origin <- mli_huc12[mli_huc12$MLI == temp$MLI[1],]$huc12
    add <- filter(temp, tohuc == origin)
    up <- add

    iterations <- 0

    while (nrow(add) > 0 && iterations < 1000) {
        add <- filter(temp, tohuc %in% add$huc12)
        up <- bind_rows(up, add)
        iterations <- iterations + 1
    }

    if (iterations == 1000) {
        print(paste0("Mapping error occurred for site ", i))
        upstream_error_tally <- upstream_error_tally + 1
    }

    upstream_hucs_list[[i]] <- up

}
