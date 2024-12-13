# Load packages
library(terra)
library(tidyverse)

# Load parameters
cdl_path <- "L:/Project-AgWeather/data/raw/nass/cdl/2011_30m_cdls/2011_30m_cdls.tif"
fertilizer_path <- "./data/input/ludemann_2022_fertilizer_use.csv"
cdl_levels_path <- "data/intermediate/cdl_crops.csv"
cdl_ludemann_key_path <- "./data/intermediate/cdl_ludemann_key.csv"

# Load function library
invisible(lapply(list.files("src/lib", full.names = TRUE), source))

# Load CDL raster
cdl_rast <- rast(cdl_path)

# Write CDL categories to CSV
cats(cdl_rast) %>%
    .[[1]] %>%
    as_tibble() %>%
    filter(Histogram > 0) %>%
    arrange(desc(Histogram)) %>%
    write_csv(cdl_levels_path) # Manually edited this file to create key

# Load crop category key
cdl_ludemann_key <- read_csv(cdl_ludemann_key_path)

# Load fertilizer use data
fert_data <- read_csv(fertilizer_path)

# Calculate fertilizer intensity
fi_data <- fert_data %>%
    mutate(kg_per_hectare = metric_tonnes_N*1000/planted_hectares) %>%
    arrange(desc(kg_per_hectare)) %>%
    bind_rows(tibble(
        CROP = "Unknown",
        kg_per_hectare = 100
    ))

# Join data to form CDL category to fertilizer intensity key
cdl_fi_key <- left_join(cdl_ludemann_key, fi_data)

