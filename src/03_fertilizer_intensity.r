# Load packages
library(terra)
library(tidyverse)

# Load parameters
cdl_path <- "L:/Project-AgWeather/data/raw/nass/cdl/2011_30m_cdls/2011_30m_cdls.tif"
crp_dir <- "L:/Project-AgWeather/data/int/rasterized_crp/states"
fi_write_dir <- "L:/Project-AgWeather/data/int/N_fertilizer_intensity"

fertilizer_path <- "./data/input/ludemann_2022_fertilizer_use.csv"
cdl_levels_write_path <- "data/intermediate/cdl_crops.csv"
cdl_ludemann_key_path <- "./data/intermediate/cdl_ludemann_key.csv"

# Load CDL raster
cdl_rast <- rast(cdl_path)

# Load CRP raster shapes
crp_paths <- list.files(crp_dir, pattern = "2012", recursive = TRUE, full.names = TRUE)
crp_rasts <- lapply(crp_paths, rast) 

### Produce a key to convert between CDL categories and fertilizer intensity ###

# Write CDL categories to CSV
cats(cdl_rast) %>%
    .[[1]] %>%
    as_tibble() %>%
    filter(Histogram > 0) %>%
    arrange(desc(Histogram)) %>%
    write_csv(cdl_levels_write_path) # Manually edited this file to create key

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

### Resample CDL raster to CRP extents and convert to fertilizer intensity

fi_files <- crp_paths %>%
    str_extract("ras_crp.*$") %>%
    str_replace("crp","fi") %>%
    str_replace("2012","2011")

coltab(cdl_rast) <- NULL

for (i in 1:length(crp_rasts)) {

    print(paste0("Sampling fertilizer intensity for CRP raster ",i," of ",length(crp_rasts)))

    cdl_rast %>%
        resample(crp_rasts[[i]]) %>%
        as.numeric() %>%
        subst(
            from = cdl_fi_key$value, 
            to = as.integer(round(cdl_fi_key$kg_per_hectare)), 
            others = 0,
            filename = file.path(fi_write_dir, fi_files[i]),
            overwrite = TRUE
        )
}
