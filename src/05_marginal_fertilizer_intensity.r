# Load packages
library(terra)
library(tidyverse)
library(sf)
library(exactextractr)
library(fst)

# Turn off spherical geometry
sf_use_s2(FALSE)

# Set parameters
crp_enroll_dir <- "L:/Project-AgWeather/data/int/rasterized_crp/states/enroll_year/"
crp_years <- 2012:2022
crp_crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
fi_dir <- "L:/Project-AgWeather/data/int/N_fertilizer_intensity/"
overwrite_figure <- TRUE

# Load CRP and FI path names
crp_files <- list.files(
    crp_enroll_dir, 
    pattern = "crp_[0-9]*.*tif", 
    full.names = TRUE
)

fi_files <- list.files(
    fi_dir, 
    pattern = "fi_[0-9]*.*tif", 
    full.names = TRUE
)

# Extract CRP FIPS integers
crp_fips <- crp_files %>%
    str_extract("crp_[0-9]*") %>% 
    unique() %>%
    str_extract("[0-9]+") %>%
    as.integer()

# Extract FI FIPS integers
fi_fips <- fi_files %>%
    str_extract("fi_[0-9]*") %>% 
    unique() %>%
    str_extract("[0-9]+") %>%
    as.integer()

if (!all(crp_fips == fi_fips)) {
    stop("State(s) missing CRP enrollment or fertilizer intensity rasters")
}

# Load FIPS key
st_fips_key <- read_csv("./data/input/state_fips_key.csv") %>%
    mutate(st = as.numeric(st)) %>%
    filter(st %in% crp_fips) %>%
    arrange(st)

# Make configuration table of file paths and years
raster_params <- bind_cols(
    fi_files = fi_files, 
    crp_files = crp_files,
    fips = st_fips_key$st,
    state = st_fips_key$stusps
    ) %>%
    filter(state != "ME") %>% # Maine causes an error when extracting raster data
    expand_grid(year = c(0, crp_years)) # 0 is a dummy to extract directly from FI raster

# Load HUC-12 data
huc_12 <- st_read(
    "L:/Project-AgWeather/data/raw/wbd/WBD_National_GPKG.gpkg",
    layer = "WBDHU12"
    ) %>%
    st_transform(crp_crs) %>%
    st_simplify(dTolerance = 50) # meters (CRP pixel size)
gc() # otherwise R keeps big WBD file in memory

# Extract FI x CRP enrollment raster data to HUC-12 polygons (COMPUTE-INTENSIVE)
fi_huc12 <- list()
for (i in 1:nrow(raster_params)) {

    print(paste0("Extracting ",i," of ",nrow(raster_params)))

    if(raster_params$year[i] == 0) {
        fi_huc12[[i]] <- huc_12 %>%
            filter(str_detect(states, raster_params$state[i])) %>%
            exact_extract(
                x = rast(raster_params$fi_files[i]),
                fun = "sum",
                append_cols = c("huc12", "tohuc", "name", "areasqkm", "states"),
                max_cells_in_memory = 3e8
            )
    } else {
        fi_huc12[[i]] <- huc_12 %>%
            filter(str_detect(states, raster_params$state[i])) %>%
            exact_extract(
                x = rast(raster_params$fi_files[i])*(rast(raster_params$crp_files[i])==raster_params$year[i]),
                fun = "sum",
                append_cols = c("huc12", "tohuc", "name", "areasqkm", "states"),
                max_cells_in_memory = 3e8
            )
    }

}

# Join parameter data
fi_huc12_wide <- list()
for (i in 1:nrow(raster_params)) {
    fi_huc12_wide[[i]] <- fi_huc12[[i]] %>%
        bind_cols(raster_params[i,]) %>%
        as_tibble()
}

# Handle HUC-12s overlapping state boundaries and raster edges (about 1% of polygons)
fert_mass_huc12_tidy <- bind_rows(fi_huc12_wide) %>%
    group_by(across(c(huc12:states, year))) %>%
    summarize(fertilizer_mass = mean(sum)/4) # convert from kg/hectare*(50 m^2) to kg

# Make sanity test figure
if (overwrite_figure) {
    fert_mass_huc12_tidy %>%
        mutate(states = str_sub(states,,2)) %>%
        filter(str_detect(states, "ND|AR|NC"), year > 2012) %>%
        group_by(states, year) %>%
        summarize(fm = mean(fertilizer_mass)) %>%
        ungroup() %>%
        ggplot(aes(x = year, y = fm, color = states)) +
        geom_line() +
        labs(title = "North Dakota has some of the highest CRP enrollment")
    ggsave("./figs/test/05_marginal_fertilizer_intensity.svg")
}

# Save output
write_fst(fert_mass_huc12_tidy, "./data/intermediate/nitrogen_mass_by_huc12.fst")
