# Load packages
library(tidyverse)
library(sf)
library(fst)
library(fixest)

sf_use_s2(FALSE)

# Load function library
invisible(lapply(list.files("src/lib", full.names = TRUE), source))

# Load data
wqp <- read_fst("./data/intermediate/wqp_pull.fst")

wqp_crp <- read_sf("./data/input/wqp_crp_upstream_and_local.gpkg") %>%
    as_tibble() %>%
    st_drop_geometry()

huc12_mli <- select(wqp_crp, MLI, huc12)

nitro <- read_fst("./data/intermediate/nitrogen_mass_by_MLI copy.fst")
new_vars <- read_fst("./data/intermediate/new_vars_by_MLI.fst")
# ag <- read_csv("./data/intermediate/indiana_ag_sqkm_by_huc12.csv")
# crp_x_nitro <- read_csv("./data/intermediate/indiana_crp_nitrogen_mass_by_huc12.csv")

# Tidy data
nitro_tidy <- nitro %>% 
    as_tibble() %>%
    select(
        MLI,
        upstream_sqkm_tot = areasqkm_upstream,
        local_sqkm_tot = areasqkm_nearby,
        upstream_kg_N = `0_upstream`,
        local_kg_N = `0_nearby`
    )

ag_tidy <- new_vars %>%
    as_tibble() %>%
    select(MLI, contains("ag_area_sqkm_0"))

nitro_enroll <- nitro %>%
    as_tibble() %>%
    select(
        MLI,
        `2012_upstream`:`2022_upstream`,
        `2012_nearby`:`2022_nearby`
    )
colnames(nitro_enroll)[2:23] <- paste0("kg_N_", colnames(nitro_enroll)[2:23])

nitro_disenroll <- new_vars %>%
    as_tibble() %>%
    select(
        MLI,
        fertilizer_mass_2012_upstream:fertilizer_mass_2022_upstream,
        fertilizer_mass_2012_nearby:fertilizer_mass_2022_nearby
    )

crp_tidy <- new_vars %>%
    as_tibble() %>%
    left_join(wqp_crp) %>%
    select(
        MLI,
        X2012_upstream:X2022_upstream,
        X2012_local:X2022_local,
        crp_area_enroll_2012_upstream:crp_area_enroll_2022_upstream,
        crp_area_disenroll_2012_upstream:crp_area_disenroll_2022_upstream,
        crp_area_enroll_2012_nearby:crp_area_enroll_2022_nearby,
        crp_area_disenroll_2012_nearby:crp_area_disenroll_2022_nearby
    ) %>%
    mutate(across(X2012_upstream:X2022_local, \(x) x*100, .names = "{.col}_hectare")) # convert to hectares

wqp_tidy <- wqp %>%
    filter_wqp() %>% # See lib/ function for parameters
    transmute(
        MLI = MonitoringLocationIdentifier,
        date = ActivityStartDate,
        wq_month = month(ActivityStartDate, label = TRUE),
        wq_year = year(ActivityStartDate),
        wq_conc = ResultMeasureValue
    ) %>%
    filter(wq_conc > 0, MLI %in% nitro_tidy$MLI) %>%
    # When multiple measurements were made on the same day, take the mean
    summarise(wq_conc = mean(wq_conc), .by = MLI:wq_year)

tidy_data <- nitro_tidy %>%
    inner_join(ag_tidy) %>% 
    inner_join(nitro_enroll) %>% 
    inner_join(nitro_disenroll) %>% 
    inner_join(crp_tidy) %>% 
    inner_join(wqp_tidy) %>%
    select(MLI, date, wq_month, wq_year, wq_conc, everything()) #%>%
    # mutate(across(
    #     c(
    #         crp_area_enroll_2012_upstream:crp_area_enroll_2022_upstream,
    #         crp_area_disenroll_2012_upstream:crp_area_disenroll_2022_upstream
    #     ),
    #     \(x) x/ag_area_sqkm_0_upstream
    # )) %>%
    # mutate(across(
    #     c(
    #         crp_area_enroll_2012_nearby:crp_area_enroll_2022_nearby,
    #         crp_area_disenroll_2012_nearby:crp_area_disenroll_2022_nearby
    #     ),
    #     \(x) x/ag_area_sqkm_0_nearby
    # ))

# Create lagged version of data
lagged_data <- tidy_data %>%
    # upstream new kg N
    pivot_longer(
        kg_N_2012_upstream:kg_N_2022_upstream, 
        values_to = "UEN"
    ) %>%
    mutate(
        crp_year = as.numeric(str_extract(name, "[0-9]+")),
        lag = wq_year - crp_year
    )  %>%
    filter(lag >= 0) %>%
    arrange(lag) %>%
    select(-name, -crp_year) %>%
    pivot_wider(
        values_from = UEN, 
        names_from = lag,
        names_prefix = "UEN_lag"
    ) %>%
    # upstream retiring kg N
    pivot_longer(
        fertilizer_mass_2012_upstream:fertilizer_mass_2022_upstream, 
        values_to = "URN"
    ) %>%
    mutate(
        crp_year = as.numeric(str_extract(name, "[0-9]+")),
        lag = wq_year - crp_year
    )  %>%
    filter(lag >= 0) %>%
    arrange(lag) %>%
    select(-name, -crp_year) %>%
    pivot_wider(
        values_from = URN, 
        names_from = lag,
        names_prefix = "URN_lag"
    ) %>%
    # upstream total crp area
    pivot_longer(
        X2012_upstream:X2022_upstream, 
        values_to = "up_crp_tot"
    ) %>%
    mutate(
        crp_year = as.numeric(str_extract(name, "[0-9]+")),
        lag = wq_year - crp_year
    )  %>%
    filter(lag >= 0) %>%
    arrange(lag) %>%
    select(-name, -crp_year) %>%
    pivot_wider(
        values_from = up_crp_tot, 
        names_from = lag,
        names_prefix = "up_crp_tot_lag_"
    ) %>%
    # local total crp area
    pivot_longer(
        X2012_local:X2022_local, 
        values_to = "loc_crp_tot"
    ) %>%
    mutate(
        crp_year = as.numeric(str_extract(name, "[0-9]+")),
        lag = wq_year - crp_year
    )  %>%
    filter(lag >= 0) %>%
    arrange(lag) %>%
    select(-name, -crp_year) %>%
    pivot_wider(
        values_from = loc_crp_tot, 
        names_from = lag,
        names_prefix = "loc_crp_tot_lag_"
    )

lagged_data_w_hucs <- left_join(lagged_data, huc12_mli)

# Average n years of lookback for n = 3 to n = 6
model_data <- lagged_data_w_hucs %>%
    mutate(

        up_area_1to3y = rowMeans(across(up_crp_tot_lag_1:up_crp_tot_lag_3)),
        up_area_1to4y = rowMeans(across(up_crp_tot_lag_1:up_crp_tot_lag_4)),
        up_area_1to5y = rowMeans(across(up_crp_tot_lag_1:up_crp_tot_lag_5)),
        up_area_1to6y = rowMeans(across(up_crp_tot_lag_1:up_crp_tot_lag_6)),

        loc_area_1to3y = rowMeans(across(loc_crp_tot_lag_1:loc_crp_tot_lag_3)),
        loc_area_1to4y = rowMeans(across(loc_crp_tot_lag_1:loc_crp_tot_lag_4)),
        loc_area_1to5y = rowMeans(across(loc_crp_tot_lag_1:loc_crp_tot_lag_5)),
        loc_area_1to6y = rowMeans(across(loc_crp_tot_lag_1:loc_crp_tot_lag_6)),

        up_kg_N_enroll_1to3y = rowMeans(across(UEN_lag1:UEN_lag3)),
        up_kg_N_enroll_1to4y = rowMeans(across(UEN_lag1:UEN_lag4)),
        up_kg_N_enroll_1to5y = rowMeans(across(UEN_lag1:UEN_lag5)),
        up_kg_N_enroll_1to6y = rowMeans(across(UEN_lag1:UEN_lag6)),

        up_kg_N_retire_1to3y = rowMeans(across(URN_lag1:URN_lag3)),
        up_kg_N_retire_1to4y = rowMeans(across(URN_lag1:URN_lag4)),
        up_kg_N_retire_1to5y = rowMeans(across(URN_lag1:URN_lag5)),
        up_kg_N_retire_1to6y = rowMeans(across(URN_lag1:URN_lag6)),
        
        huc8 = str_sub(huc12,,8), 
        huc4 = str_sub(huc12,,4),
        
        up_N_dens = upstream_kg_N/upstream_sqkm_tot,
        up_N_kg_per_hectare = up_N_dens/100 # 100 hectares per sqkm
    
    )

# Run models

old_het <- model_data %>%
    # mutate(
    #     across(
    #         c(starts_with("up_area"), starts_with("loc_area")),
    #         asinh
    #     )
    # ) %>%
    feols(
        fml = log10(wq_conc) ~ 
            sw(
                up_N_dens*up_area_1to3y + up_N_dens*loc_area_1to3y, 
                up_N_dens*up_area_1to4y + up_N_dens*loc_area_1to4y, 
                up_N_dens*up_area_1to5y + up_N_dens*loc_area_1to5y,
                up_N_dens*up_area_1to6y + up_N_dens*loc_area_1to6y
            )|
            wq_month + huc4^wq_year + huc8,
        cluster = "huc8"
    )

new_het <- model_data %>%
    mutate(
        across(
            c(starts_with("up_area"), starts_with("loc_area")),
            asinh
        )
    ) %>%
    feols(
        fml = asinh(wq_conc) ~ 
            sw(
                up_N_kg_per_hectare*up_area_1to3y + up_N_kg_per_hectare*loc_area_1to3y, 
                up_N_kg_per_hectare*up_area_1to4y + up_N_kg_per_hectare*loc_area_1to4y, 
                up_N_kg_per_hectare*up_area_1to5y + up_N_kg_per_hectare*loc_area_1to5y,
                up_N_kg_per_hectare*up_area_1to6y + up_N_kg_per_hectare*loc_area_1to6y
            )|
            wq_month + huc4^wq_year + huc8,
        cluster = "huc8"
    )

# Save model output
options(width = 200)

etable(
    old_het, # log10(WQ) ~ vars
    se.below = FALSE,
    digits.stats = 3
)

etable(
    new_het, # asinh(WQ) ~ asinh(vars)
    se.below = FALSE,
    digits.stats = 3
)
