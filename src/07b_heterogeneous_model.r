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

crp <- read_sf("./data/input/wqp_crp_upstream_and_local.gpkg")
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

crp_tidy <- crp %>%
    filter(MLI %in% nitro_tidy$MLI) %>%
    mutate(
        across(
            X2012_upstream:X2022_upstream,
            \(x) x*area_upstream
        ),
        across(
            X2012_local:X2022_local,
            \(x) x*area_local
        ),
    ) %>%
    rename_with(
        \(x) paste0(str_extract(x, "local|upstream"), "_sqkm_crp_", str_sub(x, 2, 5)), 
        starts_with("X")
    ) %>%
    select(
        MLI,
        huc12,
        state,
        upstream_sqkm_tot = area_upstream,
        local_sqkm_tot = area_local,
        upstream_sqkm_crp_2012:local_sqkm_crp_2022
    )

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

ag_tidy <- new_vars %>%
    as_tibble() %>%
    select(MLI, contains("ag_area_sqkm_0"))

tidy_data <- crp_tidy %>%
    inner_join(ag_tidy) %>% 
    inner_join(nitro_tidy) %>% 
    inner_join(wqp_tidy) %>%
    select(MLI, huc12, state, date, wq_month, wq_year, wq_conc, everything()) %>%
    mutate(across(upstream_sqkm_crp_2012:upstream_sqkm_crp_2022, \(x) x/ag_area_sqkm_0_upstream)) %>%
    mutate(across(local_sqkm_crp_2012:local_sqkm_crp_2022, \(x) x/ag_area_sqkm_0_nearby))

# Create lagged version of data
lagged_data <- tidy_data %>%
    # upstream
    pivot_longer(
        upstream_sqkm_crp_2012:upstream_sqkm_crp_2022, 
        values_to = "upstream_sqkm_crp"
    ) %>%
    mutate(
        crp_year = as.numeric(str_extract(name, "[0-9]+")),
        lag = wq_year - crp_year
    )  %>%
    filter(lag >= 0) %>%
    arrange(lag) %>%
    select(-name, -crp_year) %>%
    pivot_wider(
        values_from = upstream_sqkm_crp, 
        names_from = lag,
        names_prefix = "upstream_sqkm_crp_lag"
    ) %>%
    # local
    pivot_longer(
        local_sqkm_crp_2012:local_sqkm_crp_2022, 
        values_to = "local_sqkm_crp"
    ) %>%
    mutate(
        crp_year = as.numeric(str_extract(name, "[0-9]+")),
        lag = wq_year - crp_year
    )  %>%
    filter(lag >= 0) %>%
    arrange(lag) %>%
    select(-name, -crp_year) %>%
    pivot_wider(
        values_from = local_sqkm_crp, 
        names_from = lag,
        names_prefix = "local_sqkm_crp_lag"
    )

# Average n years of lookback for n = 1 to n = 5
model_data <- lagged_data %>%
    mutate(
        up_area_1to2y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag2)),
        up_area_1to3y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag3)),
        up_area_1to4y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag4)),
        up_area_1to5y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag5)),
        up_area_1to6y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag6)),
        up_area_1to7y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag7)),
        up_area_1to8y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag8)),
        up_area_1to9y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag9)),
        loc_area_1to2y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag2)),
        loc_area_1to3y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag3)),
        loc_area_1to4y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag4)),
        loc_area_1to5y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag5)),
        loc_area_1to6y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag6)),
        loc_area_1to7y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag7)),
        loc_area_1to8y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag8)),
        loc_area_1to9y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag9)),
        huc8 = str_sub(huc12,,8), 
        huc4 = str_sub(huc12,,4),
        up_N_kg_per_hectare = upstream_kg_N/upstream_sqkm_tot/100 # 100 hectares per sqkm
    )

# Run models
m2b <- model_data %>%
    mutate(
        across(
            c(starts_with("up_area"), starts_with("loc_area")),
            asinh
        )
    ) %>%
    feols(
        fml = asinh(wq_conc) ~ 
            sw(
                up_N_kg_per_hectare*up_area_1to2y + up_N_kg_per_hectare*loc_area_1to2y, 
                up_N_kg_per_hectare*up_area_1to3y + up_N_kg_per_hectare*loc_area_1to3y, 
                up_N_kg_per_hectare*up_area_1to4y + up_N_kg_per_hectare*loc_area_1to4y, 
                up_N_kg_per_hectare*up_area_1to5y + up_N_kg_per_hectare*loc_area_1to5y,
                up_N_kg_per_hectare*up_area_1to6y + up_N_kg_per_hectare*loc_area_1to6y, 
                up_N_kg_per_hectare*up_area_1to7y + up_N_kg_per_hectare*loc_area_1to7y, 
                up_N_kg_per_hectare*up_area_1to8y + up_N_kg_per_hectare*loc_area_1to8y, 
                up_N_kg_per_hectare*up_area_1to9y + up_N_kg_per_hectare*loc_area_1to9y
            )|
            wq_month + huc4^wq_year + huc8,
        cluster = "huc8"
    )

# Save model output
options(width = 200)
etable(
    m2b,
    se.below = FALSE,
    digits.stats = 3
)
