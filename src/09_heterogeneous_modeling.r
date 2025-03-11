# Load packages
library(tidyverse)
library(sf)
library(fst)
library(fixest)

# Load function library
invisible(lapply(list.files("src/lib", full.names = TRUE), source))

# Load data
wqp <- read_fst("./data/intermediate/wqp_pull.fst")

nitro <- read_fst("./data/intermediate/nitrogen_mass_by_MLI.fst")
crp <- read_sf("./data/input/wqp_crp_upstream_and_local.gpkg")

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
    filter(wq_conc > 0) %>%
    # When multiple measurements were made on the same day, take the mean
    summarise(wq_conc = mean(wq_conc), .by = MLI:wq_year)

tidy_data <- crp_tidy %>%
    inner_join(nitro_tidy) %>%
    inner_join(wqp_tidy) %>%
    select(MLI, huc12, state, date, wq_month, wq_year, wq_conc, everything())

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
        up_N_dens = upstream_kg_N/upstream_sqkm_tot
    ) %>%
    mutate(across(up_area_1to2y:loc_area_1to9y, \(x) x*local_kg_N, .names = "{.col}_x_locN")) %>%
    mutate(across(up_area_1to2y:loc_area_1to9y, \(x) x*upstream_kg_N, .names = "{.col}_x_upN")) %>%
    mutate(across(up_area_1to2y:loc_area_1to9y, \(x) x*up_N_dens, .names = "{.col}_x_upNdens"))

# Run models
m2b <- model_data %>%
    mutate(
        across(
            c(up_N_dens, starts_with("up_area"), starts_with("loc_area")),
            asinh
        )
    ) %>%
    feols(
        fml = asinh(wq_conc) ~ 
            sw(
                up_N_dens*up_area_1to2y + up_N_dens*loc_area_1to2y, 
                up_N_dens*up_area_1to3y + up_N_dens*loc_area_1to3y, 
                up_N_dens*up_area_1to4y + up_N_dens*loc_area_1to4y, 
                up_N_dens*up_area_1to5y + up_N_dens*loc_area_1to5y,
                up_N_dens*up_area_1to6y + up_N_dens*loc_area_1to6y, 
                up_N_dens*up_area_1to7y + up_N_dens*loc_area_1to7y, 
                up_N_dens*up_area_1to8y + up_N_dens*loc_area_1to8y, 
                up_N_dens*up_area_1to9y + up_N_dens*loc_area_1to9y
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

# Make a table of local and upstream CRP sqkm deciles

deciles <- seq(0,1,.1)
tidy_data %>%
    st_drop_geometry() %>%
    summarise(across(
        upstream_sqkm_crp_2012:local_sqkm_crp_2022, 
        ~round(quantile(.x, probs = deciles), 2))
    ) %>%
    rename_all(~str_replace(.x, "upstream_sqkm_crp_", "up_")) %>%
    rename_all(~str_replace(.x, "local_sqkm_crp_", "loc_")) %>%
    mutate(quantile = deciles) %>%
    select(quantile, everything())


