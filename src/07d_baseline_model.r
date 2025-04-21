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

new_vars <- read_fst("./data/intermediate/new_vars_by_MLI.fst")

# Tidy data
wqp_tidy <- wqp %>%
    filter_wqp() %>% # See lib/ function for parameters
    as_tibble() %>%
    transmute(
        MLI = MonitoringLocationIdentifier,
        date = ActivityStartDate,
        wq_month = month(ActivityStartDate, label = TRUE),
        wq_year = year(ActivityStartDate),
        wq_conc = ResultMeasureValue
    ) %>%
    filter(wq_conc > 0, MLI %in% wqp_crp$MLI) %>%
    # When multiple measurements were made on the same day, take the mean
    summarise(wq_conc = mean(wq_conc), .by = MLI:wq_year)

ag_tidy <- new_vars %>%
    as_tibble() %>%
    select(MLI, contains("ag_area_sqkm_0"))

tidy_data <- wqp_tidy %>%
    left_join(wqp_crp) %>%
    left_join(ag_tidy)

# Lag data
lagged_data <- tidy_data %>%
    # upstream
    pivot_longer(
        X2012_upstream:X2022_upstream, 
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
        X2012_local:X2022_local, 
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

# Average n years of lookback for n = 2 to n = 9
model_data <- lagged_data %>%
    mutate(
        up_area_1to2y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag2))/ag_area_sqkm_0_upstream,
        up_area_1to3y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag3))/ag_area_sqkm_0_upstream,
        up_area_1to4y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag4))/ag_area_sqkm_0_upstream,
        up_area_1to5y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag5))/ag_area_sqkm_0_upstream,
        up_area_1to6y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag6))/ag_area_sqkm_0_upstream,
        up_area_1to7y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag7))/ag_area_sqkm_0_upstream,
        up_area_1to8y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag8))/ag_area_sqkm_0_upstream,
        up_area_1to9y = rowMeans(across(upstream_sqkm_crp_lag1:upstream_sqkm_crp_lag9))/ag_area_sqkm_0_upstream,
        loc_area_1to2y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag2))/ag_area_sqkm_0_nearby,
        loc_area_1to3y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag3))/ag_area_sqkm_0_nearby,
        loc_area_1to4y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag4))/ag_area_sqkm_0_nearby,
        loc_area_1to5y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag5))/ag_area_sqkm_0_nearby,
        loc_area_1to6y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag6))/ag_area_sqkm_0_nearby,
        loc_area_1to7y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag7))/ag_area_sqkm_0_nearby,
        loc_area_1to8y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag8))/ag_area_sqkm_0_nearby,
        loc_area_1to9y = rowMeans(across(local_sqkm_crp_lag1:local_sqkm_crp_lag9))/ag_area_sqkm_0_nearby,
        huc8 = str_sub(huc12,,8), 
        huc4 = str_sub(huc12,,4)
    )

# Run models
m2b <- model_data %>%
    # mutate(
    #     across(
    #         c(starts_with("up_area"), starts_with("loc_area")),
    #         \(x) x*100 # Convert to hectares
    #     )
    # ) %>%
    feols(
        fml = asinh(wq_conc) ~ 
            sw(
                up_area_1to2y + loc_area_1to2y, 
                up_area_1to3y + loc_area_1to3y, 
                up_area_1to4y + loc_area_1to4y, 
                up_area_1to5y + loc_area_1to5y,
                up_area_1to6y + loc_area_1to6y, 
                up_area_1to7y + loc_area_1to7y, 
                up_area_1to8y + loc_area_1to8y, 
                up_area_1to9y + loc_area_1to9y
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
