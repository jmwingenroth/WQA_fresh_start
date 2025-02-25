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

# Run model
lookback <- c(1) # year(s)

model <- lagged_data %>%
    mutate(
        huc8 = str_sub(huc12,,8), 
        huc4 = str_sub(huc12,,4)
    ) %>%
    feols(
        log10(wq_conc) ~
            local_sqkm_crp_lag.[lookback] +
            local_sqkm_crp_lag.[lookback] * local_kg_N +
            upstream_sqkm_crp_lag.[lookback] +
            upstream_sqkm_crp_lag.[lookback] * upstream_kg_N |
            wq_month + huc4^wq_year + huc8,
        cluster = "huc8"
    )

print(model)

# Make data coverage plot

p1 <- tidy_data %>%
    ggplot() +
    geom_sf(aes(color = as.numeric(huc12)))

ggsave("figs/test/09_data_coverage.svg", p1)

crp_tidy %>%
    filter(rowSums(across(upstream_sqkm_crp_2012:local_sqkm_crp_2022)) > 0)
