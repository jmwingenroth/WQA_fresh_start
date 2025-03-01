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
        huc4 = str_sub(huc12,,4)
    ) %>%
    mutate(across(up_area_1to2y:loc_area_1to9y, \(x) x*local_kg_N, .names = "{.col}_x_locN")) %>%
    mutate(across(up_area_1to2y:loc_area_1to9y, \(x) x*upstream_kg_N, .names = "{.col}_x_upN"))

# Run models
m1 <- model_data %>%
    feols(
        fml = log10(wq_conc) ~ 
            sw(
                local_kg_N*up_area_1to2y + local_kg_N*loc_area_1to2y, 
                local_kg_N*up_area_1to3y + local_kg_N*loc_area_1to3y, 
                local_kg_N*up_area_1to4y + local_kg_N*loc_area_1to4y, 
                local_kg_N*up_area_1to5y + local_kg_N*loc_area_1to5y,
                local_kg_N*up_area_1to6y + local_kg_N*loc_area_1to6y, 
                local_kg_N*up_area_1to7y + local_kg_N*loc_area_1to7y, 
                local_kg_N*up_area_1to8y + local_kg_N*loc_area_1to8y, 
                local_kg_N*up_area_1to9y + local_kg_N*loc_area_1to9y
            )|
            wq_month + huc4^wq_year + huc8,
        cluster = "huc8"
    )

m2 <- model_data %>%
    feols(
        fml = log10(wq_conc) ~ 
            sw(
                upstream_kg_N*up_area_1to2y + upstream_kg_N*loc_area_1to2y, 
                upstream_kg_N*up_area_1to3y + upstream_kg_N*loc_area_1to3y, 
                upstream_kg_N*up_area_1to4y + upstream_kg_N*loc_area_1to4y, 
                upstream_kg_N*up_area_1to5y + upstream_kg_N*loc_area_1to5y,
                upstream_kg_N*up_area_1to6y + upstream_kg_N*loc_area_1to6y, 
                upstream_kg_N*up_area_1to7y + upstream_kg_N*loc_area_1to7y, 
                upstream_kg_N*up_area_1to8y + upstream_kg_N*loc_area_1to8y, 
                upstream_kg_N*up_area_1to9y + upstream_kg_N*loc_area_1to9y
            )|
            wq_month + huc4^wq_year + huc8,
        cluster = "huc8"
    )

m3 <- model_data %>%
    feols(
        fml = log10(wq_conc) ~ 
            sw(
                up_area_1to2y + loc_area_1to2y + up_area_1to2y_x_locN + loc_area_1to2y_x_locN, 
                up_area_1to3y + loc_area_1to3y + up_area_1to3y_x_locN + loc_area_1to3y_x_locN, 
                up_area_1to4y + loc_area_1to4y + up_area_1to4y_x_locN + loc_area_1to4y_x_locN, 
                up_area_1to5y + loc_area_1to5y + up_area_1to5y_x_locN + loc_area_1to5y_x_locN, 
                up_area_1to6y + loc_area_1to6y + up_area_1to6y_x_locN + loc_area_1to6y_x_locN, 
                up_area_1to7y + loc_area_1to7y + up_area_1to7y_x_locN + loc_area_1to7y_x_locN, 
                up_area_1to8y + loc_area_1to8y + up_area_1to8y_x_locN + loc_area_1to8y_x_locN, 
                up_area_1to9y + loc_area_1to9y + up_area_1to9y_x_locN + loc_area_1to9y_x_locN
            )|
            wq_month + huc4^wq_year + huc8,
        cluster = "huc8"
    )

m4 <- model_data %>%
    feols(
        fml = log10(wq_conc) ~ 
            sw(
                up_area_1to2y + loc_area_1to2y + up_area_1to2y_x_upN + loc_area_1to2y_x_upN, 
                up_area_1to3y + loc_area_1to3y + up_area_1to3y_x_upN + loc_area_1to3y_x_upN, 
                up_area_1to4y + loc_area_1to4y + up_area_1to4y_x_upN + loc_area_1to4y_x_upN, 
                up_area_1to5y + loc_area_1to5y + up_area_1to5y_x_upN + loc_area_1to5y_x_upN, 
                up_area_1to6y + loc_area_1to6y + up_area_1to6y_x_upN + loc_area_1to6y_x_upN, 
                up_area_1to7y + loc_area_1to7y + up_area_1to7y_x_upN + loc_area_1to7y_x_upN, 
                up_area_1to8y + loc_area_1to8y + up_area_1to8y_x_upN + loc_area_1to8y_x_upN, 
                up_area_1to9y + loc_area_1to9y + up_area_1to9y_x_upN + loc_area_1to9y_x_upN
            )|
            wq_month + huc4^wq_year + huc8,
        cluster = "huc8"
    )

m5 <- model_data %>%
    feols(
        fml = log10(wq_conc) ~ 
            sw(
                local_kg_N + up_area_1to2y_x_locN + loc_area_1to2y_x_locN, 
                local_kg_N + up_area_1to3y_x_locN + loc_area_1to3y_x_locN, 
                local_kg_N + up_area_1to4y_x_locN + loc_area_1to4y_x_locN, 
                local_kg_N + up_area_1to5y_x_locN + loc_area_1to5y_x_locN, 
                local_kg_N + up_area_1to6y_x_locN + loc_area_1to6y_x_locN, 
                local_kg_N + up_area_1to7y_x_locN + loc_area_1to7y_x_locN, 
                local_kg_N + up_area_1to8y_x_locN + loc_area_1to8y_x_locN, 
                local_kg_N + up_area_1to9y_x_locN + loc_area_1to9y_x_locN
            )|
            wq_month + huc4^wq_year + huc8,
        cluster = "huc8"
    )

m6 <- model_data %>%
    feols(
        fml = log10(wq_conc) ~ 
            sw(
                upstream_kg_N + up_area_1to2y_x_upN + loc_area_1to2y_x_upN, 
                upstream_kg_N + up_area_1to3y_x_upN + loc_area_1to3y_x_upN, 
                upstream_kg_N + up_area_1to4y_x_upN + loc_area_1to4y_x_upN, 
                upstream_kg_N + up_area_1to5y_x_upN + loc_area_1to5y_x_upN, 
                upstream_kg_N + up_area_1to6y_x_upN + loc_area_1to6y_x_upN, 
                upstream_kg_N + up_area_1to7y_x_upN + loc_area_1to7y_x_upN, 
                upstream_kg_N + up_area_1to8y_x_upN + loc_area_1to8y_x_upN, 
                upstream_kg_N + up_area_1to9y_x_upN + loc_area_1to9y_x_upN
            )|
            wq_month + huc4^wq_year + huc8,
        cluster = "huc8"
    )

# Save model data
m_list <- list(
    m1 = m1,
    m2 = m2,
    m3 = m3,
    m4 = m4,
    m5 = m5,
    m6 = m6
)

for (i in seq_along(m_list)) {
    etable(
        m_list[i],
        file = paste0("data/output/",names(m_list[i]),"_results.tex"),
        tex = TRUE,
        replace = TRUE,
        digits = 4,
        digits.stats = 3
    )
}

# Make pdf
repo_wd <- getwd()
setwd("data/output")
tools::texi2pdf("doc.tex", clean = TRUE)
setwd(repo_wd)

# Make data coverage plot
p1 <- tidy_data %>%
    ggplot() +
    geom_sf(aes(color = as.numeric(huc12)))

ggsave("figs/test/09_data_coverage.svg", p1)
