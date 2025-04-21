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

huc12_key <- read_sf("./data/input/wqp_crp_upstream_and_local.gpkg") %>%
    as_tibble() %>%
    st_drop_geometry() %>%
    select(MLI, huc12)

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
    select(
        MLI,
        crp_area_enroll_2012_upstream:crp_area_enroll_2022_upstream,
        crp_area_disenroll_2012_upstream:crp_area_disenroll_2022_upstream,
        crp_area_enroll_2012_nearby:crp_area_enroll_2022_nearby,
        crp_area_disenroll_2012_nearby:crp_area_disenroll_2022_nearby
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
    # upstream new crp area
    pivot_longer(
        crp_area_enroll_2012_upstream:crp_area_enroll_2022_upstream, 
        values_to = "UEA"
    ) %>%
    mutate(
        crp_year = as.numeric(str_extract(name, "[0-9]+")),
        lag = wq_year - crp_year
    )  %>%
    filter(lag >= 0) %>%
    arrange(lag) %>%
    select(-name, -crp_year) %>%
    pivot_wider(
        values_from = UEA, 
        names_from = lag,
        names_prefix = "UEA_lag"
    ) %>%
    # upstream retiring crp area
    pivot_longer(
        crp_area_disenroll_2012_upstream:crp_area_disenroll_2022_upstream, 
        values_to = "URA"
    ) %>%
    mutate(
        crp_year = as.numeric(str_extract(name, "[0-9]+")),
        lag = wq_year - crp_year
    )  %>%
    filter(lag >= 0) %>%
    arrange(lag) %>%
    select(-name, -crp_year) %>%
    pivot_wider(
        values_from = URA, 
        names_from = lag,
        names_prefix = "URA_lag"
    ) %>%
    # local new crp area
    pivot_longer(
        crp_area_enroll_2012_nearby:crp_area_enroll_2022_nearby, 
        values_to = "LEA"
    ) %>%
    mutate(
        crp_year = as.numeric(str_extract(name, "[0-9]+")),
        lag = wq_year - crp_year
    )  %>%
    filter(lag >= 0) %>%
    arrange(lag) %>%
    select(-name, -crp_year) %>%
    pivot_wider(
        values_from = LEA, 
        names_from = lag,
        names_prefix = "LEA_lag"
    ) %>%
    # local retiring crp area
    pivot_longer(
        crp_area_disenroll_2012_nearby:crp_area_disenroll_2022_nearby, 
        values_to = "LRA"
    ) %>%
    mutate(
        crp_year = as.numeric(str_extract(name, "[0-9]+")),
        lag = wq_year - crp_year
    )  %>%
    filter(lag >= 0) %>%
    arrange(lag) %>%
    select(-name, -crp_year) %>%
    pivot_wider(
        values_from = LRA, 
        names_from = lag,
        names_prefix = "LRA_lag"
    )

lagged_data_w_hucs <- left_join(lagged_data, huc12_key)

# Average n years of lookback for n = x
model_data_5 <- lagged_data_w_hucs %>%
    mutate(
        UEN = rowMeans(across(UEN_lag1:UEN_lag5)),
        URN = rowMeans(across(URN_lag1:URN_lag5)),
        UEA = rowMeans(across(UEA_lag1:UEA_lag5)),
        URA = rowMeans(across(URA_lag1:URA_lag5)),
        LEA = rowMeans(across(LEA_lag1:LEA_lag5)),
        LRA = rowMeans(across(LRA_lag1:LRA_lag5)),
        huc8 = str_sub(huc12,,8), 
        huc4 = str_sub(huc12,,4),
        up_tot_N_kg_per_hectare = upstream_kg_N/upstream_sqkm_tot/100, # 100 hectares per sqkm
        up_enroll_N_kg_per_hectare = UEN/UEA/100,
        up_retire_N_kg_per_hectare = URN/URA/100,
        up_enroll_area_hectare = UEA/100,
        up_retire_area_hectare = URA/100,
        local_enroll_area_hectare = LEA/100,
        local_retire_area_hectare = LRA/100
    )

model_data_4 <- lagged_data_w_hucs %>%
    mutate(
        UEN = rowMeans(across(UEN_lag1:UEN_lag4)),
        URN = rowMeans(across(URN_lag1:URN_lag4)),
        UEA = rowMeans(across(UEA_lag1:UEA_lag4)),
        URA = rowMeans(across(URA_lag1:URA_lag4)),
        LEA = rowMeans(across(LEA_lag1:LEA_lag4)),
        LRA = rowMeans(across(LRA_lag1:LRA_lag4)),
        huc8 = str_sub(huc12,,8), 
        huc4 = str_sub(huc12,,4),
        up_tot_N_kg_per_hectare = upstream_kg_N/upstream_sqkm_tot/100, # 100 hectares per sqkm
        up_enroll_N_kg_per_hectare = UEN/UEA/100,
        up_retire_N_kg_per_hectare = URN/URA/100,
        up_enroll_area_hectare = UEA/100,
        up_retire_area_hectare = URA/100,
        local_enroll_area_hectare = LEA/100,
        local_retire_area_hectare = LRA/100
    )

model_data_6 <- lagged_data_w_hucs %>%
    mutate(
        UEN = rowMeans(across(UEN_lag1:UEN_lag6)),
        URN = rowMeans(across(URN_lag1:URN_lag6)),
        UEA = rowMeans(across(UEA_lag1:UEA_lag6)),
        URA = rowMeans(across(URA_lag1:URA_lag6)),
        LEA = rowMeans(across(LEA_lag1:LEA_lag6)),
        LRA = rowMeans(across(LRA_lag1:LRA_lag6)),
        huc8 = str_sub(huc12,,8), 
        huc4 = str_sub(huc12,,4),
        up_tot_N_kg_per_hectare = upstream_kg_N/upstream_sqkm_tot/100, # 100 hectares per sqkm
        up_enroll_N_kg_per_hectare = UEN/UEA/100,
        up_retire_N_kg_per_hectare = URN/URA/100,
        up_enroll_area_hectare = UEA/100,
        up_retire_area_hectare = URA/100,
        local_enroll_area_hectare = LEA/100,
        local_retire_area_hectare = LRA/100
    )

models_data <- list(model_data_4, model_data_5, model_data_6)
models <- list()
for (i in seq_along(models_data)) {

    models[[i]] <- models_data[[i]] %>%
        # mutate(
        #     across(
        #         c(ends_with("A_1to5")),
        #         asinh
        #     )
        # ) %>%
        feols(
            fml = asinh(wq_conc) ~ 
                (up_tot_N_kg_per_hectare + up_enroll_N_kg_per_hectare) * (local_enroll_area_hectare + up_enroll_area_hectare) + 
                (up_tot_N_kg_per_hectare + up_retire_N_kg_per_hectare) * (local_retire_area_hectare + up_retire_area_hectare) |
                wq_month + huc4^wq_year + huc8,
            cluster = "huc8"
        )

}

# Save model output
options(width = 200)
etable(
    lag_4 = models[[1]],
    lag_5 = models[[2]],
    lag_6 = models[[3]],
    se.below = FALSE,
    digits.stats = 3
)
