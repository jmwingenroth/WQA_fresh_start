# Load libraries
library(tidyverse)
library(fst)

# Load function library
invisible(lapply(list.files("src/lib", full.names = TRUE), source))

# Load data
wqp_raw <- read_fst("./data/intermediate/wqp_pull.fst") %>% as_tibble()
mli_raw <- read_fst("./data/intermediate/mli_pull.fst") %>% as_tibble()
fert_raw <- read_fst("./data/intermediate/nitrogen_mass_by_MLI.fst") %>% as_tibble()

# Create MLI to HUC8 key
mli_huc8 <- mli_raw %>%
    select(MLI = MonitoringLocationIdentifier, HUC8 = HUCEightDigitCode)

# Combine WQP data with HUC8 and fertilizer mass data
tidy_data <- wqp_raw %>%
    filter_wqp() %>% # See lib/ function for parameters
    transmute(
        MLI = MonitoringLocationIdentifier,
        date = ActivityStartDate,
        wq_month = month(ActivityStartDate, label = TRUE),
        wq_year = year(ActivityStartDate),
        wq_conc = ResultMeasureValue
    ) %>%
    filter(wq_conc > 0, wq_year > 2013) %>% # 2013 is the first year with marginal CRP data
    # When multiple measurements were made on the same day, take the mean
    summarise(wq_conc = mean(wq_conc), .by = MLI:wq_year) %>%
    left_join(mli_huc8) %>%
    left_join(fert_raw) %>%
    rename(total_N_upstream = `0_upstream`, total_N_nearby = `0_nearby`) %>%
    # Restrict to ever-treated (upstream), not counting CRP on "0 fertilizer" land
    filter(rowSums(across(`2012_upstream`:`2022_upstream`)) > 0)

# Convert fertilizer mass data to lagged format
tidy_lagged_data <- tidy_data %>%
    select(-c(`2012_nearby`:`2022_nearby`, `2012_upstream`)) %>%
    pivot_longer(`2013_upstream`:`2022_upstream`, values_to = "crp_fert") %>%
    mutate(fert_year = as.numeric(str_extract(name, "[0-9]*"))) %>%
    filter(fert_year < wq_year) %>%
    mutate(lag = wq_year - fert_year) %>%
    select(-name, -fert_year) %>%
    pivot_wider(names_from = lag, values_from = crp_fert, names_prefix = "N_removed_lag_")

write_csv(tidy_lagged_data, "data/output/model_data.csv")

wq_N_plot <- tidy_lagged_data %>%
    ggplot(aes(x = total_N_upstream, y = wq_conc)) +
    scale_y_log10() +
    scale_x_log10() +
    geom_jitter(size = .2, width = .005, height = .005) +
    geom_smooth(method = lm)

ggsave("figs/test/07_log_log_wq_measurements_as_function_of_upstream_N.svg", wq_N_plot)
