# Load libraries
library(tidyverse)
library(fixest)

# Load data
model_data <- read_csv("data/output/model_data.csv")

# Plot sample size vs lookback length
lookback_length_plot <- model_data %>%
    summarise(across(everything(), ~sum(!is.na(.x)))) %>%
    pivot_longer(N_removed_lag_1:N_removed_lag_11) %>%
    mutate(name = as.numeric(str_extract(name, "[0-9]+"))) %>%
    ggplot(aes(x = name, y = value)) +
    geom_col() +
    labs(x = "lookback length", y = "number of observations")

ggsave("figs/test/08_sample_size_as_function_of_lookback_length.svg", lookback_length_plot)

# Run models
model_data %>%
    mutate(
        sum_4_9_yr_n_removed = log10(rowSums(across(c(N_removed_lag_4:N_removed_lag_9)))),
        sum_4_8_yr_n_removed = log10(rowSums(across(c(N_removed_lag_4:N_removed_lag_8)))),
        sum_4_7_yr_n_removed = log10(rowSums(across(c(N_removed_lag_4:N_removed_lag_7)))),
        sum_4_6_yr_n_removed = log10(rowSums(across(c(N_removed_lag_4:N_removed_lag_6)))),
        sum_4_5_yr_n_removed = log10(rowSums(across(c(N_removed_lag_4:N_removed_lag_5)))),
        HUC4 = str_sub(HUC8,,4)
    ) %>%
    feols(
        fml = log(wq_conc) ~
            log(total_N_upstream) +
            sw(
                log(sum_4_9_yr_n_removed),
                log(sum_4_8_yr_n_removed),
                log(sum_4_7_yr_n_removed),
                log(sum_4_6_yr_n_removed),
                log(sum_4_5_yr_n_removed)
            ) |
            wq_month + 
            HUC8 +
            HUC4^wq_year,
        cluster = "HUC8"
    ) %>%
    etable(
        file = "data/output/model_results.tex",
        tex = TRUE,
        replace = TRUE,
        digits = 4,
        digits.stats = 3
    )
