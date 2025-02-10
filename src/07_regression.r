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
        month = month(ActivityStartDate, label = TRUE),
        year = year(ActivityStartDate),
        value = ResultMeasureValue
    ) %>%
    filter(value > 0, year > 2013) %>% # 2013 is the first year with marginal CRP data
    left_join(mli_huc8) %>%
    left_join(fert_raw) %>%
    # Restrict to ever-treated (upstream), not counting CRP on "0 fertilizer" land
    filter(rowSums(across(`2012_upstream`:`2022_upstream`)) > 0)

tidy_data %>% View()
