library(tidyverse)
library(fst)

source("src/lib/wqp_filter.r")

pre <- read_fst(
  "/Users/jordanwingenroth/Dropbox/RFF_WQA/data/intermediate/wqp_pull.fst"
)

mli_pull <- read_fst(
  "/Users/jordanwingenroth/Dropbox/RFF_WQA/data/intermediate/mli_pull.fst"
)

post <- read_fst(
  "/Users/jordanwingenroth/Dropbox/RFF_WQA/data/intermediate/nitrogen_mass_by_MLI copy.fst"
)

new_vars <- read_fst(
  "/Users/jordanwingenroth/Dropbox/RFF_WQA/data/intermediate/new_vars_by_MLI.fst"
)


pre %>%
  as_tibble() %>%
  distinct(MonitoringLocationIdentifier) %>%
  nrow()
# Sites in wqp_pull.fst: 143032

pre %>%
  as_tibble() %>%
  filter_wqp() %>%
  as_tibble() %>%
  distinct(MonitoringLocationIdentifier) %>%
  nrow()
# Sites after filtering with criteria in src/lib/wqp_filter.r : 39634

pre %>%
  as_tibble() %>%
  filter_wqp() %>%
  filter(ResultMeasureValue > 0) %>%
  as_tibble() %>%
  distinct(MonitoringLocationIdentifier) %>%
  nrow()
# Sites after dropping 0 (and negative) measurements: 36203

post %>%
  as_tibble() %>%
  distinct(MLI) %>%
  nrow()
# Sites in nitrogen_mass_by_MLI copy.fst: 25732

new_vars %>%
  as_tibble() %>%
  distinct(MLI) %>%
  nrow()
# Sites in new_vars_by_MLI.fst: 25732
