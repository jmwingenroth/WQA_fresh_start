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

# 143032

pre %>%
  as_tibble() %>%
  filter_wqp() %>%
  filter(MonitoringLocationIdentifier %in% nitro_tidy$MLI) %>%
  as_tibble() %>%
  distinct(MonitoringLocationIdentifier) %>%
  nrow()

# 39634

pre %>%
  group_by(CharacteristicName,ResultMeasure.MeasureUnitCode,ResultSampleFractionText) %>%
  tally() %>%
  arrange(desc(n))

# Filter out concentrations < or = 0
# 36203

pre %>%
  as_tibble() %>%
  filter_wqp() %>%
  filter(MonitoringLocationIdentifier %in% nitro_tidy$MLI) %>%
  as_tibble() %>%
  distinct(MonitoringLocationIdentifier) %>%
  nrow()

# 25732

post %>%
  as_tibble() %>%
  distinct(MLI) %>%
  nrow()

# 25732

new_vars %>%
  as_tibble() %>%
  distinct(MLI) %>%
  nrow()

# 25732
