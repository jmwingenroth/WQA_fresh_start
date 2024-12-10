# Load packages
library(dataRetrieval)
library(dplyr)
library(fst)

# Load parameters
query_size <- 3000
mli_convert_to_numeric <- c(
    "HorizontalAccuracyMeasure.MeasureValue",
    "VerticalMeasure.MeasureValue"
)

# Load function library
invisible(lapply(list.files("src/lib", full.names = TRUE), source))

# Load WQP pull data
wqp_pull <- read.fst("data/intermediate/wqp_pull.fst")

# Get distinct MLI values
mli_names <- unique(wqp_pull$MonitoringLocationIdentifier)

# Pull WQP site data
mli_pull_data <- iterate_mli_download(mli_names)

# Bind rows and save data
mli_pull_data %>%
    lapply(mutate, across(mli_convert_to_numeric, as.numeric)) %>%
    bind_rows() %>%
    write.fst("data/intermediate/mli_pull.fst")
