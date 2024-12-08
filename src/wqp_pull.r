# Load packages
library(dataRetrieval)
library(dplyr)
library(fst)

# Load parameters
wqp_start_date <- "2010-01-01"
wqp_convert_to_numeric <- c(
    "ResultMeasureValue",
    "PrecisionValue",
    "ActivityTopDepthHeightMeasure.MeasureValue",
    "ActivityBottomDepthHeightMeasure.MeasureValue",
    "ActivityDepthHeightMeasure.MeasureValue",
    "ResultDepthHeightMeasure.MeasureValue",
    "DetectionQuantitationLimitMeasure.MeasureValue"
)

# Load function library
invisible(lapply(list.files("src/lib", full.names = TRUE), source))

# Load strings to query WQP for ammonia data
nh3_handles <- select_WQP_chars(
    "data/input/wqp_water_chars_handles.csv", 
    "ammonia"
)

# Pull data from WQP server
wqp_pull_data <- iterate_wqp_download(
    nh3_handles$CharacteristicName, 
    state.abb, 
    wqp_start_date
)

# Bind rows of data and save
wqp_pull_data %>%
    lapply(mutate, across(wqp_convert_to_numeric, as.numeric)) %>%
    bind_rows() %>%
    write.fst("data/intermediate/wqp_pull.fst")
