# Parameters
wqp_start_date <- "2010-01-01"

# Load function library
invisible(lapply(list.files("src/lib", full.names = TRUE), source))

# Load strings to query WQP for ammonia data
nh3_handles <- select_WQP_chars(
    "data/input/wqp_water_chars_handles.csv", 
    "ammonia"
)

# Pull data from WQP server
wqp_pull_data <- iterate_wqp_download(nh3_handles$CharacteristicName, c("AL", "GA"), wqp_start_date)

# Bind rows of data
# ...
