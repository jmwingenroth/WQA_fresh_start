library(dataRetrieval)
library(dplyr)

iterate_wqp_download <- function(
    characteristic_names,
    state_abbreviations,
    start_date
) {

    wqp_params <- expand.grid(
        chem = characteristic_names, 
        state = state_abbreviations
    )

    wqp_pull_data <- list()
    for (i in 1:nrow(wqp_params)) {
        print(paste0(
            "Querying '", 
            wqp_params$chem[i], 
            "' in ", 
            wqp_params$state[i], 
            ": ", 
            i, 
            " of ", 
            nrow(wqp_params)
        ))
        temp <- NULL
        try(
            suppressMessages({
                temp <- readWQPdata(
                    characteristicName = wqp_params$chem[i],
                    statecode = wqp_params$state[i],
                    startDate = start_date
                    ) %>%
                    as_tibble()
            }),
            silent = TRUE
        )
        if(!is.null(temp)) {
            wqp_pull_data[[i]] <- temp
        } else {
            print("Query failed")
            wqp_pull_data[[i]] <- NA # logical
        }
    }

    # Retry failed queries
    for (i in 1:length(wqp_pull_data)) {
        if (typeof(wqp_pull_data[[i]]) == "logical") {
            print(paste0(
                "Retrying '", 
                wqp_params$chem[i], 
                "' in ", 
                wqp_params$state[i]
            ))
            temp <- NULL
            try(
                suppressMessages({
                    temp <- readWQPdata(
                        characteristicName = wqp_params$chem[i],
                        statecode = wqp_params$state[i],
                        startDate = start_date
                        ) %>%
                        as_tibble()
                }),
                silent = TRUE
            )
            if(!is.null(temp)) {
                wqp_pull_data[[i]] <- temp
            } else {
                print("Query failed again")
                wqp_pull_data[[i]] <- NA # logical
            }
        }
    }

    return(wqp_pull_data)

}

select_WQP_chars <- function(
    csv_path,
    pattern
) {
    raw <- read.csv(csv_path)
    nh3_handles <- raw[grep(pattern, raw$charhandle), c("CharacteristicName", "charhandle")]
    return(nh3_handles)
}
