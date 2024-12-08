library(dataRetrieval)
library(dplyr)

iterate_mli_download <- function(
    mli_names, 
    max_query_size = 3000
    ) {

    mli_queries <- split(mli_names, ceiling(seq_along(mli_names) / max_query_size))

    mli_pull_data <- list()
    for (i in 1:length(mli_queries)) {
        print(paste0("Beginning WQP site metadata query ", i, " of ", length(mli_queries)))
        mli_pull_data[[i]] <- whatWQPsites(siteid = mli_queries[[i]])
    }

    if (sum(sapply(mli_pull_data, nrow)) != length(mli_names)) {
        warning("Data could not be retrieved for some sites")
    }

    return(mli_pull_data)

}

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

select_wqp_chars <- function(
    csv_path,
    pattern
    ) {
    raw <- read.csv(csv_path)
    nh3_handles <- raw[grep(pattern, raw$charhandle), c("CharacteristicName", "charhandle")]
    return(nh3_handles)
}
