library(dplyr)

filter_wqp <- function(

    raw_data,
    characteristic_name = "Ammonia",
    result_measure = "mg/L",
    provider_name = "STORET",
    sample_fraction = "Total"

) {

    raw_data %>%
        filter(
            CharacteristicName == characteristic_name,
            ResultMeasure.MeasureUnitCode == result_measure,
            ProviderName == provider_name,
            ResultSampleFractionText == sample_fraction
        )

}
