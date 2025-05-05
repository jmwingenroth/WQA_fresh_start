# Calculate combined fertilizer intensity of upstream HUC-12s (subset of nearby)

# colnames(mli_fi_nb)[-1] <- paste0("nb_", colnames(mli_fi_nb)[-1])


#  mli_sf %>% 
#     select(
#         Name = MonitoringLocationName, 
#         MLI = MonitoringLocationIdentifier, 
#         MLI_HUC8 = HUCEightDigitCode
#     ) %>%
#     st_buffer(dist = 5e4) %>% # 50 km
#     st_join(fertilizer_sf)

# NOTE: about 1% of MLIs are labeled with HUC-8 codes that don't match
#       their HUC-12s after the join, mostly in Florida. I'm not sure
#       how much of the WQ measurements come from these MLIs. It could
#       be that HUCs were remapped at some point and the MLI data is
#       out of date.
# mli_huc12 %>%
#     filter(MLI_HUC8 != str_sub(huc12,,8))

# wqp_data %>%
#     filter(
#         CharacteristicName == "Ammonia",
#         ResultMeasure.MeasureUnitCode == "mg/L",
#         ProviderName == "STORET",
#         ResultSampleFractionText == "Total", ResultMeasureValue > 1e-6 # no way they're measuring nanograms/ml
#     ) %>%
#     left_join(mli_fi_nb, by = c("MonitoringLocationIdentifier" = "MLI")) %>%
#     ggplot(aes(x = nb_0/1000, y = ResultMeasureValue)) +
#     geom_point(alpha = .2, size = .3) +
#     geom_smooth(method = "lm") +
#     scale_x_sqrt() +
#     scale_y_log10() +
#     labs(x = "Nitrogen fertilizer mass within 50 km (metric tons)", y = "Ammonia concentration (mg/L)")

# mli_huc12 %>%
#     st_drop_geometry() %>%
#     group_by(MLI) %>%
#     tally() %>%
#     filter(n > 1)


# # Tidy fertilizer data
# fertilizer <- left_join()
# left_join(fertilizer, wbd)

# st_centroid(wbd)

# # Tidy fertilizer data
# fertilizer %>%
#     left_join()

# # Focusing on a sizable chunk of the WQP dataset with consistent SOP for now
# # Currently, n = 391,159
# wqp_tidy <- wqp %>%
#     select(
#         MonitoringLocationIdentifier,
#         ActivityStartDate,
#         ResultMeasureValue,
#         CharacteristicName,
#         ResultMeasure.MeasureUnitCode,
#         ProviderName,
#         ResultSampleFractionText
#     ) %>% 
#     filter(
#         CharacteristicName == "Ammonia",
#         ResultMeasure.MeasureUnitCode == "mg/L",
#         ProviderName == "STORET",
#         ResultSampleFractionText == "Total"
#     )

# nrow(mli)

# plot(as.numeric(mli$LongitudeMeasure), as.numeric(mli$LatitudeMeasure))

# wqp_tidy %>%
#     left_join(mli) %>%

#     summarize(min(LongitudeMeasure))

# mli %>%
#     filter(CountryCode == "US") %>%
#     distinct(HorizontalCoordinateReferenceSystemDatumName)
# max(mli$LatitudeMeasure)
# mli %>%
#     select(
#         MonitoringLocationIdentifier,
#         HUCEightDigitCode,
#         Latitude,
#         Longitude,

#     )

# View(head(mli))



#     group_by(
#         ResultMeasure.MeasureUnitCode,
#         CharacteristicName,
#         ProviderName,
#         ResultSampleFractionText
#     ) %>%
#     filter(!is.na(ResultMeasureValue)) %>%
#     summarize(mean = mean(ResultMeasureValue), n = n()) %>%
#     arrange(desc(n)) %>%
#     View()


#     filter(
#         !is.na(ResultMeasure.MeasureUnitCode),
#         !is.na(ResultMeasureValue)
#     ) %>%
#     summarize(
#         min(ResultMeasureValue),
#         quantile(ResultMeasureValue,.01),
#         mean = mean(ResultMeasureValue),
#         quantile(ResultMeasureValue,.99),
#         max(ResultMeasureValue),
#         sd = sd(ResultMeasureValue),
#         n = n()
#     ) %>%
#     arrange(desc(n))

# View(.Last.value)

