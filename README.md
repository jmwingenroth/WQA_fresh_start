# Effects of CRP on downstream water quality

Jordan Wingenroth (jwing421@gmail.com)

## Documentation

### Disclaimer

This code is not currently in a tidy, reproducible state. This README is intended as a debrief so that other team members can use the code in case Jordan is unavailable.

### Notes for Jordan and Penny's 7/22/2025 meeting

The code in `src/` performs several purposes:

- loads water quality data from the [Water Quality Portal](https://www.waterqualitydata.us/)
    - `01_wqp_pull.r`
- gets metadata from water quality sampling stations
    - `02_mli_pull.r`
- creates rasters of fertilizer intensity using the [Cropland Data Layer](https://www.nass.usda.gov/Research_and_Science/Cropland/Release/) and a dataset borrowed from [Ludemann et al. 2022](https://www.nature.com/articles/s41597-022-01592-z)
    - `03_fertilizer_intensity.r`
- makes rasters where pixels are populated with enrollment (first year appearing in rasterized CRP data) and disenrollment (last year appearing in rasterized CRP data)
    - `04_crp_enrollment.r`
