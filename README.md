# solar_forecast

## Overview
This R script allows the user to upload electric load data on solar generation, estimate a model based on weather-related variables and generate a forecast of solar output.  The solar generation output data can be found [here](https://dataminer2.pjm.com/feed/solar_gen).

The data is hourly and segmented into regions within the ISO.

## Dependencies

This script was developed in the RStudio environment and will require the following libraries to be installed:
* stats
* data.table
* lubridate
* readxl
* stringr
* car
* sandwich
* timeSeries
* tseries
* urca
* tidyr

## Status

This script is in the early stages of development.  The following key improvements are still required:
* Ensure that the data is complete for the period of investigation. This assurance includes:
  * No missing hours within the date-time period.
  * When data for an hour is missing, fill-in the missing data
* Incorporate weather variables into analysis.
* Generate solar generation model.

## License

The script is shared under the GNU GPLv3 license.


