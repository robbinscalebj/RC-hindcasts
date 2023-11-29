#script takes forecasts and models them with random forest
library(here)
library(tidyverse)
library(tidymodels)
# Load model

#randfor_fit <- read_rds

# Load historical data - make sure this includes ALL historical data, even data random forest wasn't fit on.
rc_observations <- read_csv(here("RC_profiler_postQC_df_8Dec2022.csv"))|>
  dplyr::select(Datetime_UTC, Payout_m, DO_mg.L)|>
  mutate(Datetime = round_date(Datetime_UTC, "hour"))|>
  rename(Depth_m = "Payout_m")

# Forecasting workflow

## define loop
forecast_dataframes <- list.files(here("NearTerm Forecasts/2022_NOAA_GEFS_csv_files"))


forecast_function <- function(forecast_df){
  # Tidy forecast
  df <- read_csv(here(paste0("NearTerm Forecasts/2022_NOAA_GEFS_csv_files/", forecast_df)))|>
    mutate(AirTemp_C = air_temperature - 273.15,
           Cloud_cov_fraction = cloud_area_fraction/100, #note cloud_area_fraction is not in oktas, like the ISD data
           WWind_m.s = eastward_wind, #eastward = West wind
           SWind_m.s = northward_wind, #northward = South wind
           DewPoint_C = AirTemp_C - ((100-relative_humidity)/0.5),
           Pressure_hPa = air_pressure/100,
           doy = yday(forecast_datetime_UTC),
           hour_of_day = hour(as_hms(forecast_datetime_UTC)))|>
    dplyr::select(-contains("downwelling"),-precipitation_flux, -contains("ward_wind"), -relative_humidity,
           -cloud_area_fraction, -air_pressure, -air_temperature,-wind_speed)|>
    #filter(ensemble %in% c("00", "01"))|>
    mutate(Datetime = forecast_datetime_UTC)|>
    expand_grid(Depth_m = seq(0, 10, by = 0.5))|> # Expands so weather data exist at each depth
    drop_na()
  
  forecast_start <- min(df$Datetime)-days(10) 
  forecast_end <- max(df$Datetime)
  forecast_issue_date <- str_remove(forecast_df, "_ensembles.csv")

  # Make DO predictions
  predictions <- randfor_fit |> 
    predict(df)|>
    bind_cols(df)|>#|>select(.pred,Depth_m, Datetime)
    left_join(rc_observations|>filter(Datetime_UTC >= forecast_start & Datetime_UTC <= forecast_end),
              by = c("Datetime", "Depth_m"))|>
    filter(!is.na(.pred))|> #gaps because model output is hourly, observations are two hourly
    mutate(forecast_issue_date = str_remove(forecast_df, "_ensembles.csv"))
  
  write_csv(predictions, here(paste0("NearTerm Forecasts/randfor_forecast_predictions/", forecast_issue_date, ".csv")))
}

walk(forecast_dataframes, ~forecast_function(.))




