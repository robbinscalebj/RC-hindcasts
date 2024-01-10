#This script 
# 1) Combines the Richland-Chambers "short-term" (2021-2022) data sources in proper formatting to be used for predictive modeling. 
# 2) Splits the 2021 data into training and validation periods
# 2a) this must respect the stratification of sampling efforts - Depth, important time periods

library(tidyverse)
library(hms)
library(here)

#read meteorological data
cors_isd <- read_csv(here("Data/Observation_Data/noaa_isd_corsicana.csv"))|>
  rename(Datetime = "Datetime_UTC")

#read profiler data
profiler_dat <- read_csv(here("Data/Observation_Data/RC_profiler_tidied_df.csv"))|>
  rename(datetime_profiler_utc = "Datetime_UTC")|>
  mutate(Datetime = round_date(datetime_profiler_utc, "hour"))|> #datetime is rounded to nearest hour to match up with isd data
  #join isd data 
  left_join(cors_isd)|>
  relocate(Datetime)|>
  group_by(profileId)|> #ensure whole profiles still stick together
  mutate(doy = yday(first(Datetime)),
         hour_of_day = hour(Datetime))|>
  relocate(doy,hour_of_day)|>
  rename(Depth_m = "Payout_m", WaterTemp_C = "Temp_C", Datetime_UTC = "Datetime")|>
  ungroup()|>
  select(-datetime_profiler_utc, -WindSpeed_m.s, -WindDirection_deg.clwise, -RelHum_per, -DO_per.sat)


profiler_dat|>write_csv(here("Data/Observation_Data/profiler-meteo-tidied_df.csv"))
profiler_dat|>filter(Datetime_UTC<as_date("2022-01-01"))|>write_csv(here("Data/Observation_Data/profiler-meteo-tidied_df-2021.csv"))
