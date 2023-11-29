#This script 
# 1) Combines the Richland-Chambers "short-term" (2021-2022) data sources in proper formatting to be used for predictive modeling. 
# 2) Splits the 2021 data into training and validation periods
# 2a) this must respect the stratification of sampling efforts - Depth, important time periods

library(tidyverse)
library(hms)
library(here)

#read meteorological data
cors_isd <- read_csv(here("Data/observation_data/noaa_isd_corsicana.csv"))|>
  rename(Datetime = "Datetime_UTC")

#read profiler data
profiler_dat <- read_csv(here("Data/observation_data/RC_profiler_tidied_df.csv"))|>
  rename(datetime_profiler_utc = "Datetime_UTC")|>
  mutate(Datetime = round_date(datetime_profiler_utc, "hour"))|> #datetime is rounded to nearest hour to match up with isd data
  #join isd data 
  left_join(cors_isd)|>
  relocate(Datetime)|>
  group_by(profileId)|> #ensure whole profiles still stick together
  mutate(doy = yday(first(Datetime)),
         time_of_day = as_hms(Datetime))|>
  relocate(doy,time_of_day)|>
  rename(Depth_m = "Payout_m", WaterTemp_C = "Temp_C")|>
  ungroup()|>
  select(-datetime_profiler_utc, -WindSpeed_m.s, -WindDirection_deg.clwise, -RelHum_per, -pH, -SpCond_uS.cm, -DO_per.sat, -profileId)


profiler_dat|>write_csv(here("Data/observation_data/profiler-meteo-tidied_df.csv"))
