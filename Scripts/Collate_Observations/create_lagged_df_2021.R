# This script creates a full lagged dataframe for the Richland-Chambers 2021 data

library(here)
library(tidyverse)


RC_df <- read_csv(here("Data/Observation_Data/profiler-meteo-tidied_df-2021.csv"))

full_time_grid <- expand_grid(Datetime_UTC = seq(first(RC_df$Datetime_UTC), last(RC_df$Datetime_UTC), by = "2 hours"),
                              Depth_m  = seq(0,10, by = 0.5))

RC_full_ts <- left_join(full_time_grid, RC_df)

RC_lagged <- RC_full_ts|>
  group_by(Depth_m)|>
  arrange(Datetime_UTC)|>
  mutate(across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C, Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 3L), .names = "{.col}_lag6hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 6L), .names = "{.col}_lag12hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 9L), .names = "{.col}_lag18hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 12L), .names = "{.col}_lag24hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 15L), .names = "{.col}_lag30hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 18L), .names = "{.col}_lag36hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 21L), .names = "{.col}_lag42hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 24L), .names = "{.col}_lag48hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 27L), .names = "{.col}_lag54hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 30L), .names = "{.col}_lag60hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 33L), .names = "{.col}_lag66hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 36L), .names = "{.col}_lag72hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 39L), .names = "{.col}_lag78hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 42L), .names = "{.col}_lag84hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 45L), .names = "{.col}_lag90hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 48L), .names = "{.col}_lag96hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 51L), .names = "{.col}_lag102hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 54L), .names = "{.col}_lag108hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 57L), .names = "{.col}_lag114hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 60L), .names = "{.col}_lag120hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 63L), .names = "{.col}_lag126hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 66L), .names = "{.col}_lag132hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 69L), .names = "{.col}_lag138hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 72L), .names = "{.col}_lag144hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 75L), .names = "{.col}_lag150hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 78L), .names = "{.col}_lag156hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 81L), .names = "{.col}_lag162hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 84L), .names = "{.col}_lag168hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 87L), .names = "{.col}_lag174hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 90L), .names = "{.col}_lag180hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 93L), .names = "{.col}_lag186hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 96L), .names = "{.col}_lag192hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 99L), .names = "{.col}_lag198hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 102L), .names = "{.col}_lag204hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 105L), .names = "{.col}_lag210hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 108L), .names = "{.col}_lag216hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 111L), .names = "{.col}_lag222hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 114L), .names = "{.col}_lag228hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 117L), .names = "{.col}_lag234hr"),
         across(c(DO_mg.L, pH, SpCond_uS.cm, WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 120L), .names = "{.col}_lag240hr"))



RC_lagged|>write_csv(here("Data/Observation_Data/profiler-meteo-tidied_df-2021-lagged.csv"))
