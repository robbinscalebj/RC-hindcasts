
#This script creates the QC'd dataframe from the raw Richland Chambers profiler data

# Libraries
library(here)
library(tidyverse)
library(ids)

#Read-in raw data and apply QC steps - .csv file is locally replicated version of unaltered data pulled directly from the profiler
RC_profiler_data <- read_csv(here("Data/Observation_Data/RC_raw_df_8Dec2022.csv"))|>
  rename(Payout_m = `Payout`,Depth_m = `Depth M`, Temp_C = `Temp C`, SpCond_uS.cm = `Sp Cond uS/cm`, pH = `pH units`, DO_mg.L = `DO mg/L`, DO_per.sat = `DO %sat`, ORP_mv = `ORP mV`, Turbidity_NTU = `Turbidity NTU`, Chla = `Chloro A (V)`)%>%
  mutate(Datetime = Datetime - minutes(30))%>% #profiler time is 30 minutes off, for some reason
  mutate(Payout_m = round(Payout_m*2, digits = 0)/2, #rounds Payout to even number, not necessarily same as depth
         Payout_m = ifelse(Payout_m <0, 0, Payout_m),
         Payout_m = as_factor(Payout_m))%>% #factor simply to facilitate flagging to ID profiles in next step - numeric later
  mutate(Payout_m = as_factor(Payout_m), #factor simply to facilitate flagging to ID profiles in next step - numeric later
         zero_flags = ifelse(Payout_m == 0, TRUE, FALSE),#unifies each profile by counting each 0 m measure (start of every profile), and giving unique ID
         sumzero_flags = cumsum(zero_flags),
         sumzero_flags = as_factor(sumzero_flags),
         Payout_m = as.numeric(as.character(Payout_m)))%>%
  # mutate(Payout_m = ifelse(Datetime > as_datetime("2022-04-04 23:30:45") & Datetime < as_datetime("2022-04-07 15:38:11"), 
  #                       ifelse(Payout_m == lag(Payout_m), Payout_m+0.5, Payout_m), Payout_m))%>%
  group_by(sumzero_flags)%>%
  mutate(profileId = as_factor(uuid()))%>%
  #mutate(Payout_m = as.numeric(as.character(Payout_m)))%>%
  ungroup()%>%
  select(-zero_flags, -sumzero_flags)%>%
  mutate(raw_index_row = row_number(),
         reduced_index_row = raw_index_row-141750)|> #useful for modeling time-dependent drift - as.numeric on Datetimes inconsistent
  mutate(pH = ifelse(pH <6.0 | pH >11.0, NA, pH),
         SpCond_uS.cm = ifelse(SpCond_uS.cm <200, NA, SpCond_uS.cm),
         Temp_C = ifelse(Temp_C < 0.000, NA, Temp_C), 
         Payout_m = ifelse(Payout_m >=11, NA, Payout_m),
         Turbidity_NTU = ifelse(Turbidity_NTU<0.1 | Turbidity_NTU>60, NA, Turbidity_NTU))%>%
  filter(Temp_C > 0.0)|>
  # Data QC specific to particular conditions and data ranges
  ## Time-dependent drift
  mutate(pH = if_else(Datetime >=as_datetime("2022-08-25 16:00:00") & Datetime < as_datetime("2022-09-13 12:00:00"), 
                      7.72+pH-(5.67236+0.39531*log(reduced_index_row)), #R2 = 0.86, logarithmic function baseline correction
                      pH),
         pH = if_else(Datetime >=as_datetime("2022-09-22 15:00:00") & Datetime < as_datetime("2022-09-29 19:00:00"), 
                      7.72+pH-(-71.54+5.327e-04*raw_index_row), #R2 = 0.95, linear function baseline correction
                      pH),
         pH = if_else(Datetime >=as_datetime("2022-10-20 15:30:00") & Datetime < as_datetime("2022-11-10 17:00:00"), 
                      7.75+pH-(8.438441+0.156381*log(reduced_index_row-13805)), #R2 = 0.58, log function
                      pH))|>
  ## Universal problems problematic date ranges for observation omission
  filter(!(Datetime >= as_datetime("2021-07-12 02:00:00") & Datetime <= as_datetime("2021-07-14 16:00:00")),
         !(Datetime >= as_datetime("2021-08-19 18:00:00") & Datetime <= as_datetime("2021-08-25 12:00:00")),
         !(Datetime >= as_datetime("2021-10-26 00:00:00") & Datetime <= as_datetime("2021-11-02 10:00:00")),
         !(Datetime >= as_datetime("2022-08-07 12:00:00") & Datetime <= as_datetime("2022-08-18 23:50:00")))%>%
  ## Depth corrections
  filter(ifelse(Datetime >=as_datetime("2022-04-04 23:30:45") & Datetime<=as_datetime("2022-04-07 15:38:11"), Payout_m>2.1, TRUE))%>%
  mutate(Payout_m = ifelse(Datetime > as_datetime("2022-04-04 23:30:45") & Datetime < as_datetime("2022-04-07 15:38:11"),Depth_m, Payout_m))%>%
  # Payout unreliable 2022-07-28 to 2022-08-07, but depth correct after correcting for difference due to uncalilbrated pressure
  # then remove observations where Payouts below 10 because these were possibly in the sediment and also inconcistent with 10 being max depth
  mutate(Payout_m = ifelse(Datetime > as_datetime("2022-07-28 16:00:00") & Datetime < as_datetime("2022-08-07 12:00:00"),
                           round(2*Depth_m-2, digits = 0)/2, Payout_m))%>%
  filter(!(Datetime > as_datetime("2022-07-28 16:00:00") & Datetime < as_datetime("2022-08-07 23:00:00") & Payout_m >10.2),
         !(Datetime > as_datetime("2022-07-25 16:00:00") & Datetime < as_datetime("2022-08-07 23:00:00") & SpCond_uS.cm <252))%>%
  ## Probe-specific issues
  mutate(pH = ifelse(Datetime >=as_datetime("2021-09-26 18:00:00") & Datetime<=as_datetime("2021-10-15 10:00:00"), NA, pH),
         pH = ifelse(Datetime >=as_datetime("2021-09-26 18:00:00") & Datetime<=as_datetime("2021-10-15 10:00:00"), NA, pH),
         pH = ifelse(Datetime >=as_datetime("2022-01-27 15:00:00") & Datetime<=as_datetime("2022-02-17 17:00:00"), NA, pH),
         pH = ifelse(Datetime >=as_datetime("2022-04-07 17:30:53") & Datetime<=as_datetime("2022-04-14 14:08:18"), NA, pH),
         pH = ifelse(Datetime >=as_datetime("2022-07-14 15:30:31") & Datetime<=as_datetime("2022-07-21 19:30:54"), pH+0.275, pH),
         pH = ifelse(Datetime >=as_datetime("2022-07-19 15:30:00") & Datetime<=as_datetime("2022-07-28 14:08:32"), NA, pH),
         pH = ifelse(Datetime >=as_datetime("2022-09-10 12:00:00") & Datetime<=as_datetime("2022-09-13 16:00:00"), NA, pH),
         pH = ifelse(Datetime == as_datetime("2022-09-24 01:30:43"), NA, pH))%>%
  mutate(DO_per.sat = ifelse(Datetime >=as_datetime("2022-07-14 15:30:31") & Datetime<=as_datetime("2022-07-28 14:08:32"), NA, DO_per.sat),
         ##
         Depth_m = ifelse(Datetime >as_datetime("2022-04-07 17:30:53") & Datetime<=as_datetime("2022-04-14 14:08:18"), NA, Depth_m),
         ##
         ORP_mv = ifelse(Datetime >=as_datetime("2022-03-31 17:30:53") & Datetime<=as_datetime("2022-03-31 18:08:24"), NA, ORP_mv),
         ORP_mv = ifelse(Datetime >=as_datetime("2022-03-19 09:30:44") & Datetime<=as_datetime("2022-03-19 10:07:55"), NA, ORP_mv),
         ORP_mv = ifelse(Datetime >=as_datetime("2022-07-19 15:30:00") & Datetime<=as_datetime("2022-07-28 14:08:32"), NA, ORP_mv),
         ##
         SpCond_uS.cm = ifelse(Datetime >=as_datetime("2021-08-04 16:00:00") & Datetime<=as_datetime("2021-08-13 10:00:00"), NA, SpCond_uS.cm),
         SpCond_uS.cm = ifelse(Datetime >=as_datetime("2022-07-01 06:00:00") & Datetime<=as_datetime("2022-07-15 00:00:00") & SpCond_uS.cm<260, NA, SpCond_uS.cm),
         SpCond_uS.cm = ifelse(Datetime > as_date("2022-04-01") & Datetime < as_date("2022-04-15") & SpCond_uS.cm <310, NA, SpCond_uS.cm),
         SpCond_uS.cm = ifelse(Datetime > as_date("2022-09-29") & Datetime < as_date("2022-10-03") & SpCond_uS.cm <265, NA, SpCond_uS.cm))|>
  #filter(ifelse(Datetime >=as_datetime("2022-04-04 23:30:45") & Datetime<=as_datetime("2022-04-07 15:38:11"), is.na(Depth_m)|Depth_m>0, TRUE))%>%    
  relocate(Datetime)

# write to file

RC_profiler_data|>
  filter(Datetime >=as_datetime("2021-04-15 01:00:00"))|> #data considered acceptable hereafter
  mutate(Datetime_UTC = as.character(Datetime))|>
  select(Datetime_UTC,Payout_m,Temp_C,SpCond_uS.cm,pH,DO_mg.L, DO_per.sat, profileId)|>
  write_csv(here("Data/Observation_Data/RC_profiler_tidied_df.csv"))
