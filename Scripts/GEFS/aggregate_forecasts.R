# This script aggregates all forecast files into a single format
library(here)
library(tidyverse)

#fix files downloaded from NOAA's s3 bucket
files <- list.files(here("Data/s3_2022_NOAA_GEFS_csv_files"),full.name = TRUE)
files_shortnames <- files|>
  str_remove("C:/Users/Caleb_Robbins/Documents/RC-hindcasts/Data/s3_2022_NOAA_GEFS_csv_files/")|>
  str_remove(" _ensembles_long.csv")|>
  str_remove("_ensembles_long.csv") #managed to have both with and without space in here...


#filter variables


walk2(.x = files, .y = files_shortnames, ~read_csv(.)|>pivot_wider(names_from = "variable", values_from = "prediction")|>
                  rename(air_pressure = "PRES",
                         air_temperature = "TMP", 
                         relative_humidity = "RH", 
                         eastward_wind = "UGRD",
                         northward_wind  = "VGRD", 
                         precipitation_flux = "APCP",
                         cloud_area_fraction = "TCDC",
                         surface_downwelling_shortwave_flux_in_air = "DSWRF",
                         surface_downwelling_longwave_flux_in_air = "DLWRF",
                         forecast_datetime_UTC = "datetime")|> 
                  mutate(hours_since_forecast_issue = as.numeric(str_remove(horizon, "^0+")),
                         forecast_issue_datetime = as_datetime(paste(reference_datetime, "00:00:00")),
                         ensemble = as.numeric(str_remove_all(ensemble,"[^0-9.-]"))+1,
                         wind_speed = sqrt(eastward_wind^2+northward_wind^2),
                         wind_direction = atan2(northward_wind,eastward_wind)*(180/pi))|> 
                  #atan2(y, x) returns the angle between the x-axis and the vector from the origin (which is on right side x axis) to (x,y)
                  select(-horizon,-cycle, -site_id,-reference_datetime)|>
                  write_csv(paste(here("Data/GEFS_data", paste0(.y, "_ensembles.csv"))))
)



# files downloaded from old NOMADS system into same format as the above files

files2 <- list.files(here("Data/2022_NOAA_GEFS_csv_files"),full.name = TRUE)
files2_shortnames <- files2|>
  str_remove("C:/Users/Caleb_Robbins/Documents/RC-hindcasts/Data/2022_NOAA_GEFS_csv_files/")|>
  str_remove("_ensembles.csv")

walk2(.x = files2, .y = files2_shortnames, ~read_csv(.)|>
                   filter(hours_since_forecast_issue %in% seq(3, 240, by =3))|>
                   mutate(wind_direction = atan2(northward_wind,eastward_wind)*(180/pi))|>
        write_csv(paste(here("Data/GEFS_Data", paste0(.y, "_ensembles.csv"))))
)
