#download 2022 NOAA forecasts for Richland Chambers reservoir

# Bulk of code to download archived forecasts from : https://github.com/eco4cast/gefs4cast
# which was authored by Carl Boettiger and R Quinn Thomas. Adapted here by Caleb Robbins from their "megacube_extract" function

#double checked this matches to previous downloads from NOMADS systems (so this script is gap-filling where those downloads failed)

library(tidyverse)
library(here)
library(tictoc)
library(furrr)
source(here("Scripts/GEFS/gefs-methods.R"))

sites_rc <- tibble(site_name = "Corsicana Airport", site_id = "rc", 
                    latitude = 32.027075451924304, longitude = -96.39709528628103)


my_sites <- function(crs = sf::st_crs(grib_wkt()) ) {
  sites <- sites_rc
  sf_sites <- sf::st_as_sf(sites,coords=c("longitude", "latitude"), #process coordinates as sf for gdal_cube::extract_geom()
                           crs = 4326) |>
    tibble::rowid_to_column("FID") |>
    sf::st_transform(crs = crs)
}

#map of the site for funsies
#texas <- tigris::states(cb = TRUE, resolution = "20m", class = "sf") %>%
#  filter(STUSPS == "TX")

#tm_shape(texas, is.main = TRUE, bbox = tmaptools::bb(texas, xlim = c(-107,-93), ylim = c(25.9,36.5)))+ 
#  tm_lines()+
#  tm_shape(sites)+
#  tm_symbols(col = "black", fill = "blue", fill_alpha = 0.2)

vars <- names(gefs_bands())
sites <- my_sites()
sites_df <- sites |>
  tibble::as_tibble() |>
  dplyr::select(dplyr::any_of(c("FID", "site_id")))

#fill in gaps from previous work using NOMADS system - gaps involved failures of either NOMADS or Caleb's poor automation skills...
f1 <- list.files(here("NearTerm Forecasts/s3_2022_NOAA_GEFS_csv_files"))|>str_remove("_ensembles_long.csv")
f2 <- list.files(here("NearTerm Forecasts/2022_NOAA_GEFS_csv_files"))|>str_remove("_ensembles.csv")
f_dates <- c(f1,f2)|>as_date()
desired_dates <- c(seq(ymd('2022-04-20'), ymd('2022-12-15'), by = '1 day'))

gefs_dates <- setdiff(desired_dates, f_dates)|>as_date()

  for(i in 1:length(gefs_dates)){   
    tryCatch({
    #set up
         reference_datetime <- gefs_dates[i]|>as_date()
        
          gribs <- #pare down the desired forecasts
            tidyr::expand_grid(ensemble = gefs_ensemble(),
                               cycle = "00",
                               #reference_datetime = Sys.Date() - 1L,
                               reference_datetime = reference_datetime,
                               horizon = gefs_horizon()) |>
            dplyr::mutate(url = gefs_urls(ensemble, reference_datetime,
                                          horizon, cycle=cycle),
                          time = as.Date(Sys.Date() + dplyr::row_number()))|>
            #slice_head(n = 5) #for testing
            filter(horizon <= 240) # 10 day forecasts - real control can be found in gefs-methods definition of GEFS_horizon()
          
          
          time <- cycle <- reference_datetime <- NULL


plan(multisession, workers = 12)
gdalcubes::gdalcubes_options(parallel = 1)
future_pmap(.options = furrr_options(seed = TRUE), list(gribs$url,gribs$time), ~gdalcubes::stack_cube(..1,
                                               datetime_values = ..2,
                                               band_names = gefs_all_bands())|>
                   gdalcubes::select_bands(gefs_bands()) |>
                   gdalcubes::extract_geom(my_sites()) |>
                   tibble::as_tibble())|>
    list_rbind()|>as_tibble()|>
  dplyr::rename({gefs_bands()}) |>
  # unpack overloaded time dimension:
  dplyr::left_join(dplyr::mutate(gribs, time=as.character(time)), by="time") |>
  dplyr::select(-"url") |> dplyr::select(-"time") |>
  dplyr::mutate(datetime = reference_datetime +
                  lubridate::hours(cycle) +
                  lubridate::hours(horizon)) |>
  # feature-id to site_id:
  dplyr::inner_join(sites_df, by = "FID") |>
  dplyr::select(-"FID") |>
  # "long" (EFI) format
  tidyr::pivot_longer(vars,
                      names_to = "variable",
                      values_to = "prediction") |>
  dplyr::ungroup()|>
  write_csv(paste(here("Data/s3_2022_NOAA_GEFS_csv_files", paste(gefs_dates[i], "_ensembles_long.csv"))))
 
print(paste0("Finished Date: ", gefs_dates[i],". Number ", i, "/", length(gefs_dates), " at ", Sys.time())) 
  }, error = function(e){cat("ERROR : ", conditionMessage(e), "\n")})
}


