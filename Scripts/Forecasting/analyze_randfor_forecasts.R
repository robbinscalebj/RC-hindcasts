library(score4cast)
library(tidyverse)
library(here)
library(lubridate)

#this file will calculate metrics on forecasts

predictions_files<-list.files(here("NearTerm Forecasts/randfor_forecast_predictions/"))

score_forecasts <- function(predictions_files){
  
fc_df <- read_csv(here(paste0("NearTerm Forecasts/randfor_forecast_predictions/",predictions_files)))

fc <- fc_df |>
  filter(!is.na(DO_mg.L))|>
  rename(reference_datetime = "forecast_issue_datetime", prediction = ".pred",site_id = "Depth_m")|>
  mutate(datetime = reference_datetime + hours(hours_since_forecast_issue))|>
  mutate(family = "sample", parameter = as.numeric(ensemble)+1, variable = "oxygen")

obs <- fc|>
  dplyr::select(DO_mg.L, datetime,site_id, variable, reference_datetime)|>
  rename(observed = "DO_mg.L")

scores <- score(fc,obs)

write_csv(scores, here(paste0("NearTerm Forecasts/randfor_forecasts_scored/",predictions_files)))
}

walk(predictions_files, ~score_forecasts(.))


#test
blah2 <- read_csv(here("NearTerm Forecasts/randfor_forecasts_scored/2022-07-01.csv"))


blah2|>
  filter(site_id == 10)|>
  ggplot()+
    geom_point(aes(x = datetime, y = observation), color = "red")+
    geom_line(aes(x = datetime, y = mean), color = "black", size = 0.75)+
    geom_line(aes(x = datetime, y = quantile02.5), color = "blue", linetype = "dashed", linewidth = 0.5)+
    geom_line(aes(x = datetime, y = quantile97.5), color = "blue", linetype = "dashed", linewidth = 0.5)+
  scale_x_datetime(breaks = "3 day")+
  ylab("Dissolved Oxygen (mg/L)")+
  theme_bw()+
  theme(axis.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.title.x = element_blank())
 
mean_blah <- blah2|>
  mutate(date = as_date(datetime))|>
  group_by(date, site_id)|>
  summarize(mean_daily_crps = mean(crps))

mean_blah|>
  filter(site_id == 0.0)|>
  ggplot()+
    geom_point(aes(x = date, y = mean_daily_crps))
  
    

generic_crps <- function(family, parameter, prediction, observation){
  names(prediction) = parameter
  y <- dplyr::first(observation)
  tryCatch(
    switch(unique(as.character(family)),
           lognormal = scoringRules::crps_lnorm(y, prediction['mu'], prediction['sigma']),
           normal = scoringRules::crps_norm(y, prediction['mu'], prediction['sigma']),
           sample = scoringRules::crps_sample(y, prediction)
    ),
    error = function(e) NA_real_, finally = NA_real_)
}

predictions <- map_dfr(forecast_dataframes, ~forecast_function(.))
#evals_by_depth <- predictions|>
#  group_by(Depth_m)|>
#  summarize(mean_pred = mean(.pred, na.rm = TRUE),
#            sd_pred = sd(.pred, na.rm = TRUE))

#write raw predictions to file

# rmse
# crps
#forecast reliability
}



ex_data <- system.file("extdata/standard-format-examples.R", package="score4cast")
source(ex_data)

scores <- score(ex_forecast, ex_target)
scores
