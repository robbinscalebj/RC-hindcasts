# This particular model is a simple random forest that has no lags in predictors - i.e., it is not explicitly a time series analysis
# Train random forest model hyperparameters on 2021 data

library(tidyverse)
library(tidymodels)
library(lubridate)
library(doParallel)
library(hms)
library(here)




#Define forecast data
RC_forecast <- read_csv(here("NearTerm Forecasts/2022_NOAA_GEFS_csv_files/2022-04-30_ensembles.csv"))|>
  mutate(AirTemp_C = air_temperature - 273.15,
         Cloud_cov_fraction = cloud_area_fraction/100, #note cloud_area_fraction is not in oktas, like the ISD data
         WWind_m.s = eastward_wind, #eastward = West wind
         SWind_m.s = northward_wind, #northward = South wind
         DewPoint_C = AirTemp_C - ((100-relative_humidity)/0.5),
         Pressure_hPa = air_pressure/100,
         doy = yday(forecast_datetime_UTC),
         hour_of_day = hour(as_hms(forecast_datetime_UTC)))|>
  select(-contains("downwelling"),-precipitation_flux, -contains("ward_wind"), -relative_humidity,
         -cloud_area_fraction, -air_pressure, -air_temperature,-wind_speed)|>
  #filter(ensemble %in% c("00", "01"))|>
  mutate(Datetime = forecast_datetime_UTC)|>
  expand_grid(Depth_m = seq(0, 10, by = 0.5))|> # Expands so weather data exist at each depth
  drop_na()

forecast_start <- first(RC_forecast$forecast_datetime_UTC) 

# Define training data - all 2021 data
RC_df <- read_csv(here("Predictive_Models/RC_pred_alldata.csv"))|>
  select(-val_period, -WaterTemp_C)|>
  filter(!is.na(DO_mg.L))|>
  drop_na()|> #package for random forest plays like garbage with NAs
  filter(Datetime < as_datetime(forecast_start)) #not actually a problem here because using a reduced data set from 2021 only



# Create recipes
randfor_recipe <- recipe(DO_mg.L ~ ., data = RC_df)|> #r formula syntax says use DO_mg.L as response, all other vars as predictors
  update_role(Datetime, new_role = "ID")|> #keep datetime in as a sort of row ID, but don't use as a predictor
  step_normalize(all_numeric_predictors(), -all_outcomes())
  
check_recipe <- randfor_recipe|>prep()|>juice()

#set engine
randfor_mod <- rand_forest(
  mtry = tune(), #
  trees = 500, # basically just set high enough - following James, Witten, Hastie, Tibshirani An Introduction to Statistical Learning
  min_n = tune()) |> #
  set_mode("regression") |> #as oppposed to classification
  set_engine("ranger", importance = "impurity") #impurity gives us variable importance scores after the final model selected
  
# set workflow

randfor_workflow <- workflow()|>
  add_model(randfor_mod)|>
  add_recipe(randfor_recipe)


#tune workflow

cl <- makePSOCKcluster(5)
doParallel::registerDoParallel(cl)

randfor_tune_fits <- randfor_workflow|>
  tune_grid(resamples = vfold_cv(RC_df, 
                                 v = 10, repeats = 5, #10-fold cross validation, repeated 5 times 
                                 strata = Depth_m), #resampling proportional coverage of different depths
            grid = 20) #creates 20 candidate parameter sets of tuning parameters


#select best tuning parameter and final model fit
randfor_tune_fits |> 
  collect_metrics()|>
  filter(.metric == "rmse")|>
  arrange(mean)

best_params <- randfor_tune_fits|>
  select_best("rmse")
  

randfor_fit <- randfor_workflow|>
  finalize_workflow(best_params)|>
  fit(data = RC_df) #final fit to training data

#Save model



extract_fit_parsnip(randfor_fit)|>vip(num_features = 20)
#fit model to new data
predictions <- randfor_fit |> 
  predict(RC_forecast)|>
  bind_cols(RC_forecast)

blah <- read_csv(here("RC_noaa_isd_corsicana.csv"))



#visualize predictions

predictions|>
  bind_rows(RC_df)|>
  filter(Depth_m %in% c(0))|>
  ggplot()+
  geom_line(aes(y = .pred, x = Datetime, group = ensemble))+
  #facet_grid(.~Depth_m)+
  scale_x_datetime(limits = c(forecast_start, last(predictions$Datetime)))

pred_sums<- predictions|>
  group_by(Datetime, Depth_m)|>
  summarize(mean_do = mean(.pred),
            ci = 1.96*sqrt(sd(.pred)/n()),
            pi = 1.96*sqrt(sd(.pred)))
  
  
predictions|>
  bind_rows(RC_df)|>
  filter(Depth_m %in% c(0))|>
  ggplot()+
  geom_line(aes(y = .pred, x = Datetime, group = ensemble))+
  #facet_grid(.~Depth_m)+
  scale_x_datetime(limits = c(forecast_start, last(predictions$Datetime)-days(10)))+
  scale_y_continuous(limits = c(7.5,10))


pred_sums|>
  filter(Depth_m == 0)|>
  ggplot()+
  geom_line(aes(y = mean_do, x = Datetime))+
  #geom_line(aes(y = mean_do+ci, x = Datetime), color = "red", linetype= "dashed")+
  #geom_line(aes(y = mean_do-ci, x = Datetime), color = "red", linetype= "dashed")+
  geom_line(aes(y = mean_do+pi, x = Datetime), color = "blue", linetype= "dashed")+
  geom_line(aes(y = mean_do-pi, x = Datetime), color = "blue", linetype= "dashed")+
  #facet_grid(.~Depth_m)+
  scale_x_datetime(limits = c(forecast_start, last(predictions$Datetime)-days(5)))+
  theme_bw()+
  scale_y_continuous(limits = c(7,11))+
  ylab("DO (mg.L)")+
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 14))






