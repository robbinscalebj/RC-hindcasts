# This script trains a random forest model randfor_temp_1lag, which incorporates an initial state in the form of a predictor that is the previously observed temperature on a three hour lag (forecast data) 
# no lagged predictors, and assumes all the observations are independent

# This model is meant to predict only the next state based on the previous state and current weather covariates

library(here)
library(tidyverse)
library(tidymodels)
library(doParallel)
library(butcher)
library(bundle)
library(arrow)
library(vip)

model_name <- "randfor_temp_1lag"


# Define training data - all 2021 data with profiler observations of temperature, DO, and observed meteorological data
# must create lagged water temp variable (where possible - some obs don't have previous info)
RC_df <- read_parquet(here("Data/Observation_Data/profiler-meteo-tidied_df-2021-lagged.parquet"))|>
  select(-contains("DO_mg.L"), -contains("pH"), -contains("SpCond_uS.cm"))|>
  select(Datetime_UTC,Depth_m, doy, hour_of_day, WaterTemp_C, contains("lag6hr"))|>
  drop_na() #package for random forest plays like garbage with NAs


# Create recipes
training_recipe <- recipe(WaterTemp_C ~ ., data = RC_df)|> #R formula syntax says use Temp as response, all other vars as predictors
  update_role(Datetime_UTC, new_role = "ID")|> #keep datetime in as a sort of row ID, but don't use as a predictor
  step_normalize(all_numeric_predictors(), -all_outcomes()) #normalize the predictors - helps random forest work better

check_recipe <- training_recipe|>prep()|>juice()


#set engine
randfor_mod <- rand_forest(
  mtry = tune(), #
  trees = 500, #TRAINING basically just set high enough - following James, Witten, Hastie, Tibshirani An Introduction to Statistical Learning
  min_n = tune()) |> #
  set_mode("regression") |> #as opposed to classification
  set_engine("ranger", importance = "impurity") #impurity gives us variable importance scores after the final model selected

# set workflow

mod_workflow <- workflow()|>
  add_model(randfor_mod)|>
  add_recipe(training_recipe)


#tune workflow

cl <- makePSOCKcluster(4)
doParallel::registerDoParallel(cl)

mod_tune_fits <- mod_workflow|>
  tune_grid(resamples = vfold_cv(RC_df, 
                                 v = 10, repeats = 5, # 10-fold cross validation, repeated 5 times 
                                 strata = Depth_m), #resampling proportional coverage of different depths
            grid = 20)


#select mod_tune_fits tuning parameter and final model fit
mod_tune_fits |> 
  collect_metrics()|>
  filter(.metric == "rmse")|>
  arrange(mean)

best_params <- mod_tune_fits|>
  select_best("rmse")


final_fit <- mod_workflow|>
  finalize_workflow(best_params)|>
  fit(data = RC_df) #final fit to training data

#convert model fit to minimal form
res_bundle <-
  final_fit %>%            
  butcher(verbose = TRUE) %>% 
  bundle()

# generate variable importances
mod_vip <- final_fit|>extract_fit_parsnip()|>vip()|>pluck("data")

#calculate final rmse on whole training data by rmse
final_preds <- predict(final_fit, RC_df)|>
  bind_cols(RC_df)

final_rmse<-final_preds|>group_by(Depth_m)|>rmse(estimate = .pred, truth = WaterTemp_C)



#write fitted model and stats to file

saveRDS(res_bundle, here(paste0("Data/Models/Fitted_Models/",model_name,".Rds")))

mod_vip|>write_csv(here(paste0("Data/Models/Model_Stats/vip_",model_name,".csv")))
final_rmse|>select(Depth_m,.estimate)|>write_csv(here(paste0("Data/Models/Model_Stats/rmse_",model_name,".csv")))