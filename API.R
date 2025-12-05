library(plumber)
library(tidymodels)
library(tidyverse)

#Read in the data

diabetes <- read_csv("diabetes_binary_5050split_health_indicators_BRFSS2015.csv")
diabetes<-as.tibble(diabetes)

#convert variables like I did at the beginning

#First selecting only the vatriables we want to look at as described in introduction in EDA file
diabetes<-diabetes |>
  select(Diabetes_binary, HighBP, HighChol, BMI, PhysActivity, Fruits, Veggies, HvyAlcoholConsump, MentHlth, Sex)


#Currently all of all variables are numeric, but it will make more sense to have several of them as factor variables. Next we will create factor versions of all variables except BMI and Mental Helath

diabetes <- diabetes |>
  mutate(
    DiabetesF = factor(
      Diabetes_binary,
      levels = c(0, 1),
      labels = c("No diabetes", "Prediabetes/Diabetes")
    ),
    BPF = factor(
      HighBP,
      levels = c(0, 1),
      labels = c("No high BP", "High BP")
    ),
    CholF = factor(
      HighChol,
      levels = c(0, 1),
      labels = c("No high cholesterol", "High cholesterol")
    ),
    PhysF = factor(
      PhysActivity,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    FruitsF = factor(
      Fruits,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    VeggiesF = factor(
      Veggies,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    AlcF = factor(
      HvyAlcoholConsump,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    SexF = factor(
      Sex,
      levels = c(0, 1),
      labels = c("Female", "Male")
    )
  )

diabetes<-diabetes |>
  select(DiabetesF, BPF, CholF, PhysF, FruitsF, VeggiesF, AlcF, SexF, BMI, MentHlth)

#let's fit the best model to the entire data set! In the modeling.qmd file we found that the best model was the random foreset model, so it is fit below


# Recipe
RF_rec <- recipe(DiabetesF ~ BPF + CholF + PhysF + FruitsF + VeggiesF + AlcF + SexF + BMI + MentHlth,
                 data = diabetes_train) |>
  step_normalize(all_numeric()) |>
  step_dummy(BPF, CholF, PhysF, FruitsF, VeggiesF, AlcF, SexF)
RF_rec |>
  prep(diabetes_train) |>
  bake(diabetes_train) 

#Model

rf_mod <- rand_forest(mtry= tune()) |>
  set_engine("ranger") |>
  set_mode("classification")

#workflow

rf_wkf <- workflow() |>
  add_recipe(RF_rec) |>
  add_model(rf_mod)

#fit to our CV folds

rf_fit <- rf_wkf |>
  tune_grid(resamples = diabetes_5_fold,
            grid = grid_regular(mtry(range = c(2, 9)), levels = 7),
            metrics = metric_set(accuracy, mn_log_loss))

#Check our metrics across the folds

rf_fit |>
  collect_metrics() |>
  filter(.metric == "mn_log_loss") |>
  arrange(mean)

# Get our best tuning parameter

rf_best_params <- select_best(rf_fit, metric = "mn_log_loss")
rf_best_params

#Refit on the entire training set using this tuning parameter

rf_final_wkf <- rf_wkf |>
  finalize_workflow(rf_best_params)
rf_final_fit <- rf_final_wkf |>
  last_fit(diabetes_split, metrics = metric_set(accuracy, mn_log_loss))

collect_metrics(rf_final_fit)


#Refit on the data

rf_final_model <- rf_final_wkf |>
  fit(diabetes)

