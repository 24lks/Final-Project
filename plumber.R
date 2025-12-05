#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(tidymodels)
library(tidyverse)
library(yardstick)
library(ggplot2)
library(dplyr)

#Read in the data
diabetes <- read_csv("diabetes_binary_5050split_health_indicators_BRFSS2015.csv")
rf_final_model <- readRDS("rf_final_model.rds")



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


# Function to return default value for one variable

get_default <- function(vec) {
  if (is.numeric(vec)) {
    return(mean(vec, na.rm = TRUE))
  } else {
    return(names(sort(table(vec), decreasing = TRUE))[1])
  }
}

# Create a named list of defaults for the predictors your RF model uses
predictor_defaults <- diabetes |> 
  select(BPF, CholF, PhysF, FruitsF, VeggiesF, AlcF, SexF, BMI, MentHlth) |> 
  summarise(across(everything(), get_default)) |> 
  as.list()


#* @apiTitle Diabetes Prediction API
#* @apiDescription Creating endpoints for prediction, info, and confusion matrix.


#* Prediction Endpoint-
#* 
#* @param BPF
#* @param CholF
#* @param PhysF
#* @param FruitsF
#* @param VeggiesF
#* @param AlcF
#* @param SexF
#* @param BMI
#* @param MentHlth
#* @get /pred 
function(
BPF = predictor_defaults$BPF,
CholF = predictor_defaults$CholF,
PhysF = predictor_defaults$PhysF,
FruitsF = predictor_defaults$FruitsF,
VeggiesF = predictor_defaults$VeggiesF,
AlcF = predictor_defaults$AlcF,
SexF = predictor_defaults$SexF,
BMI = predictor_defaults$BMI,
MentHlth = predictor_defaults$MentHlth
) {
  newdata <- tibble(
    BPF = factor(BPF, levels = levels(diabetes$BPF)),
    CholF = factor(CholF, levels = levels(diabetes$CholF)),
    PhysF = factor(PhysF, levels = levels(diabetes$PhysF)),
    FruitsF = factor(FruitsF, levels = levels(diabetes$FruitsF)),
    VeggiesF = factor(VeggiesF, levels = levels(diabetes$VeggiesF)),
    AlcF = factor(AlcF, levels = levels(diabetes$AlcF)),
    SexF = factor(SexF, levels = levels(diabetes$SexF)),
    BMI = as.numeric(BMI),
    MentHlth = as.numeric(MentHlth)
  )
  predict(rf_final_model, new_data = newdata, type = "prob")
}

# Example calls:
# http://127.0.0.1:31307/pred?BPF=Yes&CholF=No&PhysF=Yes&FruitsF=No&VeggiesF=Yes&AlcF=No&SexF=Female&BMI=30&MentHlth=5
# http://127.0.0.1:31307/pred?BPF=No&CholF=Yes&PhysF=No&FruitsF=Yes&VeggiesF=Yes&AlcF=Yes&SexF=Male&BMI=25&MentHlth=2
# http://127.0.0.1:31307/pred?BMI=22&MentHlth=0




#* API Info Endpoint-
#* @get /info
function () {
list(
  name="Laurie Short",
rendered_github_pages_url="https://24lks.github.io/Final-Project/",
message = "This API provides predictions from my random forest diabetes model."

)
}

#* Confusion matrix plot Endpoint
#* @get /confusion
#* @png
function() {
  preds <- predict(rf_final_model, new_data = diabetes, type = "class")
  preds <- factor(preds, levels = levels(diabetes$DiabetesF))
  
  cm_data <- tibble(
    truth = diabetes$DiabetesF,
    estimate = preds
  )
  
  cm <- conf_mat(cm_data, truth = {{truth}}, estimate = {{estimate}})
  
  # This line must be last
  print(autoplot(cm))
}
