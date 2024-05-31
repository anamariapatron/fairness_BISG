setwd("/Users/agathefernandesmachado/Documents/PhD/predicting_race/cloud")
source("metrics.r")
library(dplyr)
library(readr)
library(wru)

# Race by geolocation
try_data <- get_census_data(
  key ="6dd6f38d30161dff47997298310b313880989514",
  states = c("NJ", "NY"), 
  age = FALSE, 
  sex = FALSE)

str(try_data[[1]])

# Surname by race
load("../surnames2000.RData")