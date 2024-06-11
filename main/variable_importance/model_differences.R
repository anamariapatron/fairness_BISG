# Import packages
library(dplyr)
library(readr)
library(wru)
library(stringr)
library(caret)
library(purrr)
library(pROC)

# Load datasets for which race has been predicted
# Using surname only
pred_surname <- read.csv("output/results/BIS_surname.csv")
pred_surname_tract <- read.csv2("output/results/BISG_surname_tract.csv")
pred_surname_first_tract <- read.csv2("output/results/BIFSG_surname_first_tract.csv")

# Confusion matrix for each file
define_label <- function(data){
  data <- data %>%
    rowwise() %>%
    mutate(
      max_prob = max(nh_white, nh_black, hispanic, asian, other),
      pred = case_when(
        nh_white == max_prob ~ "nh_white",
        nh_black == max_prob ~ "nh_black",
        hispanic == max_prob ~ "hispanic",
        asian == max_prob ~ "asian",
        other == max_prob ~ "other",
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()
  data
}

pred_surname <- define_label(pred_surname)
pred_surname_tract <- define_label(pred_surname_tract)
pred_surname_first_tract <- define_label(pred_surname_first_tract)

# Accuracy
accuracy <- function(data, data_name) {
  actual <- as.factor(data$race)
  predicted <- as.factor(data$pred)
  conf_matrix <- confusionMatrix(predicted, actual)
  accuracy <- conf_matrix$overall['Accuracy']
  tibble(
    dataframe = data_name,
    accuracy = accuracy
  )
}

list_dataframes <- tibble(pred_surname = pred_surname, 
                          pred_surname_tract = pred_surname_tract, 
                          pred_surname_first_tract = pred_surname_first_tract)

name_dataframes <-names(list_dataframes)

# Overall accuracy
overall <- map(
  .x = name_dataframes,
  .f = ~accuracy(
    data = list_dataframes[[.x]],
    data_name = .x
  )
)
overall_acc <- overall %>% bind_rows()

# Accuracy per ethnicity group
accuracy_race <- function(data, data_name, race) {
  data_race <- data %>% filter(race == !!race)
  actual <- data_race$race
  predicted <- data_race$pred
  acc_race <- length(which(actual == predicted))/length(actual)
  tibble(
    dataframe = data_name,
    race = race,
    accuracy = acc_race
  )
}

races <- unique(pred_surname$race)
group <- tibble()
for (elt in races){
  elt_acc <- map(
    .x = name_dataframes,
    .f = ~accuracy_race(
      data = list_dataframes[[.x]],
      data_name = .x,
      race = elt
    )
  )
  group <- group %>% bind_rows(elt_acc)
}

overall_acc <- overall_acc %>% 
  mutate(race = "all")

res <- group %>% bind_rows(overall_acc)
View(res)