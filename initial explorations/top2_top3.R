# ----------------
#  top 3 for relaxed accuracy - recall- precision mai 23
# ----------------
setwd("~/Desktop/GRI/soa05_tutorial-main") 
library(readxl)
library(fastDummies)

BIFSG <- read_excel("data/bifsg.xlsx")
BIFSG <- fastDummies::dummy_cols(BIFSG, select_columns = "race")
df <- BIFSG

prob_columns <- c("nh_white", "nh_black", "asian", "hispanic", "other")  # Update with your actual column names
df[prob_columns] <- lapply(df[prob_columns], as.numeric)

# Find the index of the column with the first maximum probability for each row
first_index_max <- function(row) {
  sorted_indices <- order(-row)
  
  second_index <- sorted_indices[1]
  
  return(second_index)
}

max_prob_index_first<- apply(df[prob_columns], 1, first_index_max )
# Get the corresponding class name based on the index
predicted_first <- prob_columns[max_prob_index_first]



# Find the index of the column with the second maximum probability for each row
second_index_max <- function(row) {
  sorted_indices <- order(-row)

  second_index <- sorted_indices[2]
  
  return(second_index)
}

max_prob_index_second <- apply(df[prob_columns], 1, second_index_max )
# Get the corresponding class name based on the index
predicted_second <- prob_columns[max_prob_index_second]




# Find the index of the column with the third maximum probability for each row
third_index_max <- function(row) {
  sorted_indices <- order(-row)
  
  third_index <- sorted_indices[3]
  
  return(third_index)
}

max_prob_index_third <- apply(df[prob_columns], 1, third_index_max )
# Get the corresponding class name based on the index
predicted_third <- prob_columns[max_prob_index_third]



#crear una funcion que escoja entre first y second para hacer la confusion matriz el que le cnvenga al modelo y si no pailas que esoja cualquiera

first_or_second <- function(df, predicted_first, predicted_second){
  race <- df$race
  predicted <- character(length(race))  # Creamos un vector vacío del mismo tamaño que x
  
  for (i in seq_along(race)) {
    if (race[i] == predicted_first[i]) {
      predicted [i] <- predicted_first[i]  # Si el valor está en y, lo agregamos al nuevo vector
    } 
    else if (race[i] == predicted_second[i]) {
      predicted [i] <- predicted_second[i]  # Si el valor está en z, lo agregamos al nuevo vector
    } 
    else {
      predicted [i] <- sample(c( predicted_first[i], predicted_second[i]), 1)  # Si no está en y ni en z, agregamos NA al nuevo vector
    }
  }
  return(predicted)
}

first_or_second_or_third <- function(df, predicted_first, predicted_second,predicted_third ){
  race <- df$race
  predicted <- character(length(race))  # Creamos un vector vacío del mismo tamaño que x
  
  for (i in seq_along(race)) {
    if (race[i] == predicted_first[i]) {
      predicted [i] <- predicted_first[i]  # Si el valor está en y, lo agregamos al nuevo vector
    } 
    else if (race[i] == predicted_second[i]) {
      predicted [i] <- predicted_second[i]  # Si el valor está en z, lo agregamos al nuevo vector
    } 
    else if (race[i] == predicted_third[i]) {
      predicted [i] <- predicted_third[i]  # Si el valor está en z, lo agregamos al nuevo vector
    } 
    else {
      predicted [i] <- sample(c( predicted_first[i], predicted_second[i],predicted_third[i]), 1)  # Si no está en y ni en z, agregamos NA al nuevo vector
    }
  }
  return(predicted)
}

#ver missing values- just to check in advance
valores_faltantes <- is.na(predicted_first)
# Contar valores faltantes
sum(valores_faltantes)



#predicted <- first_or_second(head(df, 10), head(predicted_first, 10), head(predicted_second, 10))
predicted <- first_or_second(df, predicted_first, predicted_second)

predicted <- first_or_second_or_third(df, predicted_first, predicted_second, predicted_third)

# Create confusion matrix
conf_matrix <- table(df$race, predicted)
# metrics
df$predicted <-predicted 


compute_recalls <- function(data, cohorts, race_pred_col, self_rep_race_col) {
  #' Compute recall for each cohort
  #' @param data data frame containing the predictions
  #' @param cohorts vector of cohort names
  #' @param race_pred_col name of prediction column
  #' @param self_rep_race_col  name of self-reported race column
  #' @return dataframe with recalls
  recalls <- data.frame(cohort = character(), recall = numeric())
  
  for (cohort in cohorts) {
    recall_value <- MLmetrics::Recall(
      y_pred = data[[race_pred_col]],
      y_true = data[[self_rep_race_col]],
      positive = cohort
    )
    
    result_row <- data.frame(cohort = cohort, recall = recall_value)
    recalls <- rbind(recalls, result_row)
  }
  
  return(recalls)
}
cohorts <- c("nh_white", "nh_black", "asian", "hispanic", "other")
recalls <- compute_recalls(df, cohorts, "predicted", "race")






compute_precisions <- function(data, cohorts, race_pred_col, self_rep_race_col) {
  #' Compute precision for each cohort
  #' @param data data frame containing the predictions
  #' @param cohorts vector of cohort names
  #' @param race_pred_col name of prediction column
  #' @param self_rep_race_col  name of self-reported race column
  #' @return dataframe with precisions
  precisions <- data.frame(cohort = character(), precision = numeric())
  
  for (cohort in cohorts) {
    precision_value <- MLmetrics::Precision(
      y_pred = data[[race_pred_col]],
      y_true = data[[self_rep_race_col]],
      positive = cohort
    )
    
    result_row <- data.frame(
      cohort = cohort, precision = precision_value
    )
    precisions <- rbind(precisions, result_row)
  }
  
  return(precisions)
}
precisions <- compute_precisions(df, cohorts, "predicted", "race")





get_auc_values <- function(df, cohorts) {
  
  auc_metrics <- list()
  fpr_tprs<- list()
  
  for (cohort in cohorts) {
    
    fpr_tpr <- pROC::multiclass.roc(
      df[[paste0("race_",cohort)]], df[[cohort]], levels = c(0, 1)
    )
    
    auc_value <- pROC::auc(fpr_tpr)
    auc_metrics[[cohort]] <- auc_value
    fpr_tprs[[cohort]] <- fpr_tpr
  }
  
  auc_metrics_df <- data.frame(
    cohort = names(auc_metrics), auc = unlist(auc_metrics)
  )
  
  return(fpr_tprs)
}
get_auc_values(df, c("nh_white", "nh_black", "asian", "hispanic", "other"))
 

 