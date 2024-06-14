            # ----------------------
            # implementation of nested binomial approach
            # ----------------------


# ----------------------
# data preparation: trial with subsample n=99
# ----------------------            
setwd("~/Desktop/GRI/cascade") 
library(dplyr)
library(readr)

df <- readr::read_csv(
  "stat-methods-imputing-race-ethnicity-data.csv",
  col_types = cols(
    GEOID_block = col_character(),
    GEOID_blockgroup = col_character(),
    GEOID_tract = col_character()
  )
) %>%
  dplyr::rename(
    surname = last_name,
    first = first_name,
    middle = middle_name,
  ) %>%
  dplyr::mutate(
    state_code = stringr::str_sub(GEOID_block, 1L, 2L),
    county = stringr::str_sub(GEOID_block, 3L, 5L),
    tract = stringr::str_sub(GEOID_tract, 6L, 11L)
  )

fips_codes <- tigris::fips_codes %>%
  dplyr::select(state, state_code) %>%
  dplyr::distinct()

df <- df %>%
  dplyr::left_join(
    fips_codes,
    by = "state_code"
  )

# wru does not support US territories in voter.file, and will throw an error if
# it finds them. To get around this, for surname-only predictions only pass the
# surname column to voter.file, and for geographic methods filter out rows with
# US territories.

territories <- c("AS", "GU", "MP", "PR", "VI")

df_no_territories <- df %>%
  dplyr::filter(!state %in% territories)


#### model white and non white
set.seed(123)
muestra <- df_no_territories %>% 
  group_by("race", "GEOID_tract") %>%
  sample_frac(0.001)


muestra$GEOID <- paste0(muestra$state_code, muestra$county) 
muestra$surname <- toupper(muestra$surname)

# load database surnames
load("/Users/anamariapatron/Desktop/GRI/cascade/wru-main/data/surnames2010.RData")

# load database geolocalization
load("/Users/anamariapatron/Desktop/GRI/cascade/census_county.rda")
census
#data <- readRDS("census_test_nj_block_2010.rds")
#geo <- data$NJ$tract

# model: multiplication of probabilities
# Assuming 'df' is your dataframe and 'variables_to_drop' is a vector of the variable names you want to remove
#muestra <- muestra[, !(names(muestra) %in% c("p_whi", "p_bla", "p_asi", "p_oth", "p_nonwhi"))]
#muestra[2, ]

# paste p(S_i | R_i) from voters_file
muestra <- merge(muestra, surnames2010, by = "surname", all.x = TRUE)

#colnames(muestra)[colnames(muestra) == "geo_id"] <- "GEOID"
# paste p(R_i | G_i) from census- counts
subset_census <- subset(census, select = c("GEOID","r_whi", "r_bla", "r_asi", "r_his","r_oth"))
muestra <- merge(muestra, subset_census, by = "GEOID", all.x = TRUE)

# ----------------------
# hierarchihal model
# ----------------------
# first stage: white vs non white
muestra$p_nonwhi <- muestra$p_bla + muestra$p_his + muestra$p_asi + muestra$p_oth
muestra$r_nonwhi <- muestra$r_bla + muestra$r_his + muestra$r_asi + muestra$r_oth
muestra$pred_whi <- muestra$p_whi* muestra$r_whi
muestra$pred_nonwhi <- muestra$p_nonwhi* muestra$r_nonwhi
denominator <- rowSums(muestra[, c("pred_nonwhi", "pred_whi")])         
muestra$pred_whi <-muestra$pred_whi/denominator
muestra$pred_nonwhi <-muestra$pred_nonwhi/denominator

# second stage: asian vs non asian
muestra$p_nonasi <- muestra$p_bla + muestra$p_his + muestra$p_oth
muestra$r_nonasi <- muestra$r_bla + muestra$r_his + muestra$r_oth
muestra$pred_asi <- muestra$p_asi* muestra$r_asi
muestra$pred_nonasi <- muestra$p_nonasi* muestra$r_nonasi
denominator <- rowSums(muestra[, c("pred_nonasi", "pred_asi")])         
muestra$pred_asi <-muestra$pred_asi/denominator
muestra$pred_nonasi <-muestra$pred_nonasi/denominator

# third stage: black vs non black
muestra$p_nonbla <-   muestra$p_his + muestra$p_oth
muestra$r_nonbla <-  muestra$r_his + muestra$r_oth
muestra$pred_bla <- muestra$p_bla* muestra$r_bla
muestra$pred_nonbla <- muestra$p_nonbla* muestra$r_nonbla
denominator <- rowSums(muestra[, c("pred_nonbla", "pred_bla")])         
muestra$pred_bla <-muestra$pred_bla/denominator
muestra$pred_nonbla <-muestra$pred_nonbla/denominator

# fourth stage: hispanic vs non hispanic
muestra$p_nonhis <-  muestra$p_oth
muestra$r_nonhis <-  muestra$r_oth
muestra$pred_his <- muestra$p_his* muestra$r_his
muestra$pred_nonhis <- muestra$p_nonhis* muestra$r_nonhis
denominator <- rowSums(muestra[, c("pred_nonhis", "pred_his")])         
muestra$pred_his <- muestra$pred_his/denominator
muestra$pred_nonhis <-muestra$pred_nonhis/denominator

# final vector of probas
muestra$vec_whi <- muestra$pred_whi
muestra$vec_asi <- muestra$pred_nonwhi * muestra$pred_asi
muestra$vec_bla <- muestra$pred_nonwhi * muestra$pred_asi* muestra$pred_bla
muestra$vec_his <- muestra$pred_nonwhi * muestra$pred_asi* muestra$pred_nonbla * muestra$pred_his
muestra$vec_oth <- muestra$pred_nonwhi * muestra$pred_asi* muestra$pred_nonbla* muestra$pred_nonhis

# ----------------------
# bisg model
# ----------------------
muestra$bisg_whi <- muestra$p_whi* muestra$r_whi
muestra$bisg_asi <- muestra$p_asi* muestra$r_asi
muestra$bisg_bla <- muestra$p_bla* muestra$r_bla
muestra$bisg_his <- muestra$p_his* muestra$r_his
muestra$bisg_oth <- muestra$p_oth* muestra$r_oth
# normalization
denominator <- rowSums(muestra[, c("bisg_whi", "bisg_asi", "bisg_bla", "bisg_his", "bisg_oth")])  
muestra$bisg_whi <- muestra$bisg_whi/denominator
muestra$bisg_asi <- muestra$bisg_asi/denominator
muestra$bisg_bla <- muestra$bisg_bla/denominator
muestra$bisg_his <- muestra$bisg_his/denominator
muestra$bisg_oth <- muestra$bisg_oth/denominator



# ----------------------
# confusion matrix
# ----------------------

# compute maximum probability from vec
df <- muestra[, c("surname","first","middle","race","vec_whi", "vec_asi", "vec_bla", "vec_his","vec_oth", "pred_whi","pred_nonwhi","pred_asi","pred_nonasi" ,"pred_bla","pred_nonbla","pred_his","pred_nonhis","bisg_whi","bisg_asi","bisg_bla","bisg_his" ,"bisg_oth" )]
prob_columns <- c("vec_whi", "vec_asi", "vec_bla", "vec_his","vec_oth")   
df[prob_columns] <- lapply(df[prob_columns], as.numeric)
first_index_max <- function(row) {
  sorted_indices <- order(-row)
  
  second_index <- sorted_indices[1]
  
  return(second_index)
}

max_prob_index_first<- apply(df[prob_columns], 1, first_index_max )
predicted_vec <- prob_columns[max_prob_index_first]
df$vec <-predicted_vec 

df$vec <- ifelse(df$vec  == "vec_asi", "asian", 
                  ifelse(df$vec  == "vec_whi", "nh_white", 
                         ifelse(df$vec  == "vec_other", "other",
                                ifelse(df$vec  == "vec_bla", "nh_black",
                                       ifelse(df$vec  == "vec_his", "hispanic", df$vec )))))


conf_matrix_vec <- table(df$race, df$vec )
# we see a problem with minorities as well
save(df, file = "results_hierarchichal_subsample99.rda")


# compare with treshold = 0.8 and 'cutting' the tree... the order matters!
for (i in 1:nrow(df)) {
  if (df[i, "pred_whi"] > 0.7) {
    df[i, "pred_race"] <- "nh_white"
  } else if (df[i, "pred_asi"] > 0.7) {
    df[i, "pred_race"] <- "asian"
  } else if (df[i, "pred_bla"] > 0.7) {
    df[i, "pred_race"] <- "nh_black"
  }else {
    df[i, "pred_race"] <- "other"
  }
}

conf_matrix_pred <- table(df$race, df$pred_race)

# compare with bisg

prob_columns_bisg <- c("bisg_whi","bisg_asi","bisg_bla","bisg_his" ,"bisg_oth")   
df[prob_columns_bisg] <- lapply(df[prob_columns_bisg], as.numeric)
max_prob_index_first_bisg<- apply(df[prob_columns_bisg], 1, first_index_max )
predicted_bisg <- prob_columns_bisg[max_prob_index_first]
df$bisg <-predicted_bisg

df$bisg <- ifelse(df$bisg == "bisg_asi", "asian", 
                       ifelse(df$bisg == "bisg_whi", "nh_white", 
                              ifelse(df$bisg == "bisg_other", "other",
                                     ifelse(df$bisg == "bisg_bla", "nh_black",
                                            ifelse(df$bisg == "bisg_his", "hispanic", df$bisg)))))


conf_matrix_bisg <- table(df$race, df$bisg)

# export results
library(openxlsx)
write.xlsx(conf_matrix_pred, "conf_matrix_pred.xlsx") # cutting tree
write.xlsx(conf_matrix_vec, "conf_matrix_vec.xlsx") # multiplying leafs of tree
write.xlsx(conf_matrix_bisg, "conf_matrix_bisg.xlsx") # normal bisg


# ----------------------
# metrics
# ----------------------

# define functions
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

get_auc_values <- function(df, cohorts, type) {
  a <- df
  if (type== 'bisg') {
    names(a)[names(a) %in% c("bisg_whi","bisg_asi","bisg_bla","bisg_his" ,"bisg_oth")] <- c("nh_white", "asian", "nh_black", "hispanic", "other")
  } else if (type== 'pred') {
    names(a)[names(a) %in% c("pred_whi","pred_asi","pred_bla","pred_his" ,"pred_oth")] <- c("nh_white",  "asian","nh_black", "hispanic", "other")
  } else if (type== 'vec') {
    names(a)[names(a) %in% c("vec_whi", "vec_asi", "vec_bla", "vec_his","vec_oth")] <- c("nh_white", "asian","nh_black", "hispanic", "other")
  }
  
  auc_metrics <- list()
  fpr_tprs<- list()
  
  for (cohort in cohorts) {
    
    fpr_tpr <- pROC::multiclass.roc(
      df[[paste0("race_",cohort)]], a[[cohort]], levels = c(0, 1)
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

# results
# I need to create dummies first
df <- fastDummies::dummy_cols(df, select_columns = "race")
cohorts <- c("nh_white", "nh_black", "asian", "hispanic", "other")



# multiplying leafs of tree
precisions_vec <- compute_precisions(df, cohorts, "vec", "race")
recalls_vec <- compute_recalls(df, cohorts, "vec", "race")
auc_vec <- get_auc_values(df, c("nh_white", "nh_black", "asian", "hispanic", "other"), 'vec')

# bisg 
precisions_bisg <- compute_precisions(df, cohorts, "bisg", "race")
recalls_bisg <- compute_recalls(df, cohorts, "bisg", "race")
auc_bisg <- get_auc_values(df, c("nh_white", "nh_black", "asian", "hispanic", "other"), 'bisg')

# cutting tree
precisions_pred <- compute_precisions(df, cohorts, "pred_race", "race")
recalls_pred <- compute_recalls(df, cohorts, "pred_race", "race")
#since probabilities are no suming one, not sure how to find auc here
#auc_pred <- get_auc_values(df, c("nh_white", "nh_black", "asian", "hispanic", "other"), 'pred')


b<-cbind(precisions_pred, recalls_pred,precisions_vec, recalls_vec,precisions_bisg,recalls_bisg)
write.xlsx(b, "metrics_draft.xlsx")
write.xlsx(df, "fulldataset.xlsx")


