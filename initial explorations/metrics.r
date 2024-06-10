library(pROC)
library(ggplot2)
library(MLmetrics)
library(dplyr)


#' Calculate the AUC of a binary classifier for each cohort.
#'
#' @param df data frame with columns "{cohort}_ind", each containing the
#'   predicted values of the binary classifier for that cohort
#' @param cohorts vector of strings giving cohorts to evaluate
#'
#' @return data frame containing the AUC for each cohort
get_auc_values <- function(df, cohorts) {

  auc_metrics <- list()

  for (cohort in cohorts) {

    fpr_tpr <- pROC::roc(
      df[[paste0(cohort, "_ind")]], df[[cohort]], levels = c(0, 1)
    )

    auc_value <- pROC::auc(fpr_tpr)

    auc_metrics[[cohort]] <- auc_value
  }

  auc_metrics_df <- data.frame(
    cohort = names(auc_metrics), auc = unlist(auc_metrics)
  )

  return(auc_metrics_df)
}


#' Compute the average predicted probability of belonging to a given cohort
#' among individuals who self-report belonging to that cohort.
#'
#' @param df data frame with columns "{cohort}", each containing predicted
#'   probabilities of belonging to that cohort; and a column race_col containing
#'   self-reported races
#' @param race_col column name for self-reported race
#' @param cohorts vector of strings giving cohorts to evaluate
#'
#' @return data frame containing the average predicted probability of belonging
#'   to a cohort among self-reported membes of that cohort
get_prob_race_given_race <- function(df, race_col, cohorts) {

  avg_probs <- list()

  for (cohort in cohorts) {
    subset <- subset(df, df[[race_col]] == cohort)

    avg_prob <- mean(subset[[cohort]])

    avg_probs[[cohort]] <- avg_prob
  }

  avg_probs_df <- data.frame(
    cohort = names(avg_probs), avg_prob = unlist(avg_probs)
  )

  return(avg_probs_df)
}


#' Compute the average predicted probability of belonging to the white cohort
#' among individuals who self-report belonging to a particular cohort. For
#' members of the white cohort, this should produce the same result as
#' get_prob_race_given_race.
#'
#' @param df data frame with a column race_col containing self-reported races;
#'   and a column white_prob containing predicted probabilities of belonging to
#'   the white cohort
#' @param race_col column name for self-reported race
#' @param white_prob column name for predicted probability of being white
#' @param cohorts vector of strings giving cohorts to evaluate
#'
#' @return data frame containing the average predicted probability of belonging
#'   to the white cohort among self-reported members of each cohort
get_prob_white_given_race <- function(df, race_col, white_prob, cohorts) {

  avg_white_probs <- list()

  for (cohort in cohorts) {
    subset <- subset(df, df[[race_col]] == cohort)

    avg_white_prob <- mean(subset[[white_prob]])

    avg_white_probs[[cohort]] <- avg_white_prob
  }

  avg_white_probs_df <- data.frame(
    cohort = names(avg_white_probs), avg_prob = unlist(avg_white_probs)
  )

  return(avg_white_probs_df)
}

get_white_rate <- function(df, race_col, cohorts) {
  #' Compute white rate for each cohort
  #' @param df data frame containing the predictions
  #' @param race_col  name of self-reported race column
  #' @param cohorts vector of cohort names
  #' @return dataframe with white rates
  white_rates <- list()
  for (cohort in cohorts) {
    subset <- subset(df, df[[race_col]] == cohort)
    white_rate <- sum(subset$race_pred == "nh_white") / nrow(subset)
    white_rates[[cohort]] <- white_rate
  }
  white_rates_df <- data.frame(
    cohort = names(white_rates), white_rate = unlist(white_rates)
  )
  return(white_rates_df)
}

get_TPFP_ratios <- function(df, race_col, cohorts) {
  #' Compute TP/FP ratios for each cohort
  #' @param df data frame containing the predictions
  #' @param race_col  name of self-reported race column
  #' @param cohorts vector of cohort names
  #' @return dataframe with TF/FP ratios
  tpfp_ratios <- list()
  for (cohort in cohorts) {
    tpfp_ratio <- df %>%
      dplyr::mutate(
        TP = ifelse(!!sym(race_col) == cohort & race_pred == cohort, 1, 0),
        FP = ifelse(!!sym(race_col) != cohort & race_pred == cohort, 1, 0)
      ) %>%
      dplyr::summarise(
        TP = sum(TP),
        FP = sum(FP),
        ratio = TP / FP
      )
    tpfp_ratios[[cohort]] <- tpfp_ratio$ratio
  }
  tpfp_ratios_df <- data.frame(
    cohort = names(tpfp_ratios), value = unlist(tpfp_ratios)
  )
  return(tpfp_ratios_df)
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

compute_specificities <- function(data, cohorts, race_pred_col, self_rep_race_col) {
  #' Compute specificity for each cohort
  #' @param data data frame containing the predictions
  #' @param cohorts vector of cohort names
  #' @param race_pred_col name of prediction column
  #' @param self_rep_race_col  name of self-reported race column
  #' @return dataframe with specificities
  specificities <- data.frame(cohort = character(), specificity = numeric())

  for (cohort in cohorts) {
    race_ind <- ifelse(data[[self_rep_race_col]] == cohort, 1, 0)
    race_pred_ind <- ifelse(data[[race_pred_col]] == cohort, 1, 0)

    specificity_value <- MLmetrics::Specificity(
      y_pred = race_pred_ind, y_true = race_ind, positive = 1
    )

    result_row <- data.frame(
      cohort = cohort, specificity = specificity_value
    )
    specificities <- rbind(specificities, result_row)
  }

  return(specificities)
}

compute_positive_likelihood_ratios <- function(data, cohorts, race_pred_col, self_rep_race_col) {
  #' Compute positive likelihood ratio for each cohort
  #' @param data data frame containing the predictions
  #' @param cohorts vector of cohort names
  #' @param race_pred_col name of prediction column
  #' @param self_rep_race_col  name of self-reported race column
  #' @return dataframe with positive likelihood ratios
  positive_likelihood_ratios <- data.frame(
    cohort = character(), positive_likelihood_ratio = numeric()
  )

  for (cohort in cohorts) {
    recall_value <- MLmetrics::Recall(
      y_pred = data[[race_pred_col]],
      y_true = data[[self_rep_race_col]],
      positive = cohort
    )

    race_ind <- ifelse(data[[self_rep_race_col]] == cohort, 1, 0)
    race_pred_ind <- ifelse(data[[race_pred_col]] == cohort, 1, 0)

    specificity_value <- MLmetrics::Specificity(
      y_pred = race_pred_ind, y_true = race_ind, positive = 1
    )

    # Compute positive likelihood ratio
    positive_likelihood_ratio <- recall_value / (1 - specificity_value)

    # Store the positive likelihood ratio
    result_row <- data.frame(
      cohort = cohort,
      positive_likelihood_ratio = positive_likelihood_ratio
    )

    # Append to the data frame
    positive_likelihood_ratios <- rbind(positive_likelihood_ratios, result_row)
  }

  return(positive_likelihood_ratios)
}

get_avg_outcomes <- function(df, race_col, race_pred_col, cohorts, birdie = FALSE) {
  #' Compute average outcomes for each cohort
  #' @param df data frame containing the predictions
  #' @param race_col  name of self-reported race column
  #' @param race_pred_col name of prediction column
  #' @param cohorts vector of cohort names
  #' @param birdie whether or not it is a birdie model
  #' @return list of data frames containing average outcomes
  if (birdie) {
    outcome_var <- "outcome_pred"
  } else {
    outcome_var <- "target"
  }

  # PROBABILITY-WEIGHTED AVERAGE OUTCOME BY GROUP
  wavg_est_outcomes <- list()
  for (cohort in cohorts) {
    subset <- subset(df, df[[race_col]] == cohort)
    wavg_est_outcome <- weighted.mean(subset[[outcome_var]], subset[[cohort]])
    wavg_est_outcomes[[cohort]] <- wavg_est_outcome
  }

  wavg_est_outcomes_df <- data.frame(
    cohort = names(wavg_est_outcomes),
    wavg_est_outcome = unlist(wavg_est_outcomes)
  )

  # AVERAGE OUTCOME BY GROUP - CLASSIFIED USING HIGHEST PROBABILITY
  avg_est_outcomes <- list()
  for (cohort in cohorts) {
    subset <- subset(df, df[[race_pred_col]] == cohort)
    avg_est_outcome <- mean(subset[[outcome_var]])
    avg_est_outcomes[[cohort]] <- avg_est_outcome
  }

  avg_est_outcomes_df <- data.frame(
    cohort = names(avg_est_outcomes), avg_est_outcome = unlist(avg_est_outcomes)
  )

  return(list(
    wavg_outcomes = wavg_est_outcomes_df, avg_outcomes = avg_est_outcomes_df
  ))
}

get_actl_vs_exp_probs <- function(df, race_col, race_pred_col, cohorts) {
  #' Compute actual vs expected values for each cohort using probabilities
  #' @param df data frame containing the predictions
  #' @param race_col  name of self-reported race column
  #' @param race_pred_col name of prediction column
  #' @param cohorts vector of cohort names
  #' @return dataframe with actual vs expected values
  actual_vs_expected_values <- list()
  for (cohort in cohorts) {
    actual_vs_expected <- sum(
      df[[paste0(cohort, "_ind")]]
    ) / sum(df[[cohort]])
    actual_vs_expected_values[[cohort]] <- actual_vs_expected
  }
  actual_vs_expected_df <- data.frame(
    cohort = names(actual_vs_expected_values),
    value = unlist(actual_vs_expected_values)
  )
  return(actual_vs_expected_df)
}

get_actl_vs_exp_classification <- function(df, race_col, race_pred_col, cohorts) {
  #' Compute actual vs expected values for each cohort using classification
  #' @param df data frame containing the predictions
  #' @param race_col  name of self-reported race column
  #' @param race_pred_col name of prediction column
  #' @param cohorts vector of cohort names
  #' @return dataframe with actual vs expected values
  actual_vs_expected_values <- list()
  for (cohort in cohorts) {
    actual_vs_expected <- sum(
      df[[paste0(cohort, "_ind")]]
    ) / sum(df[[race_pred_col]] == cohort)
    actual_vs_expected_values[[cohort]] <- actual_vs_expected
  }
  actual_vs_expected_df <- data.frame(
    cohort = names(actual_vs_expected_values),
    value = unlist(actual_vs_expected_values)
  )
  return(actual_vs_expected_df)
}


#' Plot histograms of predicted probabilities for each cohort.
#'
#' @param df data frame with columns "{cohort}", each containing predicted
#'   probabilities of belonging to that cohort
#' @param cohorts vector of strings giving cohorts to evaluate
#' @param method name of method that was used to predict probabilities; used for
#'   labeling png files
#' @param outpath file path to directory where png should be saved
#'
#' @return nothing, but writes png files
plot_histograms <- function(df, cohorts, method, outpath) {

  title_labels <- c(
    nh_white = "White",
    nh_black = "Black",
    asian = "Asian",
    native = "Native",
    multiple = "Multiple",
    hispanic = "Hispanic",
    other = "Other"
  )

  for (cohort in cohorts) {
    tryCatch(
      {
        plot_data <- df %>%
          select(all_of(cohort))

        ggplot2::ggplot(plot_data, aes(x = .data[[cohort]])) +
          ggplot2::geom_histogram(
            bins = 10,
            fill = "skyblue",
            color = "black",
            alpha = 0.7,
            binwidth = NULL
          ) +
          ggplot2::labs(
            title = title_labels[[cohort]],
            x = "Predicted Probability",
            y = "Frequency"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.text = ggplot2::element_text(size = 10),
            axis.title = ggplot2::element_text(size = 12),
            plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
            legend.position = "none",
            plot.background = ggplot2::element_rect(fill = "white")
          )

        ggplot2::ggsave(
          filename = sprintf(
            "%s/%s/probs_distribution_%s_%s.png",
            outpath, method, method, cohort
          ),
          width = 8, height = 6, units = "in", dpi = 300
        )
      },

      error = function(e) {
        cat(
          paste(
            "Column", cohort, "not found in dataframe when plotting histogram/n"
          )
        )
      }
    )
  }
}


#' Run all metrics on a data frame of predictions produced by a given method.
#'
#' @param data data frame with columns containing predicted probabilities for
#'   race cohorts, one of which must be "nh_white"; a column self_rep_race
#'   containing the self-reported race for each individual; and a column
#'   "race_pred" containing the predicted race cohort for each individual
#' @param cohorts vector of strings giving names of columns of data containing
#'   predicted probabilities for each race cohort
#' @param self_rep_race column name for self-reported race
#' @param method name of method that was used to predict probabilities; used for
#'   labeling files
#' @param outpath file path to directory where files should be saved
#' @param birdie TRUE if model was a birdie model, FALSE otherwise
#'
#' @return nothing but saves metrics to disk
run_metrics <- function(data, cohorts, self_rep_race, method, outpath, birdie = FALSE) {

  dir.create(file.path(outpath, method), showWarnings = FALSE, recursive = TRUE)

  for (cohort in cohorts) {
    data[[paste0(cohort, "_ind")]] <- ifelse(
      data[[self_rep_race]] == cohort, 1, 0
    )
  }

  # AUC
  auc_metrics <- get_auc_values(
    data,
    cohorts
  )
  write.csv(
    auc_metrics,
    file.path(outpath, method, paste0("auc_metrics_", method, ".csv")),
    row.names = FALSE
  )

  # given you are of a specific cohort, what's the avg probability of being assigned that cohort # nolint: line_length_linter.
  prob_race_given_race <- get_prob_race_given_race(
    data,
    self_rep_race,
    cohorts
  )
  write.csv(
    prob_race_given_race,
    file.path(outpath, method, paste0("prob_race_given_race_", method, ".csv")),
    row.names = FALSE
  )

  # average probability of incorrect race
  prob_incorrect_race <- prob_race_given_race %>%
    mutate(
      avg_prob_incorrect = 1 - avg_prob,
      ratio = avg_prob / avg_prob_incorrect
    )
  write.csv(
    prob_incorrect_race,
    file.path(outpath, method, paste0("prob_incorrect_race_", method, ".csv")),
    row.names = FALSE
  )

  # given you are of a specific cohort,
  # what's the avg probability of being assigned white
  prob_white_given_race <- get_prob_white_given_race(
    data,
    self_rep_race,
    "nh_white",
    cohorts
  )
  write.csv(
    prob_white_given_race,
    file.path(
      outpath, method, paste0("prob_white_given_race_", method, ".csv")
    ),
    row.names = FALSE
  )

  plot_histograms(
    data,
    cohorts,
    method,
    outpath
  )

  data <- data %>%
    dplyr::mutate(
      race_pred = colnames(
        select(., all_of(cohorts))
      )[max.col(select(., all_of(cohorts)), "first")]
    )

  # white rate
  white_rates <- get_white_rate(data, self_rep_race, cohorts)
  write.csv(
    white_rates,
    file.path(outpath, method, paste0("white_rate_", method, ".csv")),
    row.names = FALSE
  )

  # TP/FP ratio
  tpfp_ratios <- get_TPFP_ratios(data, self_rep_race, cohorts)
  write.csv(
    tpfp_ratios,
    file.path(outpath, method, paste0("TPFP_ratios_", method, ".csv")),
    row.names = FALSE
  )

  # precision
  precisions <- compute_precisions(data, cohorts, "race_pred", self_rep_race)
  write.csv(
    precisions,
    file.path(outpath, method, paste0("precision_", method, ".csv")),
    row.names = FALSE
  )

  # recall
  recalls <- compute_recalls(data, cohorts, "race_pred", self_rep_race)
  write.csv(
    recalls,
    file.path(outpath, method, paste0("recall_", method, ".csv")),
    row.names = FALSE
  )

  # specificity
  specificities <- compute_specificities(
    data, cohorts, "race_pred", self_rep_race
  )
  write.csv(
    specificities,
    file.path(outpath, method, paste0("specificity_", method, ".csv")),
    row.names = FALSE
  )

  # positive likelihood ratio
  plrs <- compute_positive_likelihood_ratios(
    data, cohorts, "race_pred", self_rep_race
  )
  write.csv(
    plrs,
    file.path(outpath, method, paste0("likelihood_ratios_", method, ".csv")),
    row.names = FALSE
  )

  wavg_and_avg_outcomes <- get_avg_outcomes(
    data, self_rep_race, "race_pred", cohorts
  )
  write.csv(
    wavg_and_avg_outcomes$wavg_outcomes,
    file.path(outpath, method, paste0("wavg_outcomes_", method, ".csv")),
    row.names = FALSE
  )
  write.csv(
    wavg_and_avg_outcomes$avg_outcomes,
    file.path(outpath, method, paste0("avg_outcomes_", method, ".csv")),
    row.names = FALSE
  )

  actl_vs_exp_probs <- get_actl_vs_exp_probs(
    data, self_rep_race, "race_pred", cohorts
  )
  write.csv(
    actl_vs_exp_probs,
    file.path(
      outpath, method, paste0("actl_vs_exp_probs_", method, ".csv")
    ),
    row.names = FALSE
  )

  actl_vs_exp_classification <- get_actl_vs_exp_classification(
    data, self_rep_race, "race_pred", cohorts
  )
  write.csv(
    actl_vs_exp_classification,
    file.path(
      outpath, method, paste0("actl_vs_exp_classification_", method, ".csv")
    ),
    row.names = FALSE
  )
}
