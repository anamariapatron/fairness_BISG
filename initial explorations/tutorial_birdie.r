# If on a Windows machine, must install RTools first:
# https://cran.r-project.org/bin/windows/Rtools/

# Install required packages
renv::restore()

library(tidyverse)
library(birdie)
source("metrics.R")

# TODO: set path to output directory
OUTPUT <- "C:/Documents"

for (method in c("SA", "SA_birdie", "BISG", "BISG_birdie", "fBISG", "fBISG_birdie")) {
  dir = file.path(OUTPUT, method)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive=TRUE)
  }
}

# TODO: set path to tutorial data
df <- readr::read_csv(
  "tutorial_20231030.csv",
  # force ZCTA to character type, otherwise birdie::bisg throws an error
  col_types = cols(ZCTA = col_character())
)

# Initial race predictions using BISG or fBISG methodology. Other BISG software
# such as the wru package can also be used for this step.

# -------------------------
# Surname Analysis
# -------------------------

# Create initial race predictions using Surname Analysis
sa_probs <- birdie::bisg(
  ~ nm(last_name),
  data = df,
  p_r = p_r_natl()
)

# Horizontally concatenate the initial SA predictions to the original data
sa <- cbind(df, sa_probs)

sa <- sa %>% dplyr::rename(
  nh_white = pr_white,
  nh_black = pr_black,
  asian = pr_asian,
  hispanic = pr_hisp,
  american_indians_alaska_native = pr_aian,
  other = pr_other
)

# Save csv of initial SA predictions
readr::write_csv(sa, file.path(OUTPUT, "SA", "predictions_SA.csv"))

# Run custom metrics function on initial SA predictions. Outputs (csv and png)
# are written to the "SA" subdirectory of OUTPUT directory.
run_metrics(
  sa,
  c("nh_white", "nh_black", "asian", "hispanic", "other"),
  "race",
  "SA",
  OUTPUT
)

# Run birdie model using initial SA predictions to predict racial disparities
# in target
birdie_sa_model <- birdie::birdie(
  sa_probs,
  target ~ 1,
  data = df,
  family = gaussian()
)

# Extract updated racial probability predictions using fitted function and
# horizontally concatenate them to the original data
sa_birdie <- cbind(df, fitted(birdie_sa_model))

sa_birdie <- sa_birdie %>% dplyr::rename(
  nh_white = pr_white,
  nh_black = pr_black,
  asian = pr_asian,
  hispanic = pr_hisp,
  american_indians_alaska_native = pr_aian,
  other = pr_other
)

# Get birdie predictions for outcome variable (`target`) by subtracting the
# residuals of the birdie model from the observed values of the outcome
birdie_sa_residuals <- residuals(birdie_sa_model) %>%
  as.data.frame() %>%
  dplyr::rename(
    residual = target
  )

sa_birdie <- cbind(sa_birdie, birdie_sa_residuals)

sa_birdie <- sa_birdie %>% dplyr::mutate(
  outcome_pred = target - residual
)

# Save csv of updated SA predictions, and birdie model's predicted values and
# residuals of the outcome variable
readr::write_csv(
  sa_birdie,
  file.path(OUTPUT, "SA_birdie", "predictions_SA_birdie.csv")
)

# Run custom metrics function on updated SA predictions. Outputs (csv and png)
# are written to the "SA_birdie" subdirectory of OUTPUT directory.
run_metrics(
  sa_birdie,
  c("nh_white", "nh_black", "asian", "hispanic", "other"),
  "race",
  "SA_birdie",
  OUTPUT,
  birdie = TRUE
)


# -------------------------
# BISG
# -------------------------

bisg_probs <- birdie::bisg(
  ~ nm(last_name) + zip(ZCTA),
  data = df,
  p_r = p_r_natl()
)

bisg <- cbind(df, bisg_probs)

bisg <- bisg %>% dplyr::rename(
  nh_white = pr_white,
  nh_black = pr_black,
  asian = pr_asian,
  hispanic = pr_hisp,
  american_indians_alaska_native = pr_aian,
  other = pr_other
)

readr::write_csv(bisg, file.path(OUTPUT, "BISG", "predictions_BISG.csv"))

run_metrics(
  bisg,
  c("nh_white", "nh_black", "asian", "hispanic", "other"),
  "race",
  "BISG",
  OUTPUT
)

birdie_bisg_model <- birdie::birdie(
  bisg_probs,
  target ~ 1,
  data = df,
  family = gaussian()
)

bisg_birdie <- cbind(df, fitted(birdie_bisg_model))

bisg_birdie <- bisg_birdie %>% dplyr::rename(
  nh_white = pr_white,
  nh_black = pr_black,
  asian = pr_asian,
  hispanic = pr_hisp,
  american_indians_alaska_native = pr_aian,
  other = pr_other
)

birdie_residuals <- residuals(birdie_bisg_model) %>%
  as.data.frame() %>%
  dplyr::rename(
    residual = target
  )

bisg_birdie <- cbind(bisg_birdie, birdie_residuals)

bisg_birdie <- bisg_birdie %>% dplyr::mutate(
  outcome_pred = target - residual
)

readr::write_csv(
  bisg_birdie,
  file.path(OUTPUT, "BISG_birdie", "predictions_BISG_birdie.csv")
)

run_metrics(
  bisg_birdie,
  c("nh_white", "nh_black", "asian", "hispanic", "other"),
  "race",
  "BISG_birdie",
  OUTPUT,
  birdie = TRUE
)


# -------------------------
# fBISG
# -------------------------

fbisg_probs <- birdie::bisg_me(
  ~ nm(last_name) + zip(ZCTA),
  data = df,
  p_r = p_r_natl(),
  iter = 2000
)

fbisg <- cbind(df, fbisg_probs)

fbisg <- fbisg %>% dplyr::rename(
  nh_white = pr_white,
  nh_black = pr_black,
  asian = pr_asian,
  hispanic = pr_hisp,
  american_indians_alaska_native = pr_aian,
  other = pr_other
)

readr::write_csv(
  fbisg,
  file.path(OUTPUT, "fBISG", "predictions_fBISG.csv")
)

run_metrics(
  fbisg,
  c("nh_white", "nh_black", "asian", "hispanic", "other"),
  "race",
  "fBISG",
  OUTPUT
)

birdie_fbisg_model <- birdie::birdie(
  fbisg_probs,
  target ~ 1,
  data = df,
  family = gaussian()
)

fbisg_birdie <- cbind(df, fitted(birdie_fbisg_model))

fbisg_birdie <- fbisg_birdie %>% dplyr::rename(
  nh_white = pr_white,
  nh_black = pr_black,
  asian = pr_asian,
  hispanic = pr_hisp,
  american_indians_alaska_native = pr_aian,
  other = pr_other
)

fbisg_birdie_residuals <- residuals(birdie_fbisg_model) %>%
  as.data.frame() %>%
  dplyr::rename(
    residual = target
  )

fbisg_birdie <- cbind(fbisg_birdie, fbisg_birdie_residuals)

fbisg_birdie <- fbisg_birdie %>% dplyr::mutate(
  outcome_pred = target - residual
)

readr::write_csv(
  fbisg_birdie,
  file.path(OUTPUT, "fBISG_birdie", "predictions_fBISG_birdie.csv")
)

run_metrics(
  fbisg_birdie,
  c("nh_white", "nh_black", "asian", "hispanic", "other"),
  "race",
  "fBISG_birdie",
  OUTPUT,
  birdie = TRUE
)
