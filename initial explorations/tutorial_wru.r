# set the directory
setwd("~/Desktop/GRI/soa05_tutorial-main") 


# If on a Windows machine, must install RTools first:
# https://cran.r-project.org/bin/windows/Rtools/

# Install required packages
#renv::restore()

library(wru)
library(tidyverse)
source("metrics.R")

# TODO: set path to output directory
OUTPUT <- "C:~/Desktop/GRI"

# Request a Census API key here: https://api.census.gov/data/key_signup.html
# Save your key to your .Renviron file to avoid sharing it with other users
#usethis::edit_r_environ()
Sys.getenv("CENSUS_API_KEY")

# TODO: set path to tutorial data
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

# ----------------
# wru models
# ----------------

bisg <- predict_race(
  voter.file = df_no_territories,
  surname.only = FALSE,
  census.geo = "tract",
  age = FALSE,
  sex = FALSE,
  year = 2020,
  model = "BISG",
  census.key="29caaead12373544ff9fb7db17cb01532b2e2468"
)

bifsg <- predict_race(
  voter.file = df_no_territories,
  surname.only = FALSE,
  census.geo = "tract",
  age = FALSE,
  sex = FALSE,
  year = 2020,
  model = "BISG",
  names.to.use = "surname, first",
  census.key="29caaead12373544ff9fb7db17cb01532b2e2468"
  
)


# ----------------------
# metrics for BISG model
# ----------------------

# Rename to be consistent with other packages
bisg <- bisg %>% dplyr::rename(
  nh_white = pred.whi,
  nh_black = pred.bla,
  asian = pred.asi,
  hispanic = pred.his,
  other = pred.oth
)

run_metrics(
  bisg,
  c("nh_white", "nh_black", "asian", "hispanic", "other"),
  "race",
  "BISG",
  OUTPUT
)



# ----------------------
# metrics for BIFSG model
# ----------------------

bifsg <- bifsg %>% dplyr::rename(
  nh_white = pred.whi,
  nh_black = pred.bla,
  asian = pred.asi,
  hispanic = pred.his,
  other = pred.oth
)

run_metrics(
  bifsg,
  c("nh_white", "nh_black", "asian", "hispanic", "other"),
  "race",
  "BIFSG",
  OUTPUT
)
