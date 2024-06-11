# Import packages
library(dplyr)
library(readr)
library(wru)
library(stringr)

# Load datasets to predict race

## Voter file with 12 obs.
data <- load("data/voters.rda")
str(voters)

## Data from SOA tutorial with 100,000 obs.
df <- readr::read_csv(
  "data/stat-methods-imputing-race-ethnicity-data.csv",
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

territories <- c("AS", "GU", "MP", "PR", "VI")

df <- df %>%
  dplyr::filter(!state %in% territories)

# Work with data from SOA

## Proportions
table(df$race)/nrow(df)*100
 
## Surname data : P(R|S)
load("data/surname/surnames2000.RData")
load("data/surname/surnames2010.RData")

sum(!(surnames2000$surname %in% surnames2010$surname))/nrow(surnames2000)*100
surnames2010[which(!(surnames2000$surname %in% surnames2010$surname)),]
sum(!(surnames2010$surname %in% surnames2000$surname))/nrow(surnames2010)*100

# Surname only (default: using surname data from 2020)
bis  <- predict_race(voter.file = df, surname.only = TRUE)
str(bis)
df_surnames <- unique(bis$surname)
sum(!(df_surnames %in% c(surnames2010$surname, surnames2000$surname)))
which(is.na(bis$pred.oth))
which(is.na(bis), arr.ind=TRUE)
bis$surname <- toupper(bis$surname)
df_surnames <- unique(bis$surname)
sum(!(df_surnames %in% c(surnames2010$surname, surnames2000$surname)))

bis_join_surname <- bis %>% 
  left_join(
    surnames2010,
    by="surname"
    ) %>% 
  left_join(
    surnames2000,
    by="surname"
  )
#str(bis_join_surname)

nrow(bis_join_surname %>% 
  filter(
    pred.whi != p_whi.x,
    pred.bla != p_bla.x,
    pred.his != p_his.x,
    pred.asi != p_asi.x,
    pred.oth != p_oth.x
  ))

nrow(bis_join_surname %>% 
  filter(
    pred.whi != p_whi.y,
    pred.bla != p_bla.y,
    pred.his != p_his.y,
    pred.asi != p_asi.y,
    pred.oth != p_oth.y
  ))

bis_DOAN <- bis_join_surname %>% 
  filter(
    surname=="DOAN"
  ) 
#%>% distinct()

bis  <- predict_race(voter.file = df, surname.only = TRUE, year="2010")
bis$surname <- toupper(bis$surname)
bis_join_surname <- bis %>% 
  left_join(
    surnames2010,
    by="surname"
  )

# Surname only
res <- predict_race(voter.file = df, surname.only = TRUE)
save(res, file = "output/results/bisg/BISG_surname.rda")

# BISG results for all possible variables and geolocation
name_variables <- c("surname", "surname, first", "surname, first, middle")
for (elt in name_variables){
  res <- predict_race(
    voter.file = df, 
    census.geo = "tract", 
    names.to.use = elt,
    census.key="29caaead12373544ff9fb7db17cb01532b2e2468"
    )
  save(res, 
       file = str_c("output/results/bisg/BISG_tract_", elt, ".rda", 
                         sep="")
       )
}

