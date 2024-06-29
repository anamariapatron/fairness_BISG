# Import packages
library(dplyr)
library(readr)
library(wru)
library(stringr)
library(caret)
library(purrr)
library(pROC)
library(tidyr)

# Load census and surname datasets

## Contains counts by tract and race: N_rg and P(G|R) calculated by Ana
load("data/geolocation/census_tract_decennial2020.rda")
census <- census_final
str(census)

## Contains P(R|S)
load("data/surname/last_nameRaceProbs.rData")
surnames <- last_nameRaceProbs
surnames <- surnames %>% rename(
  surname = name,
  p_whi = whi,
  p_bla = bla,
  p_his = his,
  p_asi = asi,
  p_oth = oth
)
str(surnames)

## Contains P(S|R)
load("data/surname/last_raceNameProbs.rData")
surnames_inv <- last_raceNameProbs
surnames_inv <- surnames_inv %>% rename(
  surname = name,
  p_inv_whi = whi,
  p_inv_bla = bla,
  p_inv_his = his,
  p_inv_asi = asi,
  p_inv_oth = oth
)
str(surnames_inv)

# Apply the methodology on a subsample first: voters file
# Voters file with BISG surname + tract prediction (with wru package)
pred_surname_tract <- read.csv2("output/results/BISG_surname_tract.csv")

# Select one county: Alabama
census_al <- census %>% filter(substr(GEOID, 1, 2) == "01")
bisg_al <- pred_surname_tract %>% filter(state == "AL")
bisg_al <- bisg_al %>% rename(white_bisg_wru = nh_white,
                              black_bisg_wru = nh_black,
                              hispa_bisg_wru = hispanic,
                              asian_bisg_wru = asian,
                              other_bisg_wru = other)

zero_counts <- bisg_al %>% filter(
  white_bisg_wru == 0 | black_bisg_wru == 0 | hispa_bisg_wru == 0 | asian_bisg_wru == 0 | other_bisg_wru == 0
)

nrow(zero_counts)/nrow(bisg_al)*100 # almost 11% of zero counts

# Implementing BISG without wru
## P(R|S)
bisg_al <- bisg_al %>% mutate(surname = toupper(surname))

bisg_al <- bisg_al %>% left_join(surnames, by="surname") # P(R|S)
bisg_al <- bisg_al %>% left_join(surnames_inv, by="surname") # P(S|R)
which(is.na(bisg_al), arr.ind = TRUE)
to_delete <- unique(which(is.na(bisg_al), arr.ind = TRUE)[,1]) # 15 obs. to delete
bisg_al <- bisg_al[-to_delete,] # 1486 obs.

census_al <- census_al %>% mutate(GEOID_tract = as.numeric(GEOID))
bisg_al <- bisg_al %>% left_join(census_al %>% select(
  GEOID_tract, r_whi, r_bla, r_his, r_asi, r_oth),
  by="GEOID_tract") # P(G|R)
bisg_al <- bisg_al %>% mutate(white_bisg_ = p_whi*r_whi,
                              black_bisg_ = p_bla*r_bla,
                              hispa_bisg_ = p_his*r_his,
                              asian_bisg_ = p_asi*r_asi,
                              other_bisg_ = p_oth*r_oth)
# Normalizing probabilities
n_race <- rowSums(bisg_al %>% select(white_bisg_, 
                                     black_bisg_,
                                     hispa_bisg_,
                                     asian_bisg_,
                                     other_bisg_))
bisg_al <- bisg_al %>% mutate(n_race = n_race)
bisg_al <- bisg_al %>% mutate(white_bisg = white_bisg_/n_race,
                              black_bisg = black_bisg_/n_race,
                              hispa_bisg = hispa_bisg_/n_race,
                              asian_bisg = asian_bisg_/n_race,
                              other_bisg = other_bisg_/n_race)
bisg_al <- bisg_al[,-c(37:42)] # not the same probabilities than with BISG wru...

# Implementing fully bayesian
## Initialization
iter <- 5
burnin <- iter%/%2
### n_rg
# Computes the maximum probability and predicted race obtained with BISG (without using wru)
bisg_al <- bisg_al %>%
  rowwise() %>%
  mutate(
    max_bisg = max(white_bisg, black_bisg, hispa_bisg, asian_bisg, other_bisg),
    pred_bisg = case_when(
      white_bisg == max_bisg ~ "white_bisg",
      black_bisg == max_bisg ~ "black_bisg",
      hispa_bisg == max_bisg ~ "hispa_bisg",
      asian_bisg == max_bisg ~ "asian_bisg",
      other_bisg == max_bisg ~ "other_bisg",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

# Group observations by unicity for surname and tract
n_rg <- bisg_al %>% 
  select(
    surname,
    GEOID_tract,
    tract,
    pred_bisg
  ) %>%
  group_by(
    surname,
    GEOID_tract,
    tract,
    pred_bisg
  ) %>%
  summarise(
    seen = n()
  )

# Pivot the table to get counts by surname and tract
n_rg <- n_rg %>%
  pivot_wider(
    names_from = pred_bisg, 
    values_from = seen
  )
n_rg[is.na(n_rg)] <- 0

# Final table with counts by tract
n_rg <- n_rg %>%
  group_by(
    GEOID_tract,
    tract
  ) %>%
  summarise(
    init_white = sum(white_bisg),
    init_black = sum(black_bisg),
    init_hispa = sum(hispa_bisg),
    init_asian = sum(asian_bisg),
    init_other = sum(other_bisg)
  )
# Number of zero counts obtained with BISG (without using wru)
length(unique(which(n_rg==0,arr.ind=TRUE)[,1])) # 368 zero counts

# Number of ethnic groups and observations
bisg_al <- bisg_al %>%
  mutate(
    race_fb = case_when(
      race == "asian" ~ "asian",
      race == "hispanic" ~ "hispa",
      race == "nh_black" ~ "black",
      race == "nh_white" ~ "white",
      race == "other" ~ "other",
      TRUE ~ "F"
    )
  )
races <- unique(bisg_al$race_fb)
J <- length(unique(bisg_al$race_fb))
# Number of records in voters file
N <- nrow(bisg_al)
# Output matrix with fBISG predicted probabilities
pred_matrix <- matrix(data=0, nrow=N, ncol=J)
colnames(pred_matrix) <- races

# N_rg (obtained with census data at tract level)
census_al <- census_al %>%
  mutate(N_other = P1_005N+P1_007N+P1_008N,
         N_white = P1_003N,
         N_black = P1_004N,
         N_asian = P1_006N,
         N_hispa = P2_002N
  )

m <- 1
i <- 1
j <- 1

for (m in 1:iter){
  for (i in 1:N){
    r_i <- substr(bisg_al$pred_bisg[i], 1, 5)
    g_i <- bisg_al$GEOID_tract[i]
    n_rg[which(n_rg$GEOID_tract == g_i), paste0("init_", r_i)] <- 
      n_rg[which(n_rg$GEOID_tract == g_i), paste0("init_", r_i)] - 1
    probs_i <- rep(0, J)
    for (j in 1:J){
      r_j <- races[j]
      probs_i[j] <- as.numeric(
        log(
          n_rg[which(n_rg$GEOID_tract == g_i), paste0("init_", r_j)] + 
            census_al[which(census_al$GEOID_tract == g_i), paste0("N_", r_j)] +
            1
        )
      )
      probs_i[j] <- as.numeric(
        probs_i[j] + log(
          bisg_al[i, paste0("p_inv_", substr(r_j, 1, 3))] + 
            1e-8
        )
      )
    }
    probs_i <- probs_i - max(probs_i)
    probs_i <- exp(probs_i)
    probs_i <- probs_i/sum(probs_i)
    #probs_i <- cumsum(probs_i)
    # Sample a new race
    new_race <- sample(races, size=1, prob=probs_i)
    bisg_al$race_fb[i] <- new_race
    n_rg[which(n_rg$GEOID_tract == g_i), paste0("init_", new_race)] <- 
      n_rg[which(n_rg$GEOID_tract == g_i), paste0("init_", new_race)] + 1
    if (m >= burnin) {
      pred_matrix[i, which(races == new_race)] <- 
        pred_matrix[i, which(races == new_race)] +
        1
    }
  }
}