library(dplyr)
library(tibble)
library(tidycensus)
library(stringr)
library(tidyr)

# ----------------------
# Get data at county level
# ----------------------  

census_api_key("29caaead12373544ff9fb7db17cb01532b2e2468")

# Load variables for 2019 ACS at the county level
variables <- load_variables(2019, "acs5", cache = TRUE)
race_variables <- variables[grep("race", variables$label, ignore.case = TRUE), ]
print(race_variables,n = 34)

# Get data on race from the 2019 American Community Survey (ACS) at the county level
vars <- c("C02003_003", "C02003_004", "C02003_005", "C02003_006", "C02003_007", "C02003_008")
estados <-  c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
# Data 
raw_census <- get_acs(geography = "county", 
                      variables = vars, 
                      year = 2020)


raw_census_df <- as.data.frame(raw_census)

#  
census <- pivot_wider(raw_census_df, 
                      names_from = "variable", 
                      values_from = c("estimate", "moe"))

# calculate probabilities
census <- census %>%
  mutate(
    r_whi = (estimate_C02003_003 +moe_C02003_003)/sum(census[3:14]),
    r_bla = (estimate_C02003_004 + moe_C02003_004)/sum(census[3:14]),
    r_his = (estimate_C02003_005+moe_C02003_005)/sum(census[3:14]),
    r_asi = (estimate_C02003_006+moe_C02003_006)/sum(census[3:14]),
    r_oth = (estimate_C02003_007 + estimate_C02003_008 +moe_C02003_007+moe_C02003_008)/sum(census[3:14])
  )

n_whi <- sum(census$r_whi)
n_bla <- sum(census$r_bla)
n_his <- sum(census$r_his)
n_asi <- sum(census$r_asi)
n_oth <- sum(census$r_oth)

census <- census %>%
  mutate(
    r_whi = r_whi/n_whi ,
    r_bla = r_bla/n_bla,
    r_his = r_his/n_his,
    r_asi = r_asi/n_asi,
    r_oth = r_oth/n_oth 
  )

save(census, file = "census_county.rda")


# ----------------------
# Get data at tractlevel
# ----------------------  


library(dplyr)
library(tibble)
library(tidycensus)
library(stringr)
library(tidyr)

vars_dec2020 <- c("P2_002N","P1_003N", "P1_004N", "P1_005N", "P1_006N", "P1_007N", "P1_008N")
estados <-  c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
# Data 
census_api_key("29caaead12373544ff9fb7db17cb01532b2e2468")
census_data2020 = get_decennial(
  geography = "tract",
  state = estados,
  variables = vars_dec2020, 
  year = 2020,
  sumfile = "pl"
)
decennial_2020_vars <- load_variables(
  year = 2020, 
  "pl", 
  cache = TRUE
)

d<-decennial_2020_vars[grep("hispanic", decennial_2020_vars$label, ignore.case = TRUE), ]
print(d,n = 334)
#usamos este

raw_census_df <- as.data.frame(census_data2020)

#  
census <- pivot_wider(raw_census_df, 
                      names_from = "variable", 
                      values_from = "value")

# P1_003N White                                                          
# P1_004N black                                     
# P1_005N American Indian and Alaska Native alone"                                
# P1_006N asian                                                          
# P1_007N Native Hawaiian and Other Pacific Islander alone"                     
# P1_008N Some Other Race alone"  
# P2_002N hispanic" 

# calculate probabilities
#p(G_i| R_i )=p(R_i | G_i) *P( G_i)/P( R_i)=#counts_for_race_j_in_geo_i/#counts_all_races_in_geo*#counts_all_races_in_geo/#count_in_total/#count_total_races_j/#count_in_total
census <- census %>%
  mutate(
    r_whi = (P1_003N)/sum(census$P1_003N),
    r_bla = (P1_004N)/sum(census$P1_004N),
    r_his = (P2_002N)/sum(census$P2_002N),
    r_asi = (P1_006N)/sum(census$P1_006N),
    r_oth = (P1_005N+P1_007N+P1_008N)/sum(census[c("P1_005N", "P1_007N", "P1_008N")])
  )
# we follow hierarchical: white -> asia -> balck -> hispanic -> other. The order matters and probabilities depends on the order 
census <- census %>%
  mutate(
    r_nonwhi  = (P1_004N+P2_002N+P1_006N+P1_005N+P1_007N+P1_008N)/sum(census[c("P1_004N","P2_002N","P1_006N","P1_005N","P1_007N","P1_008N")]),
    r_nonasi = (P1_004N+P2_002N+P1_005N+P1_007N+P1_008N)/sum(census[c("P1_004N","P2_002N","P1_005N","P1_007N","P1_008N")]),
    r_nonbla = (P2_002N+P1_005N+P1_007N+P1_008N)/sum(census[c("P2_002N","P1_005N","P1_007N","P1_008N")]),
    r_nonhis = (P1_005N+P1_007N+P1_008N)/sum(census[c("P1_005N","P1_007N","P1_008N")])
  )

save(census, file = "census_tract_decennial2020.rda")





# 2016 - 2020 5 Year American Community Survey (ACS) Variables
acs_20_vars = load_variables(
  year = 2020, 
  "acs5",
  cache = TRUE
)
acs_20_vars[grep("race", acs_20_vars$label, ignore.case = TRUE), ]
print(race_variables,n = 134)


# 2010 Decennial Census Variables
decennial_2010_vars <- load_variables(
  year = 2010, 
  "pl", 
  cache = TRUE
)
decennial_2010_vars[grep("race", decennial_2010_vars$label, ignore.case = TRUE), ]
vars_dec <- c("P001003", "P001004", "P001005", "P001006", "P001007", "P001008")
estados <-  c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
# Data 
census_data2010 = get_decennial(
  geography = "tract",
  state = estados,
  variables = vars_dec, 
  year = 2010,
  sumfile = "pl"
)
save(census_data2010, file = "census_tract_decennial2010.rda")
