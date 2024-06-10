library(dplyr)
library(tibble)
library(tidycensus)
library(stringr)
library(tidyr)

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
    r_whi = (estimate_C02003_003 +moe_C02003_003)/rowSums(wide_df[3:14]),
    r_bla = (estimate_C02003_004 + moe_C02003_004)/rowSums(wide_df[3:14]),
    r_his = (estimate_C02003_005+moe_C02003_005)/rowSums(wide_df[3:14]),
    r_asi = (estimate_C02003_006+moe_C02003_006)/rowSums(wide_df[3:14]),
    r_oth = (estimate_C02003_007 + estimate_C02003_008 +moe_C02003_007+moe_C02003_008)/rowSums(wide_df[3:14])
  )

save(census, file = "census_county.rda")
