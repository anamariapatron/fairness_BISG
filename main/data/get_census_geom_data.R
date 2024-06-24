library(dplyr)
library(tibble)
library(tidycensus)
library(stringr)
library(tidyr)
library(sf)

census_api_key("29caaead12373544ff9fb7db17cb01532b2e2468")

### ############
###tract#######
### #############


# 2020 Decennial Census Variables
decennial_2020_vars <- load_variables(
  year = 2020, 
  "pl", 
  cache = TRUE
)
# P1_003N White                                                          
# P1_004N black                                     
# P1_005N American Indian and Alaska Native alone"                                
# P1_006N asian                                                          
# P1_007N Native Hawaiian and Other Pacific Islander alone"                     
# P1_008N Some Other Race alone"  
# P2_002N hispanic" 

vars_dec2020 <- c("P2_002N","P1_003N", "P1_004N", "P1_005N", "P1_006N", "P1_007N", "P1_008N")
estados <-  c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
# Data 
census_data2020 = get_decennial(
  geography = "tract",
  state = estados,
  variables = vars_dec2020, 
  year = 2020,
  sumfile = "pl",
  geometry = TRUE
)

# P1_003N White                                                          
# P1_004N black                                     
# P1_005N American Indian and Alaska Native alone"                                
# P1_006N asian                                                          
# P1_007N Native Hawaiian and Other Pacific Islander alone"                     
# P1_008N Some Other Race alone"  
# P2_002N hispanic" 

#  
sf_census <- pivot_wider(census_data2020,
                         names_from = "variable", 
                         values_from = "value")

# Save the shapefile
st_write(sf_census, "data/geolocation/census_tract_decennial2020_geom.shp")

# Read the shapefile
sf_census <- st_read("data/geolocation/census_tract_decennial2020_geom.shp")

# Playing with sf
#plot(sf_census) # All variables are plotted

# Plotting number of white people
plot(sf_census["P1_003N"])