library(tidyverse)
library(tidycensus)
library(purrr)

# Population Data

census_api_key("02f3fa45f183f3343f69234d1f329a2c3cb294c8")

us <- unique(fips_codes$state)[1:51]

r11totalpop <- map_df(us, function(x) {
  get_acs(geography = "county", variables = "B01003_001", 
          state = x)
})

cny_pop <- read.csv("11-counties.csv")

cny_pop$GEOID <- as.character(cny_pop$GEOID)

sso_t_pop <- left_join(cny_pop, r11totalpop, by = "GEOID")

sso_sum <- aggregate(sso_t_pop$estimate, by = list(mlrassoarea = sso_t_pop$mlrassoarea), FUN = sum)
