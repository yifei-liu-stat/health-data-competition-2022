library(tidygeocoder)
library(tidyverse)

hospital_dat <- read_csv('data/hospital_characteristics.csv')
locs <- tibble(street = hospital_dat$Address,city = hospital_dat$City, 
               state = hospital_dat$State)

geo_dat <- locs %>%
  geocode(street = street ,city = city,
          state = state, 
          method = "census", verbose = TRUE)

write_csv(geo_dat, "data/geodata.csv")