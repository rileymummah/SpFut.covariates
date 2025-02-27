## code to prepare `locs` dataset goes here

library(tidyverse)
region <- readr::read_rds("../species-futures/data/species/PJOR/region-1e+05-FALSE_NA_NA_NA_NA.rds")

region$sp.grid %>%
  dplyr::select(-area, -sp.grid.id) %>%
  mutate(conus.grid.id = 1:n()) %>%
  rename('grid.id' = 'conus.grid.id') -> locs

plot(locs)


usethis::use_data(locs, overwrite = TRUE)

