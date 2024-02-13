# ERIC data

library(readxl)
library(dplyr)
library(stringr)

#### Data after 2010 ####
eric_after_2010 <- read_excel("data/ERIC.xlsx", 
                                   sheet = "data"
                                   ) |>
  calculate_single_bedrooms() |>
  mutate(value = as.numeric(value))
  

#### Getting the before 2010 data ####
eric_2008 <- get_eric_before_2010(2008)
eric_2009 <- get_eric_before_2010(2009)

#### Putting it all together ####
eric <- eric_after_2010 |>
  bind_rows(eric_2008,
            eric_2009
            ) |>
  mutate(site_type = str_replace_all(site_type, "[:digit:]. ", "") |> 
           str_to_lower(),
         organisation_type = str_to_lower(organisation_type)
  ) 
