# ERIC data

#### Setup ####
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)

#### Getting the UDAL data ####
eric_09_15 <- read.csv("data/sql_eric_09_15.csv") 

eric_16_23 <- read.csv("data/sql_eric_16_23.csv") 

eric_udal <- eric_09_15 |>
  bind_rows(eric_16_23) |>
  get_single_bedrooms() |>
  mutate(value = as.numeric(value),
         effective_snapshot_date = as.Date(effective_snapshot_date, 
                                           format = "%d/%m/%Y"
                                           )
         )

#### Getting single bedrooms for 2009 and 2010 ####
eric_09 <- get_single_bedrooms_for_2009_10(2009)
eric_10 <- get_single_bedrooms_for_2009_10(2010)

#### Putting it all together ####
eric <- eric_udal |>
  bind_rows(eric_09,
            eric_10
            ) |>
  mutate(site_type = str_replace_all(site_type, "[:digit:]. ", "") |> 
           str_to_lower(),
         organisation_type = str_to_lower(organisation_type)
  ) |>
  pivot_wider(names_from = "measure", values_from = "value") |>
  clean_names()

#### Checks ####
summary(eric)

# Quick plot to check what months we have and data quality:
library(plotly)
library(ggplot2)

ggplotly(
  eric |>
    # filter(organisation_code == "RTH") |> # can look at specific organisation
    summarise(total_single_bedrooms = sum(total_single_bedrooms, na.rm = TRUE), 
              .by = c(effective_snapshot_date)
    ) |>
    ggplot(aes(effective_snapshot_date, total_single_bedrooms)) +
    geom_line() +
    geom_point() +
    theme_bw() 
)

ggplotly(
  eric |>
    # filter(organisation_code == "RTH") |> # can look at specific organisation
    summarise(cleaning_staff_wte = sum(cleaning_staff_wte, na.rm = TRUE), 
              .by = c(effective_snapshot_date)
    ) |>
    ggplot(aes(effective_snapshot_date, cleaning_staff_wte)) +
    geom_line() +
    geom_point() +
    theme_bw() 
)

#### Saving output ####
write.csv(eric, "Data/formatted_eric.csv", row.names = FALSE)
