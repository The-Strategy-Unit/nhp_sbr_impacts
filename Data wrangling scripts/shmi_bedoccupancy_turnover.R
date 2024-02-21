# SHMI, bed occupancy and turnover.

# This script has different sections to wrangle the data gathered from sql and 
  # save for later use. Uses functions from data_wrangling_functions.R.

#### Setup ####
library(dplyr)
library(tidyr)

#### Wrangle shmi ####
# shmi <- read_sql_output("Data/sql_shmi.csv") |>
#   mutate(across(c(shmi_value, observed, expected, spells), as.numeric)) |>
#   rename("organisation_code" = provider_code)

#### Checks for shmi ####
summary(shmi)

# Quick plot to check what months we have and data quality:
library(plotly)
library(ggplot2)

ggplotly(
  shmi |>
    filter(level == "Trust") |> # or "Site"
    pivot_longer(names_to = "measure", 
                 values_to = "value", 
                 cols = c("shmi_value", "observed", "expected", "spells")
                 ) |>
    # filter(organisation_code == "RTH") |> # can look at specific organisation
    summarise(value = sum(value, na.rm = TRUE), 
              .by = c(effective_snapshot_date, measure)
              ) |>
    ggplot(aes(effective_snapshot_date, value, col = measure)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    facet_wrap(~measure, scales = "free", nrow = 4)
  )

#### Saving shmi output ####
#write.csv(shmi, "Data/formatted_shmi.csv", row.names = FALSE)

#-----------------------------------------------------------------------------#

#### Wrangle bed occupancy ####
# bed_occupancy <- read_sql_output("Data/sql_bed_occupancy.csv") |>
#   mutate(bed_occupancy = as.numeric(bed_occupancy))

#### Checks for bed occupancy ####
summary(bed_occupancy)

# Quick plot to check what months we have and data quality:
ggplotly(
  bed_occupancy |>
    pivot_longer(names_to = "measure", 
                 values_to = "value", 
                 cols = c("available", "occupied", "bed_occupancy")
                 ) |>
    # filter(organisation_code == "RTH") |> # can look at specific organisation
    summarise(value = sum(value, na.rm = TRUE), 
              .by = c(effective_snapshot_date, measure)
              ) |>
    ggplot(aes(effective_snapshot_date, value, col = measure)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    facet_wrap(~measure, scales = "free", nrow = 4)
  )

#### Saving bed occupancy output ####
#write.csv(bed_occupancy, "Data/formatted_bed_occupancy.csv", row.names = FALSE)

#-----------------------------------------------------------------------------#

#### Wrangle turnover ####
turnover <- read_sql_output("Data/sql_turnover.csv") |>
  rename("organisation_code" = org_code,
         "turnover_headcount" = head_count,
         "turnover_fte" = fte
         ) |>
  mutate(staff_group = standardise_staff_group(staff_group))

#### Checks for turnover ####
summary(turnover)

# Quick plot to check what months we have and data quality:
ggplotly(
  turnover |>
    summarise(headcount = sum(turnover_headcount, na.rm = TRUE), 
              .by = c(effective_snapshot_date, type)
              ) |>
    ggplot(aes(effective_snapshot_date, headcount, col = type)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    facet_wrap(~type, scales = "free", nrow = 4)
  )

#### Saving shmi output ####
write.csv(turnover, "Data/formatted_turnover.csv", row.names = FALSE)