# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("dplyr", "janitor", "stringr", "tidyr") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  
  # Bed occupancy data
  tar_target(
    name = formatted_bed_occupancy,
    command = get_bed_occupancy_from_udal("Data/sql_bed_occupancy.csv")
  ), 
  
  # ERIC data
  tar_target(
    name = eric_udal,
    command = get_eric_data_from_udal("data/sql_eric_09_15.csv", "data/sql_eric_16_23.csv")
  ),
  tar_target(
    name = eric_09,
    command = get_single_bedrooms_for_2009_10(2009)
  ),
  tar_target(
    name = eric_10,
    command = get_single_bedrooms_for_2009_10(2010)
  ),
  tar_target(
    name = formatted_eric,
    command = combine_eric_data(eric_udal, eric_09, eric_10)
  ),
  
  
  
  
  
  
  # Finding similar matches for single bedrooms
  tar_target(
    name = available_beds,
    command = get_available_beds_by_organisation(formatted_bed_occupancy)
  ),
  tar_target(
    name = single_bedrooms,
    command = formatted_eric |>
      get_percentage_single_bedrooms(available_beds)
  ),
  tar_target(
    name = single_bedroom_matches,
    command = combine_single_bedroom_matches(single_bedrooms)
  )
)
