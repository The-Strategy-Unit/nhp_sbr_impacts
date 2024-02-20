# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  
  # Finding similar matches for single bedrooms
  tar_target(
    name = available_beds,
    command = read.csv("data/formatted_bed_occupancy.csv") |>
      get_available_beds_by_organisation()
    # format = "qs" # Efficient storage for general data objects.
  ),
  tar_target(
    name = single_bedrooms,
    command = read.csv("data/formatted_eric.csv") |>
      get_percentage_single_bedrooms(available_beds)
  ),
  tar_target(
    name = single_bedroom_matches,
    command = combine_single_bedroom_matches(single_bedrooms)
  )
)
