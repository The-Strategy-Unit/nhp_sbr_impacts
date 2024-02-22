# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("dplyr", "janitor", "readxl", "stringr", "tidyr") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
#-----------------------------------------------------------------------------#
 #### Data Wrangling ####
  
  # Bed occupancy data
  tar_target(bed_occupancy_filepath, "Data/sql_bed_occupancy.csv", 
             format = "file"
             ),
  
  tar_target(
    formatted_bed_occupancy,
    wrangle_bed_occupancy(bed_occupancy_filepath)
  ), 
 
  # ERIC data
  tar_target(eric_09_15_filepath, "data/sql_eric_09_15.csv", format = "file"),
  tar_target(eric_16_23_filepath, "data/sql_eric_16_23.csv", format = "file"),
  
  tar_target(
    eric_udal,
    wrangle_eric(eric_09_15_filepath, eric_16_23_filepath)
  ),
  tar_target(
    eric_09,
    get_single_bedrooms_for_2009_10(2009)
  ),
  tar_target(
    eric_10,
    get_single_bedrooms_for_2009_10(2010)
  ),
  tar_target(
    formatted_eric,
    combine_eric_data(eric_udal, eric_09, eric_10)
  ),
  
 # SHMI data
 tar_target(shmi_filepath, "Data/sql_shmi.csv", format = "file"),
 
 tar_target(
   formatted_shmi,
   wrangle_shmi(shmi_filepath)
 ),
  
 # Turnover
 tar_target(turnover_filepath, "Data/sql_turnover.csv", format = "file"),
 
 tar_target(
   formatted_turnover,
   wrangle_turnover(turnover_filepath)
 ),
 
 # Workforce
 tar_target(workforce_udal_filepath, "Data/sql_workforce.csv", 
            format = "file"
            ),
 
 tar_target(workforce_udal, read.csv(workforce_udal_filepath)),
 
 tar_target(workforce_links_filepath, "Data/links_workforce.xlsx", 
            format = "file"
            ),
 tar_target(workforce_links, read_excel(workforce_links_filepath)),
 
 tar_target(formatted_workforce, 
            combine_workforce(workforce_udal, workforce_links)),
 
 # SUS various
 tar_target(beddays, 
            fun_load_beddays("Data/nhp_hospsite_beddays_mth.csv")),
 tar_target(sus_apcs, 
            fun_load_sus_apcs("Data/nhp_hospsite_apcs_mth.csv", beddays)),
 tar_target(sus_cost_yr, 
            fun_load_sus_cost_yr("Data/nhp_hospsite_costs_yr.csv")),
 tar_target(sus_cost_mth, 
            fun_load_sus_cost_mth("Data/nhp_hospsite_costs_mth.csv")),
 
 #-----------------------------------------------------------------------------#
 
 #### Selecting controls #### 
  
  # Finding similar matches for single bedrooms
  tar_target(
    available_beds,
    get_available_beds_by_organisation(formatted_bed_occupancy)
  ),
  tar_target(
    single_bedrooms,
    formatted_eric |>
      get_percentage_single_bedrooms(available_beds)
  ),
 
 
 tar_target(ref_org_sites_filepath,
            "data/ref_organisations_sites.csv"),
 
 tar_target(
   ref_org_sites,
   read.csv(ref_org_sites_filepath) |>
     clean_names() |>
     select(-last_refreshed)
 ) , 
 
 tar_target(cancer_centres_filepath,
            "data/ref_cancer_centres.csv"),
 
 tar_target(
   cancer_centres,
   get_cancer_centre_site_codes(cancer_centres_filepath)
 ) ,
 
 
 tar_target(
   clatterbridge_matches,
   get_clatterbridge_matches(single_bedrooms, ref_org_sites, cancer_centres)
 ) ,
 
 
 
 tar_target(cardiac_site_filepath,
            "data/ref_cardiac_sites.csv"
            ),
 
 tar_target(
   cardiac_sites,
   get_cardiac_site_codes(cardiac_site_filepath)
 ) ,
 
 tar_target(
   royal_papworth_matches,
   get_royal_papworth_matches(single_bedrooms, ref_org_sites, cardiac_sites) #######
 ) ,
 
 
 
 
 
 
 
 
  tar_target(
    single_bedroom_matches,
    combine_single_bedroom_matches(single_bedrooms, clatterbridge_matches, royal_papworth_matches, ref_org_sites)
  )
)
