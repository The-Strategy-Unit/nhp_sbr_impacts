# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    "dplyr",
    "geomtextpath",
    "ggplot2",
    "janitor",
    "lubridate",
    "readxl",
    "stringr",
    "tidyr",
    "tsibble",
    "zoo"
  ) # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  #----------------------------------------------------------------------------#
  #### Data Wrangling ####
  
  # Bed occupancy data
  tar_target(
    bed_occupancy_filepath,
    "Data/sql_bed_occupancy.csv",
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
  tar_target(eric_09,
             get_single_bedrooms_for_2009_10(2009)),
  tar_target(eric_10,
             get_single_bedrooms_for_2009_10(2010)),
  tar_target(
    formatted_eric,
    combine_eric_data(eric_udal, eric_09, eric_10)
  ),
  
  # SHMI data
  tar_target(shmi_filepath, "Data/sql_shmi.csv", format = "file"),
  
  tar_target(formatted_shmi,
             wrangle_shmi(shmi_filepath)),
  
  # Turnover
  tar_target(turnover_filepath, "Data/sql_turnover.csv", format = "file"),
  
  tar_target(formatted_turnover,
             wrangle_turnover(turnover_filepath)),
  
  # Workforce
  tar_target(workforce_udal_filepath, "Data/sql_workforce.csv",
             format = "file"),
  
  tar_target(workforce_udal, read.csv(workforce_udal_filepath)),
  
  tar_target(
    workforce_links_filepath,
    "Data/links_workforce.xlsx",
    format = "file"
  ),
  tar_target(workforce_links, read_excel(workforce_links_filepath)),
  
  tar_target(
    formatted_workforce,
    combine_workforce(workforce_udal, workforce_links)
  ),
  
  # SUS various
  tar_target(
    beddays,
    fun_load_beddays("Data/nhp_hospsite_beddays_mth.csv")
  ),
  tar_target(
    sus_apcs,
    fun_load_sus_apcs("Data/nhp_hospsite_apcs_mth.csv", beddays)
  ),
  tar_target(
    sus_cost_yr,
    fun_load_sus_cost_yr("Data/nhp_hospsite_costs_yr.csv")
  ),
  tar_target(
    sus_cost_mth,
    fun_load_sus_cost_mth("Data/nhp_hospsite_costs_mth.csv")
  ),
  tar_target(
    sus_readmit,
    fun_load_sus_readmit("Data/nhp_sbr_readmit.csv")
  ),
  tar_target(
    sus_los,
    fun_load_sus_los("Data/nhp_sbr_los.csv")
  ),
  tar_target(
    sus_falls,
    fun_load_sus_falls("Data/nhp_sbr_falls.csv")
  ),
  tar_target(
    sus_ages,
    fun_load_sus_ages("Data/nhp_sbr_admit_age.csv")
  ),
  tar_target(
    sus_deaths,
    fun_load_sus_deaths("Data/nhp_sbr_deaths.csv")
  ),
  
  # rtt waiting times
  tar_target(
    formatted_rtt_data,
    rtt_data_formatting(
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/rtt_waiting_times.csv"
    )
  ),
  
  #Friends and family test scores
  tar_target(
    formatted_friends_and_family_data,
    friends_and_family_scores_data_formatting(
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/friends_and_family_inpatient_scores.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/friend_and_family_inpatient_scores_post_Jul2022.csv"
    )
  ),
  
  #Healthcare acquired infections
  tar_target(
    formatted_HCAI_data,
    hcai_formatting(
      "Data/hai_cdiff_pre_2018.csv",
      "Data/hai_cdiff.csv",
      "Data/hai_ecoli.csv",
      "Data/hai_klebsiella.csv",
      "Data/hai_mssa.csv",
      "Data/hai_mrsa.csv",
      "Data/hai_p_aeruginosa.csv"
    )
  ),
  
  # Staff sickness
  tar_target(
    formatted_staff_sickness_absence,
    staff_sickness_absence_formatting("Data/staff_sickness_absence.csv")
  ),
  
  #----------------------------------------------------------------------------#
  
  
  #### Selecting controls ####
  
  # Finding similar matches for single bedrooms and accounting for cancer and
  # cardiac sites,
  tar_target(
    available_beds,
    get_available_beds_by_organisation(formatted_bed_occupancy)
  ),
  
  tar_target(
    single_bedrooms,
    formatted_eric |>
      get_percentage_single_bedrooms(available_beds)
  ),
  
  # to add in org/site names
  tar_target(ref_org_sites_filepath,
             "data/ref_organisations_sites.csv"),
  
  tar_target(
    ref_org_sites,
    read.csv(ref_org_sites_filepath) |>
      clean_names() |>
      select(-last_refreshed)
  ) ,
  
  # clatterbridge should be matched to cancer centres:
  tar_target(cancer_centres_filepath,
             "data/ref_cancer_centres.csv"),
  
  tar_target(
    cancer_centres,
    get_cancer_centre_site_codes(cancer_centres_filepath)
  ) ,
  
  # royal papworth should be matched to cardiac sites:
  tar_target(cardiac_site_filepath,
             "data/ref_cardiac_sites.csv"),
  
  tar_target(
    cardiac_sites,
    get_cardiac_site_codes(cardiac_site_filepath)
  ) ,
  
  # other hospitals should be matched to general acute hospitals:
  tar_target(
    general_acute_sites,
    get_general_acute_site_codes(formatted_eric)
  ) ,
  
  # all single bedroom matches accounting for cancer and cardiac filters:
  tar_target(
    single_bedroom_matches,
    combine_single_bedroom_matches(
      single_bedrooms,
      ref_org_sites,
      general_acute_sites,
      cancer_centres,
      cardiac_sites
    )
  ),
  
  #----------------------------------------------------------------------------#
  
  #### Causal Impact Analysis additional formatting
  
  # Friends and Family test additional formatting
  tar_target(
    friends_and_family_cia_format,
    friends_and_family_cia_formatting(formatted_friends_and_family_data)
  ),
  
  # Staff turnover - Nurses
  tar_target(
    staff_turnover_cia_format,
    staff_turnover_cia_formatting(formatted_turnover)
  ),
  
  # Staff sickness additional formatting
  tar_target(
    staff_sickness_cia_format,
    staff_sickness_cia_formatting(formatted_staff_sickness_absence)
  ),
  
  
  # Healthcare acquired infections additional formatting
  tar_target(
    hcai_cia_format,
    hcai_cia_formatting(beddays, formatted_HCAI_data)
  ),
  
  
  #Falls and fractures additional formatting
  tar_target(
    falls_and_fractures_cia_format,
    falls_and_fractures_cia_formatting(sus_falls)
  ),
  
  # Hospital death rate additional formatting
  tar_target(sus_deaths_cia_format,
             sus_deaths_cia_formatting(sus_deaths)),
  
  
  #rtt waiting time additional formatting
  tar_target(
    rtt_waiting_time_cia_format,
    rtt_waiting_time_cia_formatting(formatted_rtt_data)
  ),
  
  # Bed occupancy additional formatting
  tar_target(
    bed_occupancy_cia_format,
    bed_occupancy_cia_formatting(formatted_bed_occupancy)
  ),
  
  # Length of Stay additional formatting
  tar_target(
    length_of_stay_cia_format,
    length_of_stay_cia_formatting(sus_los)
  ),
  
  # Cleaning staff additional formatting
  tar_target(
    cleaning_staff_cia_format,
    cleaning_staff_cia_formatting(formatted_eric)
  ),
  
  # Cleaning costs additional formatting
  tar_target(
    cleaning_costs_cia_format,
    cleaning_costs_cia_formatting(formatted_eric)
  ),
  
  # Emergency readmissions
  tar_target(
    emergency_readmissions_cia_format,
    emergency_readmissions_cia_formatting(sus_readmit)
  ),
  
  #----------------------------------------------------------------------------#
  #### Standard charts ####
  tar_target(
    hospitals,
    data.frame(
      "name" = c(
        "Royal Liverpool",
        "Clatterbridge Cancer Centre",
        "Royal Papworth",
        "Peterborough (district) Hospital",
        "Chase Farm Hospital",
        "Southmead Hospital",
        "Tunbridge Wells"
      ),
      "alias" = c(
        "royal_liverpool",
        "clatterbridge",
        "royal_papworth",
        "peterborough",
        "chase_farm",
        "southmead",
        "tunbridge_wells"
      ),
      "organisation_code" = c("REM",
                              "REN",
                              "RGM",
                              "RGN",
                              "RAL",
                              "RVJ",
                              "RWF"),
      "site_code" = c("REMRQ",
                      "REN22",
                      "RGM22",
                      "RGN80",
                      "RALC7",
                      "RVJ01",
                      "RWFTW"),
      "date_sbr" = c(
        as.Date("2022-10-31"),
        as.Date("2022-06-30"),
        as.Date("2019-05-31"),
        as.Date("2010-11-30"),
        as.Date("2018-09-30"),
        as.Date("2014-05-30"),
        as.Date("2011-01-31")
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      hospital_of_interest = c(
        "royal_liverpool",
        "clatterbridge",
        "royal_papworth",
        "peterborough",
        "chase_farm",
        "southmead",
        "tunbridge_wells"
      )
    ),
    tar_target(
      plot_bed_occupancy,
      get_indicator_organisation_level_plot(
        hospital_of_interest,
        hospitals,
        single_bedroom_matches,
        bed_occupancy_cia_format,
        "bed_occupancy",
        "quarter"
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      hospital_of_interest = c(
        "royal_liverpool",
        "clatterbridge",
        "royal_papworth",
        "peterborough",
        "chase_farm",
        "southmead",
        "tunbridge_wells"
      )
    ),
    tar_target(
      plot_staff_turnover,
      get_indicator_organisation_level_plot(
        hospital_of_interest,
        hospitals,
        single_bedroom_matches,
        staff_turnover_cia_format,
        "leaving_rate",
        "month"
      )
    )
  ),
tarchetypes::tar_map(
  list(
    hospital_of_interest = c(
      "royal_liverpool",
      "clatterbridge",
      "royal_papworth",
      "peterborough",
      "chase_farm",
      "southmead",
      "tunbridge_wells"
    )
  ),
  tar_target(
    plot_staff_sickness,
    get_indicator_organisation_level_plot(
      hospital_of_interest,
      hospitals,
      single_bedroom_matches,
      staff_sickness_cia_format,
      "staff_sickness_percent",
      "month"
    )
  )
),
  tarchetypes::tar_map(
    list(
      hospital_of_interest = c(
        "royal_liverpool",
        "clatterbridge",
        "royal_papworth",
        "peterborough",
        "chase_farm",
        "southmead",
        "tunbridge_wells"
      )
    ),
    tar_target(
      plot_hcai,
      get_indicator_organisation_level_plot(
        hospital_of_interest,
        hospitals,
        single_bedroom_matches,
        hcai_cia_format,
        "combined_rate",
        "month"
      )
    )
  ),


  # site code plots
tarchetypes::tar_map(
  list(
    hospital_of_interest = c(
      "royal_liverpool",
      "clatterbridge",
      "royal_papworth",
      "peterborough",
      "chase_farm",
      "southmead",
      "tunbridge_wells"
    )
  ),
  tar_target(
    plot_cleaning_staff,
    get_indicator_site_level_plot(
      hospital_of_interest,
      hospitals,
      single_bedroom_matches,
      cleaning_staff_cia_format,
      "cleaning_staff_wte",
      "year"
    )
  )
),
tarchetypes::tar_map(
  list(
    hospital_of_interest = c(
      "royal_liverpool",
      "clatterbridge",
      "royal_papworth",
      "peterborough",
      "chase_farm",
      "southmead",
      "tunbridge_wells"
    )
  ),
  tar_target(
    plot_cleaning_costs,
    get_indicator_site_level_plot(
      hospital_of_interest,
      hospitals,
      single_bedroom_matches,
      cleaning_costs_cia_format,
      "cleaning_service_cost",
      "year"
    )
  )
),
tarchetypes::tar_map(
  list(
    hospital_of_interest = c(
      "royal_liverpool",
      "clatterbridge",
      "royal_papworth",
      "peterborough",
      "chase_farm",
      "southmead",
      "tunbridge_wells"
    )
  ),
  tar_target(
    plot_friends_and_family,
    get_indicator_site_level_plot(
      hospital_of_interest,
      hospitals,
      single_bedroom_matches,
      friends_and_family_cia_format,
      "friends_and_family_percent",
      "month"
    )
  )
),
tarchetypes::tar_map(
  list(
    hospital_of_interest = c(
      "royal_liverpool",
      "clatterbridge",
      "royal_papworth",
      "peterborough",
      "chase_farm",
      "southmead",
      "tunbridge_wells"
    )
  ),
  tar_target(
    plot_falls_and_fractures,
    get_indicator_site_level_plot(
      hospital_of_interest,
      hospitals,
      single_bedroom_matches,
      falls_and_fractures_cia_format,
      "ff_rate",
      "month"
    )
  )
),
tarchetypes::tar_map(
  list(
    hospital_of_interest = c(
      "royal_liverpool",
      "clatterbridge",
      "royal_papworth",
      "peterborough",
      "chase_farm",
      "southmead",
      "tunbridge_wells"
    )
  ),
  tar_target(
    plot_sus_deaths,
    get_indicator_site_level_plot(
      hospital_of_interest,
      hospitals,
      single_bedroom_matches,
      sus_deaths_cia_format,
      "death_rate",
      "month"
    )
  )
),
  tarchetypes::tar_map(
    list(
      hospital_of_interest = c(
        "royal_liverpool",
        "clatterbridge",
        "royal_papworth",
        "peterborough",
        "chase_farm",
        "southmead",
        "tunbridge_wells"
      )
    ),
    tar_target(
      plot_rtt_waiting_time,
      get_indicator_organisation_level_plot(
        hospital_of_interest,
        hospitals,
        single_bedroom_matches,
        rtt_waiting_time_cia_format,
        "median_by_prov",
        "month"
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      hospital_of_interest = c(
        "royal_liverpool",
        "clatterbridge",
        "royal_papworth",
        "peterborough",
        "chase_farm",
        "southmead",
        "tunbridge_wells"
      )
    ),
    tar_target(
      plot_length_of_stay,
      get_indicator_site_level_plot(
        hospital_of_interest,
        hospitals,
        single_bedroom_matches,
        length_of_stay_cia_format,
        "avg_los",
        "month"
      )
    )
    
  )
  
)
