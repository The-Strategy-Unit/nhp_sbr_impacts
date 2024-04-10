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
    "egg",
    "flextable",
    "geomtextpath",
    "ggplot2",
    "htmltools",
    "janitor",
    "leaflet",
    "lubridate",
    "MarketMatching",
    "oceanis",
    "patchwork",
    "readxl",
    "sf",
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
  tar_target(sus_los,
             fun_load_sus_los("Data/nhp_sbr_los.csv")),
  tar_target(sus_falls,
             fun_load_sus_falls("Data/nhp_sbr_falls.csv")),
  tar_target(sus_ages,
             fun_load_sus_ages("Data/nhp_sbr_admit_age.csv")),
  tar_target(sus_deaths,
             fun_load_sus_deaths("Data/nhp_sbr_deaths.csv")),
  tar_target(ods_sites,
             fun_load_ods_sites("Data/ods_geocoded.csv")),
  
  # rtt waiting times
  tar_target(
    formatted_rtt_data,
    rtt_data_formatting(
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/rtt_waiting_time_data.csv"
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
  
  #Staff survey
  tar_target(
    formatted_staff_survey_data,
    staff_survey_formatting(
      "Data/staff_survey_2008.csv",
      "Data/staff_survey_2009.csv",
      "Data/staff_survey_2010.csv",
      "Data/staff_survey_2011.csv",
      "Data/staff_survey_2012.csv",
      "Data/staff_survey_2013.csv",
      "Data/staff_survey_2014.csv",
      "Data/staff_survey_2015.csv",
      "Data/staff_survey_2016.csv",
      "Data/staff_survey_2017.csv",
      "Data/staff_survey_2018.csv",
      "Data/staff_survey_2019.csv",
      "Data/staff_survey_2020.csv",
      "Data/staff_survey_2021.csv",
      "Data/staff_survey_2022.csv",
      "Data/staff_survey_2023.csv"
    )
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
  
  ## adding data on floor space:
  tar_target(floor_space,
             get_floor_space_by_site(formatted_eric)),
  
  tar_target(
    floor_space_matches,
    combine_floor_space_matches(floor_space)
  ),
  
  # adding floor space matches to control df
  tar_target(
    single_bedroom_matches_2,
    control_stage2(single_bedroom_matches, floor_space_matches)
  ),
  
  ## adding data on median ages:
  tar_target(med_age,
             get_med_age_by_site("Data/nhp_sbr_admit_age.csv")),
  
  tar_target(med_age_matches,
             combine_med_age_matches(med_age)),
  
  # adding med age matches to control df
  tar_target(
    single_bedroom_matches_3,
    control_stage3(single_bedroom_matches_2, med_age_matches)
  ),
  
  # ranking the secondary matching variables
  tar_target(
    single_bedroom_matches_final,
    ranking_control_var(single_bedroom_matches_3)
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
  tar_target(
    sus_deaths_cia_format,
    sus_deaths_cia_formatting(sus_deaths)
  ),
  
  
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
  
  # staff survey
  tar_target(
    staff_survey_cia_format,
    staff_survey_cia_formatting(formatted_staff_survey_data)
  ),
  
  
  # Single bed rooms
  tar_target(
    sbr_percent_cia_format,
    sbr_percent_cia_formatting(single_bedrooms)
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
      "switch_month" = c(
        as.Date("2022-10-01"),
        as.Date("2020-06-01"),
        as.Date("2019-05-01"),
        as.Date("2010-11-01"),
        as.Date("2018-09-01"),
        as.Date("2014-05-01"),
        as.Date("2011-01-01")
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_bed_occupancy,
      get_indicator_organisation_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        bed_occupancy_cia_format,
        "bed_occupancy",
        "quarter"
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_staff_turnover,
      get_indicator_organisation_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        staff_turnover_cia_format,
        "leaving_rate",
        "month"
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_staff_sickness,
      get_indicator_organisation_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        staff_sickness_cia_format,
        "staff_sickness_percent",
        "month"
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_hcai,
      get_indicator_organisation_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        hcai_cia_format,
        "combined_rate",
        "month"
      )
    )
  ),
  
  
  # site code plots
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_cleaning_staff,
      get_indicator_site_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        cleaning_staff_cia_format,
        "cleaning_staff_wte",
        "year"
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_cleaning_costs,
      get_indicator_site_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        cleaning_costs_cia_format,
        "cleaning_service_cost",
        "year"
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_friends_and_family,
      get_indicator_site_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        friends_and_family_cia_format,
        "friends_and_family_percent",
        "month"
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_falls_and_fractures,
      get_indicator_site_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        falls_and_fractures_cia_format,
        "ff_rate",
        "month"
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_sus_deaths,
      get_indicator_site_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        sus_deaths_cia_format,
        "hosp_rate_1000",
        "month"
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_rtt_waiting_time,
      get_indicator_organisation_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        rtt_waiting_time_cia_format,
        "median_by_prov",
        "month"
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_length_of_stay,
      get_indicator_site_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        length_of_stay_cia_format,
        "avg_los",
        "month"
      )
    )
    
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_sbr_percent,
      get_indicator_organisation_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        sbr_percent_cia_format,
        "percentage_single_bedrooms",
        "year"
      )
    )
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      plot_emergency_readmissions,
      get_indicator_site_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        emergency_readmissions_cia_format,
        "perc",
        "month"
      )
    )
  ),
  
  #----------------------------------------------------------------------------#
  #### Maps ####
  
  tar_target(
    map_all_sites,
    map_all("Map of SBR intervention sites",
            hospitals,
            ods_sites)
  ),
  tarchetypes::tar_map(
    list(
      org_code_of_interest = c("REM",
                               "REN",
                               "RGM",
                               "RGN",
                               "RAL",
                               "RVJ",
                               "RWF")
    ),
    tar_target(
      map,
      map_controls(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        ods_sites
      )
    )
  ),
  
  #-----------------------------------------------------------------------------#
  #### Indicator availability plots ####
  
  #Indicator availability over time
  tar_target(
    indicator_availability_plot,
    indicator_availability_over_time(
      friends_and_family_cia_format,
      staff_turnover_cia_format,
      staff_sickness_cia_format,
      staff_survey_cia_format,
      hcai_cia_format,
      falls_and_fractures_cia_format,
      sus_deaths_cia_format,
      rtt_waiting_time_cia_format,
      bed_occupancy_cia_format,
      length_of_stay_cia_format,
      emergency_readmissions_cia_format
    )
  ),
  
  #Indicator availability by site table
  tar_target(
    indicator_availability_table,
    indicator_availability_by_site(
      friends_and_family_cia_format,
      staff_turnover_cia_format,
      staff_sickness_cia_format,
      staff_survey_cia_format,
      hcai_cia_format,
      falls_and_fractures_cia_format,
      sus_deaths_cia_format,
      rtt_waiting_time_cia_format,
      bed_occupancy_cia_format,
      length_of_stay_cia_format,
      emergency_readmissions_cia_format
    )
  ),
  
  #-----------------------------------------------------------------------------#
  #### CIA models ####
  
  #Waiting time- median
  tar_target(
    waiting_time_median_REM,
    cia_analysis(
      "REM",
      rtt_waiting_time_cia_format,
      "median_by_prov",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    waiting_time_median_REN,
    cia_analysis(
      "REN",
      rtt_waiting_time_cia_format,
      "median_by_prov",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    waiting_time_median_RGM,
    cia_analysis(
      "RGM",
      rtt_waiting_time_cia_format,
      "median_by_prov",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    waiting_time_median_RGN,
    cia_analysis(
      "RGN",
      rtt_waiting_time_cia_format,
      "median_by_prov",
      0.01,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    waiting_time_median_RAL,
    cia_analysis(
      "RAL",
      rtt_waiting_time_cia_format,
      "median_by_prov",
      0.01,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  
  tar_target(
    waiting_time_median_RVJ,
    cia_analysis(
      "RVJ",
      rtt_waiting_time_cia_format,
      "median_by_prov",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    waiting_time_median_RWF,
    cia_analysis(
      "RWF",
      rtt_waiting_time_cia_format,
      "median_by_prov",
      0.01,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  
  #Waiting time -number
  
  tar_target(
    waiting_time_number_REM,
    cia_analysis(
      "REM",
      rtt_waiting_time_cia_format,
      "number_incomplete",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
 tar_target(
    waiting_time_number_REN,
    cia_analysis("REN",
                rtt_waiting_time_cia_format,
                "number_incomplete",
                0.1,
                 single_bedroom_matches_final,
  hospitals)
  ),
  
  tar_target(
    waiting_time_number_RGM,
    cia_analysis(
      "RGM",
      rtt_waiting_time_cia_format,
      "number_incomplete",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    waiting_time_number_RGN,
    cia_analysis(
      "RGN",
      rtt_waiting_time_cia_format,
      "number_incomplete",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    waiting_time_number_RAL,
    cia_analysis(
      "RAL",
      rtt_waiting_time_cia_format,
      "number_incomplete",
      0.01,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  
  tar_target(
    waiting_time_number_RVJ,
    cia_analysis(
      "RVJ",
      rtt_waiting_time_cia_format,
      "number_incomplete",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    waiting_time_number_RWF,
    cia_analysis(
      "RWF",
      rtt_waiting_time_cia_format,
      "number_incomplete",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  #Length of Stay
  
  tar_target(
    LoS_REM,
    cia_analysis(
      "REM",
      length_of_stay_cia_format,
      "avg_los",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    LoS_REN,
    cia_analysis(
      "REN",
      length_of_stay_cia_format,
      "avg_los",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    LoS_RGM,
    cia_analysis(
      "RGM",
      length_of_stay_cia_format,
      "avg_los",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    LoS_RGN,
    cia_analysis(
      "RGN",
      length_of_stay_cia_format,
      "avg_los",
      0.01,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    LoS_RAL,
    cia_analysis(
      "RAL",
      length_of_stay_cia_format,
      "avg_los",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  
  tar_target(
    LoS_RVJ,
    cia_analysis(
      "RVJ",
      length_of_stay_cia_format,
      "avg_los",
      0.01,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    LoS_RWF,
    cia_analysis(
      "RWF",
      length_of_stay_cia_format,
      "avg_los",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  #Emergency Readmissions
  
  tar_target(
    emergency_readmissions_REM,
    cia_analysis(
      "REM",
      emergency_readmissions_cia_format,
      "perc",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    emergency_readmissions_REN,
    cia_analysis(
      "REN",
      emergency_readmissions_cia_format,
      "perc",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    emergency_readmissions_RGM,
    cia_analysis(
      "RGM",
      emergency_readmissions_cia_format,
      "perc",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    emergency_readmissions_RGN,
    cia_analysis(
      "RGN",
      emergency_readmissions_cia_format,
      "perc",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    emergency_readmissions_RAL,
    cia_analysis(
      "RAL",
      emergency_readmissions_cia_format,
      "perc",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  
  tar_target(
    emergency_readmissions_RVJ,
    cia_analysis(
      "RVJ",
      emergency_readmissions_cia_format,
      "perc",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    emergency_readmissions_RWF,
    cia_analysis(
      "RWF",
      emergency_readmissions_cia_format,
      "perc",
      0.01,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  #Bed Occupancy
  
  tar_target(
    bed_occupancy_REM,
    cia_analysis(
      "REM",
      bed_occupancy_cia_format,
      "bed_occupancy",
      0.01,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    bed_occupancy_REN,
    cia_analysis(
      "REN",
      bed_occupancy_cia_format,
      "bed_occupancy",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    bed_occupancy_RGM,
    cia_analysis(
      "RGM",
      bed_occupancy_cia_format,
      "bed_occupancy",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  
  tar_target(
    bed_occupancy_RAL,
    cia_analysis(
      "RAL",
      bed_occupancy_cia_format,
      "bed_occupancy",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  
  tar_target(
    bed_occupancy_RVJ,
    cia_analysis(
      "RVJ",
      bed_occupancy_cia_format,
      "bed_occupancy",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  
  # Healthcare acquired infections
  
  tar_target(
    hcai_REM,
    cia_analysis(
      "REM",
      hcai_cia_format,
      "combined_rate",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    hcai_REN,
    cia_analysis(
      "REN",
      hcai_cia_format,
      "combined_rate",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  # Falls and Fractures in hospital
  tar_target(
    falls_and_fractures_REM,
    cia_analysis(
      "REM",
      falls_and_fractures_cia_format,
      "ff_rate",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    falls_and_fractures_REN,
    cia_analysis(
      "REN",
      falls_and_fractures_cia_format,
      "ff_rate",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    falls_and_fractures_RGM,
    cia_analysis(
      "RGM",
      falls_and_fractures_cia_format,
      "ff_rate",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    falls_and_fractures_RGN,
    cia_analysis(
      "RGN",
      falls_and_fractures_cia_format,
      "ff_rate",
      0.01,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    falls_and_fractures_RAL,
    cia_analysis(
      "RAL",
      falls_and_fractures_cia_format,
      "ff_rate",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  
  tar_target(
    falls_and_fractures_RVJ,
    cia_analysis(
      "RVJ",
      falls_and_fractures_cia_format,
      "ff_rate",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    falls_and_fractures_RWF,
    cia_analysis(
      "RWF",
      falls_and_fractures_cia_format,
      "ff_rate",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  
  # SUS deaths in hospital
  tar_target(
    sus_deaths_REM,
    cia_analysis(
      "REM",
      sus_deaths_cia_format,
      "hosp_rate_1000",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    sus_deaths_REN,
    cia_analysis(
      "REN",
      sus_deaths_cia_format,
      "hosp_rate_1000",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    sus_deaths_RGM,
    cia_analysis(
      "RGM",
      sus_deaths_cia_format,
      "hosp_rate_1000",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    sus_deaths_RGN,
    cia_analysis(
      "RGN",
      sus_deaths_cia_format,
      "hosp_rate_1000",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    sus_deaths_RAL,
    cia_analysis(
      "RAL",
      sus_deaths_cia_format,
      "hosp_rate_1000",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  
  tar_target(
    sus_deaths_RVJ,
    cia_analysis(
      "RVJ",
      sus_deaths_cia_format,
      "hosp_rate_1000",
      0.01,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    sus_deaths_RWF,
    cia_analysis(
      "RWF",
      sus_deaths_cia_format,
      "hosp_rate_1000",
      0.01,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  # Friends and family test
  tar_target(
    friends_and_family_REM,
    cia_analysis(
      "REM",
      friends_and_family_cia_format,
      "friends_and_family_percent",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    friends_and_family_REN,
    cia_analysis(
      "REN",
      friends_and_family_cia_format,
      "friends_and_family_percent",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    friends_and_family_RGM,
    cia_analysis(
      "RGM",
      friends_and_family_cia_format,
      "friends_and_family_percent",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    friends_and_family_RAL,
    cia_analysis(
      "RAL",
      friends_and_family_cia_format,
      "friends_and_family_percent",
      0.01,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  #Staff sickness
  
  tar_target(
    staff_sickness_REM,
    cia_analysis(
      "REM",
      staff_sickness_cia_format,
      "staff_sickness_percent",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    staff_sickness_REN,
    cia_analysis(
      "REN",
      staff_sickness_cia_format,
      "staff_sickness_percent",
      0.1,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    staff_sickness_RGM,
    cia_analysis(
      "RGM",
      staff_sickness_cia_format,
      "staff_sickness_percent",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  
  tar_target(
    staff_sickness_RAL,
    cia_analysis(
      "RAL",
      staff_sickness_cia_format,
      "staff_sickness_percent",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
  
  tar_target(
    staff_sickness_RVJ,
    cia_analysis(
      "RVJ",
      staff_sickness_cia_format,
      "staff_sickness_percent",
      0.05,
      single_bedroom_matches_final,
      hospitals
    )
  ),
 
 # Staff turnover
 
 tar_target(
   staff_turnover_REM,
   cia_analysis(
     "REM",
     staff_turnover_cia_format,
     "leaving_rate",
     0.1,
     single_bedroom_matches_final,
     hospitals
   )
 ),
 
 
 #Staff survey
 
prior_sd_staff_survey <- tibble::tribble(
   ~org_code_of_interest, ~prior_sd,
    "REM", 0.01,
    "REN", 0.01,
    "RGM", 0.01,
    "RAL", 0.01,
    "RVJ", 0.01
 ),
 
 list(
   tarchetypes::tar_map(
     values = prior_sd_staff_survey,
     names = org_code_of_interest,
     targets::tar_target(staff_survey,
                         cia_analysis(
                           org_code_of_interest,
                           staff_survey_cia_format,
                           "positive_responses",
                           prior_sd,
                           single_bedroom_matches_final,
                           hospitals
                         )
                         )
   )
 ),
 

# Cleaning costs

prior_sd_cleaning_costs <- tibble::tribble(
  ~org_code_of_interest, ~prior_sd,
  "REM", 0.01,
  "REN", 0.01,
  "RGM", 0.01,
  "RAL", 0.01
),

list(
  tarchetypes::tar_map(
    values = prior_sd_cleaning_costs,
    names = org_code_of_interest,
    targets::tar_target(cleaning_costs,
                        cia_analysis(
                          org_code_of_interest,
                          cleaning_costs_cia_format,
                          "cleaning_service_cost",
                          prior_sd,
                          single_bedroom_matches_final,
                          hospitals
                        )
    )
  )
),

  
  #------------------------------------------------------------------------------#
  #### Presenting CIA outputs ####
  
  tar_target(
    waiting_time_median_output,
    model_output(
      waiting_time_median_REM,
      waiting_time_median_REN,
      waiting_time_median_RGM,
      waiting_time_median_RGN,
      NA,
      NA,
      waiting_time_median_RWF
    )
  ),
  
  tar_target(
    waiting_time_number_output,
    model_output(
      waiting_time_number_REM,
      NA,
      waiting_time_number_RGM,
      waiting_time_number_RGN,
      NA,
      NA,
      waiting_time_number_RWF
    )
  ),
  
  tar_target(
    LoS_output,
    model_output(LoS_REM,
                 LoS_REN,
                 NA,
                 LoS_RGN,
                 LoS_RAL,
                 LoS_RVJ,
                 LoS_RWF)
  ),
  
  tar_target(
    emergency_readmissions_output,
    model_output(
      emergency_readmissions_REM,
      emergency_readmissions_REN,
      emergency_readmissions_RGM,
      emergency_readmissions_RGN,
      emergency_readmissions_RAL,
      emergency_readmissions_RVJ,
      emergency_readmissions_RWF
    )
  ),
  
  tar_target(
    bed_occupancy_output,
    model_output(
      bed_occupancy_REM,
      bed_occupancy_REN,
      NA,
      NA,
      bed_occupancy_RAL,
      bed_occupancy_RVJ,
      NA
    )
  ),
  
  tar_target(hcai_output,
             model_output(hcai_REM,
                          NA,
                          NA,
                          NA,
                          NA,
                          NA,
                          NA)),
  
  tar_target(
    falls_and_fractures_output,
    model_output(
      falls_and_fractures_REM,
      NA,
      NA,
      falls_and_fractures_RGN,
      NA,
      falls_and_fractures_RVJ,
      falls_and_fractures_RWF
    )
  ),
  
  
  tar_target(
    sus_deaths_output,
    model_output(
      sus_deaths_REM,
      sus_deaths_REN,
      NA,
      sus_deaths_RGN,
      NA,
      sus_deaths_RVJ,
      sus_deaths_RWF
    )
  ),
  
  tar_target(
    friends_and_family_output,
    model_output(
      friends_and_family_REM,
      friends_and_family_REN,
      friends_and_family_RGM,
      NA,
      friends_and_family_RAL,
      NA,
      NA
    )
  ),
  
  tar_target(
    staff_sickness_output,
    model_output(
      staff_sickness_REM,
      staff_sickness_REN,
      staff_sickness_RGM,
      NA,
      staff_sickness_RAL,
      staff_sickness_RVJ,
      NA
    )
  ),

tar_target(
  staff_survey_output,
  model_output(
    staff_survey_REM,
    staff_survey_REN,
    staff_survey_RGM,
    NA,
    staff_survey_RAL,
    staff_survey_RVJ,
    NA
  )
),
  
tar_target(
  cleaning_costs_output,
  model_output(
    cleaning_costs_REM,
    cleaning_costs_REN,
    cleaning_costs_RGM,
    NA,
    cleaning_costs_RAL,
    NA,
    NA
  )
)


  
)
