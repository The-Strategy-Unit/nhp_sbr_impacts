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
    "DiagrammeR",
    "egg",
    "flextable",
    "forcats",
    "geomtextpath",
    "ggplot2",
    "htmltools",
    "imputeTS",
    "janitor",
    "kableExtra",
    "knitr",
    "leaflet",
    "lubridate",
    "MarketMatching",
    "metafor",
    "oceanis",
    "patchwork",
    "readxl",
    "sf",
    "stringr",
    "StrategyUnitTheme",
    "tidyr",
    "tsibble",
    "zoo",
    "gridExtra"
  ), # Packages that your targets need for their tasks.
  error = "continue" # continue running rest of pipeline if errors.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  #----------------------------------------------------------------------------#
  #### Data Wrangling ####
  
  # SBR hospitals
  tar_target(hospitals_filepath,
             "hospitals_summary.xlsx",
             format = "file"),
  
  tar_target(hospitals,
             get_hospitals_info(hospitals_filepath)),
  
  # Bed occupancy data
  tar_target(
    bed_occupancy_filepath,
    "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/sql_bed_occupancy.csv",
    format = "file"
  ),
  
  tar_target(
    formatted_bed_occupancy,
    wrangle_bed_occupancy(bed_occupancy_filepath)
  ),
  
  # ERIC data
  tar_target(eric_09_15_filepath, "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/sql_eric_09_15.csv", format = "file"),
  tar_target(eric_16_25_filepath, "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/sql_eric_16_25.csv", format = "file"),
  
  tar_target(
    eric_udal,
    wrangle_eric(eric_09_15_filepath, eric_16_25_filepath)
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
  #tar_target(shmi_filepath, "Data/sql_shmi.csv", format = "file"),
  
 # tar_target(formatted_shmi,
  #           wrangle_shmi(shmi_filepath)),
  
  # Turnover
  tar_target(turnover_filepath, "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/sql_turnover.csv", format = "file"),
  
  tar_target(formatted_turnover,
             wrangle_turnover(turnover_filepath)),
  
  # Workforce
  tar_target(workforce_udal_filepath, "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/sql_workforce.csv",
             format = "file"),
  
  tar_target(workforce_udal, read.csv(workforce_udal_filepath)),
  
  tar_target(
    workforce_links_filepath,
    "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/links_workforce.xlsx",
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
    fun_load_beddays("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/nhp_hospsite_beddays_mth.csv")
  ),
  tar_target(
    sus_apcs,
    fun_load_sus_apcs("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/nhp_hospsite_apcs_mth.csv", beddays)
  ),
  tar_target(
    sus_cost_yr,
    fun_load_sus_cost_yr("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/nhp_hospsite_costs_yr.csv")
  ),
  tar_target(
    sus_cost_mth,
    fun_load_sus_cost_mth("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/nhp_hospsite_costs_mth.csv")
  ),
  tar_target(
    sus_readmit,
    fun_load_sus_readmit("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/nhp_sbr_readmit.csv")
  ),
  tar_target(sus_los,
             fun_load_sus_los("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/nhp_sbr_los.csv")),
  tar_target(sus_falls,
             fun_load_sus_falls("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/nhp_sbr_falls.csv")),
  tar_target(sus_ages,
             fun_load_sus_ages("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/nhp_sbr_admit_age.csv")),
  tar_target(sus_deaths,
             fun_load_sus_deaths("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/nhp_sbr_deaths.csv")),
  tar_target(ods_sites,
             fun_load_ods_sites("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/ods_geocoded.csv")),
  
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
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/friends_and_family_inpatient_scores_post_Jul2022.csv"
    )
  ),
  
  #Healthcare acquired infections
  tar_target(
    formatted_HCAI_data,
    hcai_formatting(
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/hai_cdiff_pre_2018.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/hai_cdiff.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/hai_ecoli.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/hai_klebsiella.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/hai_mssa.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/hai_mrsa.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/hai_p_aeruginosa.csv"
    )
  ),
  
  # Staff sickness
  tar_target(
    formatted_staff_sickness_absence,
    staff_sickness_absence_formatting("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_sickness_absence.csv")
  ),
  
  #Staff survey
  tar_target(
    formatted_staff_survey_data,
    staff_survey_formatting(
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2008.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2009.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2010.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2011.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2012.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2013.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2014.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2015.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2016.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2017.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2018.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2019.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2020.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2021.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2022.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2023.csv",
      "Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff_survey_2024.csv"
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
             get_med_age_by_site("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/nhp_sbr_admit_age.csv")),
  
  tar_target(med_age_matches,
             combine_med_age_matches(med_age)),
  
  # adding med age matches to control df
  tar_target(
    single_bedroom_matches_3,
    control_stage3(single_bedroom_matches_2, med_age_matches)
  ),
  
  ## adding data on elective ratio:
  tar_target(
    elec_ratio,
    get_elec_ratio_by_site("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/nhp_sbr_bedmix.csv")
  ),
  
  tar_target(elec_ratio_matches,
             combine_elec_ratio_matches(elec_ratio)),
  
  # adding elective ratio matches to control df
  tar_target(
    single_bedroom_matches_4,
    control_stage4(single_bedroom_matches_3, elec_ratio_matches)
  ),
  
  # ranking the secondary matching variables
  tar_target(
    single_bedroom_matches_final,
    ranking_control_var(single_bedroom_matches_4)
  ),
  
  tar_target(control_pool,
             get_controls(single_bedroom_matches_final)),
  tarchetypes::tar_map(
    list(
      organisation_of_interest = c("REM",
                                   "REN",
                                   "RGM",
                                   "RGN",
                                   "RAL",
                                   "RVJ",
                                   "RWF")
    ),
    tar_target(
      controls_table,
      format_controls_table(control_pool, organisation_of_interest)
    )
  ),
  
  #----------------------------------------------------------------------------#
  #### Causal Impact Analysis additional formatting ####
  
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
  
  tar_target(
    cdiff_cia_format,
    cdiff_cia_formatting(beddays, formatted_HCAI_data)
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
  
  # Indicator information
  tar_target(
    indicator_info,
    get_indicator_info("indicator_summary.xlsx", "new")
  ),
  
  # Indicator information table output
  tar_target(
    indicator_info_table,
    format_indicator_table(indicator_info)
  ),
  
  #----------------------------------------------------------------------------#
  #### Standard charts ####
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
      plot_staff_survey,
      get_indicator_organisation_level_plot(
        org_code_of_interest,
        hospitals,
        single_bedroom_matches_final,
        staff_survey_cia_format,
        "positive_responses",
        "year"
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
  
  #----------------------------------------------------------------------------#
  #### Indicator availability plots ####
  
  #Indicator availability over time
  tar_target(
    indicator_availability_plot,
    indicator_availability_over_time(
      friends_and_family_cia_format,
      staff_turnover_cia_format,
      staff_sickness_cia_format,
      staff_survey_cia_format,
      cdiff_cia_format,
      falls_and_fractures_cia_format,
      sus_deaths_cia_format,
      rtt_waiting_time_cia_format,
      bed_occupancy_cia_format,
     length_of_stay_cia_format,
      emergency_readmissions_cia_format,
      cleaning_costs_cia_format
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
      cdiff_cia_format,
      falls_and_fractures_cia_format,
      sus_deaths_cia_format,
      rtt_waiting_time_cia_format,
      bed_occupancy_cia_format,
      length_of_stay_cia_format,
      emergency_readmissions_cia_format,
      cleaning_costs_cia_format
    )
  ),
  
  #----------------------------------------------------------------------------#
  #### CIA models ####
  
  #Waiting time- median
  
  prior_sd_waiting_time_median <- tibble::tribble(
    ~ org_code_of_interest,
    ~ prior_sd,
    "REM",
    0.05,
    "REN",
    0.05,
    "RGM",
    0.05,
    "RGN",
    0.01,
    "RVJ",
    0.1,
    "RAL",
    0.01,
    "RWF",
    0.01
  ),
  
  list(
    tarchetypes::tar_map(
      values = prior_sd_waiting_time_median,
      names = org_code_of_interest,
      targets::tar_target(
        waiting_time_median,
        cia_analysis(
          org_code_of_interest,
          rtt_waiting_time_cia_format,
          "median_by_prov",
          prior_sd,
          control_pool,
          hospitals
        )
      )
    )
  ),
  
  
  #Waiting time- median DTA
  
  prior_sd_waiting_time_median_dta <- tibble::tribble(
    ~ org_code_of_interest,
    ~ prior_sd,
    "REM",
    0.05,
    "REN",
    0.01,
    "RGM",
    0.05,
    "RAL",
    0.01
  ),
  
  list(
    tarchetypes::tar_map(
      values = prior_sd_waiting_time_median_dta,
      names = org_code_of_interest,
      targets::tar_target(
        waiting_time_median_dta,
        cia_analysis(
          org_code_of_interest,
          rtt_waiting_time_cia_format,
          "median_by_prov_dta",
          prior_sd,
          control_pool,
          hospitals
        )
      )
    )
  ),
  
  
  
  
  #Waiting time -number
  # prior_sd_waiting_time_number <- tibble::tribble(
  #   ~ org_code_of_interest,
  #   ~ prior_sd,
  #   "REM",
  #   0.1,
  #   "REN",
  #   0.1,
  #   "RGM",
  #   0.05,
  #   "RGN",
  #   0.1,
  #   "RVJ",
  #   0.1,
  #   "RAL",
  #   0.01,
  #   "RWF",
  #   0.1
  # ),
  # 
  # list(
  #   tarchetypes::tar_map(
  #     values = prior_sd_waiting_time_number,
  #     names = org_code_of_interest,
  #     targets::tar_target(
  #       waiting_time_number,
  #       cia_analysis(
  #         org_code_of_interest,
  #         rtt_waiting_time_cia_format,
  #         "number_incomplete",
  #         prior_sd,
  #         control_pool,
  #         hospitals
  #       )
  #     )
  #   )
  # ),
  
  
  #Length of Stay
  prior_sd_LoS <- tibble::tribble(
    ~ org_code_of_interest,
    ~ prior_sd,
    "REM",
    0.05,
    "REN",
    0.1,
    "RGM",
    0.05,
    "RGN",
    0.01,
    "RVJ",
    0.01,
    "RAL",
    0.1,
    "RWF",
    0.05
  ),
  
  list(
    tarchetypes::tar_map(
      values = prior_sd_LoS,
      names = org_code_of_interest,
      targets::tar_target(
        LoS,
        cia_analysis(
          org_code_of_interest,
          length_of_stay_cia_format,
          "avg_los",
          prior_sd,
          control_pool,
          hospitals
        )
      )
    )
  ),
  
  #Emergency Readmissions
  prior_sd_emergency_readmissions <- tibble::tribble(
    ~ org_code_of_interest,
    ~ prior_sd,
    "REM",
    0.1,
    "REN",
    0.1,
    "RGM",
    0.1,
    "RGN",
    0.1,
    "RVJ",
    0.05,
    "RAL",
    0.01,
    "RWF",
    0.01
  ),
  
  list(
    tarchetypes::tar_map(
      values = prior_sd_emergency_readmissions,
      names = org_code_of_interest,
      targets::tar_target(
        emergency_readmissions,
        cia_analysis(
          org_code_of_interest,
          emergency_readmissions_cia_format,
          "perc",
          prior_sd,
          control_pool,
          hospitals
        )
      )
    )
  ),
  
  
  #Bed Occupancy
  prior_sd_bed_occupancy <- tibble::tribble(
    ~ org_code_of_interest,
    ~ prior_sd,
    "REM",
    0.05,
    "REN",
    0.1,
    "RGM",
    0.05,
    "RVJ",
    0.05,
    "RAL",
    0.1,
    
  ),
  
  list(
    tarchetypes::tar_map(
      values = prior_sd_bed_occupancy,
      names = org_code_of_interest,
      targets::tar_target(
        bed_occupancy,
        cia_analysis(
          org_code_of_interest,
          bed_occupancy_cia_format|>filter(occupied!=0),
          "bed_occupancy",
          prior_sd,
          control_pool,
          hospitals
        )
      )
    )
  ),
  
  
  # Healthcare acquired infections
  
  tar_target(
    hcai_REM,
    cia_analysis(
      "REM",
      hcai_cia_format|>
        filter(!is.nan(combined_rate) & !is.infinite(combined_rate)),
      "combined_rate",
      0.1,
      control_pool,
      hospitals
    )
  ),
  
  tar_target(
    hcai_REN,
    cia_analysis(
      "REN",
      hcai_cia_format|>
        filter(!is.nan(combined_rate) & !is.infinite(combined_rate)),
      "combined_rate",
     0.05,
     single_bedroom_matches_final,
      hospitals
    )
  ),
  
  # C.Difficile
  prior_sd_cdiff <- tibble::tribble(
    ~ org_code_of_interest,
    ~ prior_sd,
    "REM",
    0.1,
    "RVJ",
    0.001,
    "RAL",
    0.01
    
  ),
  
  list(
    tarchetypes::tar_map(
      values = prior_sd_cdiff,
      names = org_code_of_interest,
      targets::tar_target(
        cdiff,
        cia_analysis(
          org_code_of_interest,
          cdiff_cia_format,
          "rate",
          prior_sd,
          control_pool,
          hospitals
        )
      )
    )
  ),
  
  
  # Falls and Fractures in hospital
  prior_sd_falls_and_fractures <- tibble::tribble(
    ~ org_code_of_interest,
    ~ prior_sd,
    "REM",
    0.05,
    "REN",
    0.05,
    "RGM",
    0.05,
    "RGN",
    0.01,
    "RVJ",
    0.05,
    "RAL",
    0.05,
    "RWF",
    0.05
    
  ),
  
  list(
    tarchetypes::tar_map(
      values = prior_sd_falls_and_fractures,
      names = org_code_of_interest,
      targets::tar_target(
        falls_and_fractures,
        cia_analysis(
          org_code_of_interest,
          falls_and_fractures_cia_format,
          "ff_rate",
          prior_sd,
          control_pool,
          hospitals
        )
      )
    )
  ),
  
  
  
  # SUS deaths in hospital
  prior_sd_sus_deaths <- tibble::tribble(
    ~ org_code_of_interest,
    ~ prior_sd,
    "REM",
    0.05,
    "REN",
    0.1,
    "RGM",
    0.05,
    "RGN",
    0.1,
    "RVJ",
    0.01,
    "RAL",
    0.05,
    "RWF",
    0.01
    
  ),
  
  list(
    tarchetypes::tar_map(
      values = prior_sd_sus_deaths,
      names = org_code_of_interest,
      targets::tar_target(
        sus_deaths,
        cia_analysis(
          org_code_of_interest,
          sus_deaths_cia_format,
          "hosp_rate_1000",
          prior_sd,
          control_pool,
          hospitals
        )
      )
    )
  ),
  
  
  # Friends and family test
  prior_sd_friends_and_family <- tibble::tribble(
    ~ org_code_of_interest,
    ~ prior_sd,
    "REM",
    0.05,
    "REN",
    0.1,
    "RGM",
    0.05,
    "RAL",
    0.01,
    "RVJ",
    0.01
  ),
  
  list(
    tarchetypes::tar_map(
      values = prior_sd_friends_and_family,
      names = org_code_of_interest,
      targets::tar_target(
        friends_and_family,
        cia_analysis(
          org_code_of_interest,
          friends_and_family_cia_format,
          "friends_and_family_percent",
          prior_sd,
          control_pool,
          hospitals
        )
      )
    )
  ),
  
  
  #Staff sickness
  prior_sd_staff_sickness <- tibble::tribble(
    ~ org_code_of_interest,
    ~ prior_sd,
    "REM",
    0.05,
    "REN",
    0.05,
    "RGM",
    0.05,
    "RGN",
    0.1,
   "RAL",
    0.05,
    "RVJ",
    0.05,
    "RWF",
    0.1
  ),
  
  list(
    tarchetypes::tar_map(
      values = prior_sd_staff_sickness,
      names = org_code_of_interest,
      targets::tar_target(
        staff_sickness,
        cia_analysis(
          org_code_of_interest,
          staff_sickness_cia_format,
          "staff_sickness_percent",
          prior_sd,
          control_pool,
          hospitals
        )
      )
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
      control_pool,
      hospitals
    )
  ),
  
  tar_target(
    staff_turnover_REN,
    cia_analysis(
      "REN",
      staff_turnover_cia_format,
      "leaving_rate",
      0.1,
      control_pool,
      hospitals
    )
  ),
  
  tar_target(
    staff_turnover_RGM,
    cia_analysis(
      "RGM",
      staff_turnover_cia_format,
      "leaving_rate",
      0.01,
      control_pool,
      hospitals
    )
  ),
  
  #Staff survey
  
  prior_sd_staff_survey <- tibble::tribble(
    ~ org_code_of_interest,
    ~ prior_sd,
    "REM",
    0.01,
    "REN",
    0.01,
    "RGM",
    0.01,
    "RAL",
    0.01,
    "RVJ",
    0.01,
    "RWF",
    0.001
  ),
  
  list(
    tarchetypes::tar_map(
      values = prior_sd_staff_survey,
      names = org_code_of_interest,
      targets::tar_target(
        staff_survey,
        cia_analysis(
          org_code_of_interest,
          staff_survey_cia_format,
          "positive_responses",
          prior_sd,
          control_pool,
          hospitals
        )
      )
    )
  ),
  
  
  # Cleaning costs
  
  prior_sd_cleaning_costs <- tibble::tribble(
    ~ org_code_of_interest,
    ~ prior_sd,
    "REM",
    0.01,
    "REN",
    0.01,
    "RGM",
    0.01,
    "RAL",
    0.001
  ),
  
  list(
    tarchetypes::tar_map(
      values = prior_sd_cleaning_costs,
      names = org_code_of_interest,
      targets::tar_target(
        cleaning_costs,
        cia_analysis(
          org_code_of_interest,
          cleaning_costs_cia_format|>filter(!is.na(cleaning_service_cost)),
          "cleaning_service_cost",
          prior_sd,
          control_pool,
          hospitals
        )
      )
    )
  ),
  
  
  #----------------------------------------------------------------------------#
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
    waiting_time_median_dta_output,
    model_output(
      waiting_time_median_dta_REM,
      waiting_time_median_dta_REN,
      waiting_time_median_dta_RGM,
      waiting_time_median_dta_RAL,
      NA,
      NA,
      NA
    )
  ),
  
  # tar_target(
  #   waiting_time_number_output,
  #   model_output(
  #     waiting_time_number_REM,
  #     waiting_time_number_REN,
  #     waiting_time_number_RGM,
  #     waiting_time_number_RGN,
  #     NA,
  #     NA,
  #     waiting_time_number_RWF
  #   )
  # ),
  
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
    cdiff_output,
    model_output(cdiff_REM,
                 NA,
                 NA,
                 NA,
                 cdiff_RAL,
                 cdiff_RVJ,
                 NA)
  ),
  
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
      friends_and_family_RVJ,
      NA
    )
  ),
  
  tar_target(
    staff_sickness_output,
    model_output(
      staff_sickness_REM,
      staff_sickness_REN,
      staff_sickness_RGM,
      staff_sickness_RGN,
      staff_sickness_RAL,
      staff_sickness_RVJ,
      staff_sickness_RWF
    )
  ),
  
  tar_target(
    staff_turnover_output,
    model_output(
      staff_turnover_REM,
      staff_turnover_REN,
      staff_turnover_RGM,
      NA,
      NA,
      NA,
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
      staff_survey_RWF
    )
  ),
  
  tar_target(
    cleaning_costs_output,
    model_output(
      cleaning_costs_REM,
      cleaning_costs_REN,
      cleaning_costs_RGM,
      NA,
      NA,
      NA,
      NA
    )
  ),
  
  #----------------------------------------------------------------------------#
  #### Formatting Quarto outputs ####
   tar_target(sbr_details,
              get_sbr_details(hospitals)),
  tar_target(sbr_details_table,
             get_sbr_details_table(hospitals)),
  tar_target(
    number_control_sites,
    get_number_control_sites(single_bedroom_matches_final)
  ),
  tar_target(
    flow_chart_selecting_controls,
    grViz(
      "digraph flowchart {

        # Starts and Ends
       node[shape = box, style = filled, fillcolor = \"#f9bf07\", color = \"#f9bf07\"]
        A[shape = box, label = \"All NHS England sites\"]
        Y[shape = box, label = \"Control Pool\"]
        Z[shape = box, label = \"Excluded\"]

        # Questions
      node[shape = ellipse]
        B[label = \"Does the type of site match?\"]
        C[label = \"Is it another study site?\"]
        D[label = \"Does it have a similar SBR % before the switch?\"]
        E[label = \"Is the SBR % stable after the switch?\"]
        F[label = \"Is the similarity rank* <= 20?\"]

        # Inclusions
        A -> B [color = \"#333739\"]
        B -> C [label = \"Yes\", color = \"#333739\"]
        C -> D [label = \"No\", color = \"#333739\"]
        D -> E [label = \"Yes\", color = \"#333739\"]
        E -> F [label = \"Yes\", color = \"#333739\"]
        F -> Y [label = \"Yes\", color = \"#333739\"]

        # Exclusions
        D -> Z [label = \"No\", color = \"#333739\"]
        E -> Z [label = \"No\", color = \"#333739\"]
        B -> Z [label = \"No\", color = \"#333739\"]
        C -> Z [label = \"Yes\", color = \"#333739\"]
        F -> Z [label = \"No\", color = \"#333739\"]

    }"
    )
  ),
  
  # Forest plots
  tar_target(
    forest_plot_LoS,
    forest_plot(
      LoS_output,
      "Points to the left indicate a decrease in mean length of stay following a switch to single bed   rooms, which is considered a positive effect. Significant positive effects are green, while significant negative effects (on the right) are red. Grey indicates no significant change.",
      "Relative effect of switching to single bedrooms on mean length of stay"
    )
  ),
  tar_target(
    forest_plot_waiting_time_median,
    forest_plot(
      waiting_time_median_output,
      "Points to the left indicate a decrease in median waiting time following a switch to single bed rooms, which is considered a positive effect. Significant positive effects are green, while significant negative effects (on the right) are indicated red. Grey indicates no significant change.",
      "Relative effect of switching to single bedrooms on median waiting time"
    )
  ),
  tar_target(
    forest_plot_waiting_time_median_dta,
    forest_plot(
      waiting_time_median_dta_output,
      "Points to the left indicate a decrease in median waiting time following a switch to single bed rooms, which is considered a positive effect. Significant positive effects are green, while significant negative effects (on the right) are indicated red. Grey indicates no significant change.",
      "Relative effect of switching to single bedrooms on median waiting time"
    )
  ),
  tar_target(
    forest_plot_bed_occupancy,
    forest_plot(
      bed_occupancy_output,
      "Points to the right indicate an increase in bed occupancy following a switch to single bed rooms, which is considered a positive effect. Significant positive effects are green, while significant negative effects (on the left) are indicated red. Grey indicates no significant change.",
      "Relative effect of switching to single bedrooms on bed occupancy"
    )
  ),
  tar_target(
    forest_plot_emergency_readmissions,
    forest_plot(
      emergency_readmissions_output,
      "Points to the left indicate a decrease in emergency readmissions following a switch to single bed rooms, which is considered a positive effect. Significant positive effects are green, while significant negative effects (on the right) are red. Grey indicates no significant change.",
      "Relative effect of switching to single bedrooms on emergency readmissions"
    )
  ),
  tar_target(
    forest_plot_cleaning_costs,
    forest_plot(
      cleaning_costs_output,
      "Points to the left indicate a decrease in cleaning costs following a switch to single bed rooms, which is considered a positive effect. Significant positive effects are green, while significant negative effects (on the right) are red. Grey indicates no significant change.",
      "Relative effect of switching to single bedrooms on cleaning costs"
    )
  ),
  tar_target(
    forest_plot_sus_deaths,
    forest_plot(
      sus_deaths_output,
      "Points to the left indicate a decrease in hospital deaths following a switch to single bed rooms, which is considered a positive effect. Significant positive effects green, while significant negative effects (on the right) are red. Grey indicates no significant change.",
      "Relative effect of switching to single bedrooms on in-hospital deaths"
    )
  ),
  tar_target(
    forest_plot_falls_and_fractures,
    forest_plot(
      falls_and_fractures_output,
      "Points to the left indicate a decrease in falls and fractures following a switch to single bed rooms, which is considered a positive effect. Significant positive effects are green, while significant negative effects (on the right) are red. Grey indicates no significant change.",
      "Relative effect of switching to single bedrooms on in-hospital falls, fractures and injuries"
    )
  ),
  tar_target(
    forest_plot_cdiff,
    forest_plot(
      cdiff_output,
      "Points to the left indicate a decrease in healthcare acquired C.Difficile infections  following a switch to single bed rooms, which is considered a positive effect. Significant positive effects are green. Grey indicates no significant change.",
      "Relative effect of switching to single bedrooms on rate of healthcare acquired C.Difficile infections"
    )
  ),
  tar_target(
    forest_plot_friends_and_family,
    forest_plot(
      friends_and_family_output,
      "Points to the right indicate an increase in positive scores on the Friends and Family patient experience test following a switch to single bed rooms, which is considered a positive effect. Significant positive effects are green, while significant negative effects (on the left) are red. Grey indicates no significant change.",
      "Relative effect of switching to single bedrooms on positive scores on the Friends and Family test"
    )
  ),
  tar_target(
    forest_plot_staff_sickness,
    forest_plot(
      staff_sickness_output,
      "Points to the left indicate a decrease in staff sickness following a switch to single bed rooms, which is considered a positive effect. Significant positive effects are green, while significant negative effects (on the right) are red. Grey indicates no significant change.",
      "Relative effect of switching to single bedrooms on staff sickness rate"
    )
  ),
  tar_target(
    forest_plot_staff_turnover,
    forest_plot(
      staff_turnover_output,
      "Grey indicates no significant change in staff turnover following a switch to single bed rooms.",
      "Relative effect of switching to single bedrooms on staff turnover"
    )
  ),
  tar_target(
    forest_plot_staff_survey,
    forest_plot(
      staff_survey_output,
      "Points to the right indicate an increase in positive responses to the NHS staff survey following a switch to single bed rooms, which is considered a positive effect. Significant positive effects are green, while significant negative effects (on the left) are red. Grey indicates no significant change.",
      "Relative effect of switching to single bedrooms on % of NHS staff that recommend their workplace"
    )
  ),
  
  # Model effects tables
  tar_target(
    model_effects_table_LoS,
    model_effects_table(
      LoS_output,
      "Average effects of switching to single bedrooms on mean length of stay"
    )
  ),
  tar_target(
    model_effects_table_waiting_time_median,
    model_effects_table(
      waiting_time_median_output,
      "Mean effect on median waiting time at general acute hospitals"
    )
  ),
  tar_target(
    model_effects_table_waiting_time_median_dta,
    model_effects_table(
      waiting_time_median_dta_output,
      "Mean effect on median waiting time at general acute hospitals"
    )
  ),
  tar_target(
    model_effects_table_bed_occupancy,
    model_effects_table(
      bed_occupancy_output,
      "Average effects of switching to single bedrooms on % of bed occupancy"
    )
  ),
  tar_target(
    model_effects_table_emergency_readmissions,
    model_effects_table(
      emergency_readmissions_output,
      "Average effects of switching to single bedrooms on % of emergency readmissions"
    )
  ),
  tar_target(
    model_effects_table_cleaning_costs,
    model_effects_table(
      cleaning_costs_output,
      "Average effects of switching to single bedrooms on cleaning costs (£) per sqm"
    )
  ),
  tar_target(
    model_effects_table_sus_deaths,
    model_effects_table(
      sus_deaths_output,
      "Average effects of switching to single bedrooms on in-hospital deaths/1,000 discharges"
    )
  ),
  tar_target(
    model_effects_table_falls_and_fractures,
    model_effects_table(
      falls_and_fractures_output,
      "Average effects of switching to single bedrooms on % of spells with in-hospital fall or injury"
    )
  ),
  tar_target(
    model_effects_table_cdiff,
    model_effects_table(
      cdiff_output,
      "Average effects of switching to single bedrooms on cases of healthcare acquired C.Difficile infections /10,000 bed days"
    )
  ),
  tar_target(
    model_effects_table_friends_and_family,
    model_effects_table(
      friends_and_family_output,
      "Average effects of switching to single bedrooms on % of positive responses on the Friends and Family test"
    )
  ),
  tar_target(
    model_effects_table_staff_sickness,
    model_effects_table(
      staff_sickness_output,
      "Average effects of switching to single bedrooms on staff sickness rate (% of available days)"
    )
  ),
  tar_target(
    model_effects_table_staff_turnover,
    model_effects_table(
      staff_turnover_output,
      "Average effects of switching to single bedrooms on rate of staff turnover (% leaving)"
    )
  ),
  tar_target(
    model_effects_table_staff_survey,
    model_effects_table(
      staff_survey_output,
      "Average effects of switching to single bedrooms on % of NHS staff who would recommend their workplace"
    )
  ),
  
  # Mean forest plots
  tar_target(
    mean_forest_plot_LoS,
    mean_forest_plot_DGH_Acute(
      LoS_output,
      "Mean effect on length of stay at general acute hospitals"
    )
  ),
  tar_target(
    mean_forest_plot_waiting_time_median,
    mean_forest_plot_DGH_Acute(
      waiting_time_median_output,
      "Mean effect on median waiting time at general acute hospitals"
    )
  ),
  tar_target(
    mean_forest_plot_bed_occupancy,
    mean_forest_plot_DGH_Acute(
      bed_occupancy_output,
      "Mean effect on bed occupancy at general acute hospitals"
    )
  ),
  tar_target(
    mean_forest_plot_emergency_readmissions,
    mean_forest_plot_DGH_Acute(
      emergency_readmissions_output,
      "Mean effect on emergency readmission rate at general acute hospitals"
    )
  ),
  tar_target(
    mean_forest_plot_sus_deaths,
    mean_forest_plot_DGH_Acute(
      sus_deaths_output,
      "Mean effect on in-hospital death rate at general acute hospitals"
    )
  ),
  tar_target(
    mean_forest_plot_falls_and_fractures,
    mean_forest_plot_DGH_Acute(
      falls_and_fractures_output,
      "Mean effect on rate of falls, fractures and injuries while in hospital at general acute hospitals"
    )
  ),
  tar_target(
    mean_forest_plot_cdiff,
    mean_forest_plot_DGH_Acute(
      cdiff_output,
      "Mean effect on the rate heathlcare acquired C.Difficile infections at general acute hospitals"
    )
  ),
  tar_target(
    mean_forest_plot_friends_and_family,
    mean_forest_plot_DGH_Acute(
      friends_and_family_output,
      "Mean effect on the % of positive responses to the Friends and Family test at general acute hospitals"
    )
  ),
  tar_target(
    mean_forest_plot_staff_sickness,
    mean_forest_plot_DGH_Acute(
      staff_sickness_output,
      "Mean effect on the rate of staff sickness at general acute hospitals"
    )
  ),
  tar_target(
    mean_forest_plot_staff_survey,
    mean_forest_plot_DGH_Acute(
      staff_survey_output,
      "Mean effect on percentage of staff that would recommend their trust as a place to work at general acute hospitals"
    )
  )
)
