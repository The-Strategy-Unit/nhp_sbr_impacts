#### Single bedroom matches ####
# To sum the number of available beds by organisation and date:
get_available_beds_by_organisation <- function(data) {
  data <- data |>
    dplyr::summarise(
      total_available = sum(available, na.rm = TRUE),
      .by = c(organisation_code, effective_snapshot_date)
    )
  
  return(data)
  
}

# To get the percentage of single bedrooms by organisation and date:
get_percentage_single_bedrooms <- function(data, available_beds) {
  data <- data |>
    dplyr::summarise(
      total_single_bedrooms = sum(total_single_bedrooms,
                                  na.rm = TRUE),
      .by = c(organisation_code, effective_snapshot_date)
    ) |>
    dplyr::left_join(available_beds,
                     by = c("organisation_code",
                            "effective_snapshot_date")) |>
    dplyr::select(
      effective_snapshot_date,
      organisation_code,
      total_available,
      total_single_bedrooms
    ) |>
    dplyr::mutate(percentage_single_bedrooms = total_single_bedrooms
                  / total_available
                  * 100) |>
    dplyr::filter(percentage_single_bedrooms <= 100)
  
  return(data)
  
}

# To find organisations with similar percentage of single bedrooms. Default is
# to look at organisations with a percentage of single bedrooms that is the
# percentage of single bedrooms in the base organisation +/- 5% of the
# percentage of single bedrooms in the base organisation. If the percentage
# of single bedrooms in the comparison organisations significantly changes
# (default is 5 percentage points) in subsequent years, then that organisation
# is excluded.
find_single_bedroom_matches <- function(data,
                                        date_pre_single_bedrooms,
                                        organisation,
                                        range = 5,
                                        significant_change_level = 5) {
  # Get the percentage and number of single bedrooms of the organisation we want
  # to find matches for:
  percentage_single_bedrooms_base <- data |>
    dplyr::filter(
      effective_snapshot_date == date_pre_single_bedrooms
      & organisation_code == organisation
    ) |>
    dplyr::pull(percentage_single_bedrooms)
  
  number_single_bedrooms_base <- data |>
    dplyr::filter(
      effective_snapshot_date == date_pre_single_bedrooms
      & organisation_code == organisation
    ) |>
    dplyr::pull(total_single_bedrooms)
  
  # Find other organisations at the same time that have a similar percentage of
  # single bedrooms:
  similar_organisations <- data |>
    dplyr::filter(
      effective_snapshot_date == date_pre_single_bedrooms
      & organisation_code != organisation
      & percentage_single_bedrooms >
        percentage_single_bedrooms_base - range
      & percentage_single_bedrooms <
        percentage_single_bedrooms_base + range
    ) |>
    dplyr::mutate(
      difference_percentage_single_bedrooms =
        round(
          percentage_single_bedrooms_base -
            percentage_single_bedrooms,
          2
        ),
      difference_number_single_bedrooms =
        number_single_bedrooms_base - total_single_bedrooms
    )
  
  # Exclude the organisations have a significant change in the percentage of
  # single bedrooms in subsequent years:
  similar_organisations_that_remain_stable <- data |>
    dplyr::filter(effective_snapshot_date > date_pre_single_bedrooms) |>
    dplyr::right_join(similar_organisations, "organisation_code") |>
    dplyr::mutate(significant_change = percentage_single_bedrooms.x -
                    percentage_single_bedrooms.y) |>
    dplyr::filter(abs(significant_change) < significant_change_level) |>
    dplyr::select(organisation_code) |>
    unique() |>
    dplyr::left_join(similar_organisations, "organisation_code") |>
    dplyr::select(
      "matching_organisation_code" = organisation_code,
      difference_percentage_single_bedrooms,
      difference_number_single_bedrooms
    ) |>
    dplyr::mutate(organisation_code = organisation, .before = 1)
  
  return(similar_organisations_that_remain_stable)
  
}

# To create a dataframe of all the sites with their matching sites for single
# bedrooms.
combine_single_bedroom_matches <- function(single_bedrooms,
                                           ref_org_sites,
                                           general_acute_sites,
                                           cancer_sites,
                                           cardiac_sites) {
  data <- rbind(
    # royal_liverpool_matches
    find_single_bedroom_matches(single_bedrooms,
                                "2022-03-31",
                                "REM") |>
      site_type_filter(general_acute_sites, ref_org_sites),
    
    # clatterbridge_matches
    find_single_bedroom_matches(single_bedrooms,
                                "2020-03-31",
                                "REN") |>
      site_type_filter(cancer_sites, ref_org_sites),
    
    # royal_papworth_matches
    find_single_bedroom_matches(single_bedrooms,
                                "2019-03-31",
                                "RGM") |>
      site_type_filter(cardiac_sites, ref_org_sites),
    
    # peterborough_matches
    find_single_bedroom_matches(single_bedrooms,
                                "2010-03-31",
                                "RGN") |>
      site_type_filter(general_acute_sites, ref_org_sites),
    
    # chase_farm_matches
    find_single_bedroom_matches(single_bedrooms,
                                "2018-03-31",
                                "RAL") |>
      site_type_filter(general_acute_sites, ref_org_sites),
    
    # southmead_matches
    find_single_bedroom_matches(single_bedrooms,
                                "2014-03-31",
                                "RVJ") |>
      site_type_filter(general_acute_sites, ref_org_sites),
    
    # tunbridge_wells_matches
    find_single_bedroom_matches(single_bedrooms,
                                "2011-03-31",
                                "RWF") |>
      site_type_filter(general_acute_sites, ref_org_sites)
  )
  
  return(data)
  
}

# To get site codes for cancer centres:
get_cancer_centre_site_codes <- function(cancer_centres_filepath) {
  data <- read.csv(cancer_centres_filepath) |>
    janitor::clean_names() |>
    tidyr::pivot_longer(
      cols = contains("site_code"),
      names_to = "number",
      values_to = "site_code"
    ) |>
    dplyr::select(site_code) |>
    na.omit()
  
  return(data)
  
}

# To get site codes for cardiac sites:
get_cardiac_site_codes <- function(cardiac_site_filepath) {
  data <- read.csv(cardiac_site_filepath) |>
    janitor::clean_names() |>
    dplyr::select("site_code" = org_site_code) |>
    na.omit()
  
  return(data)
  
}

# To get general acute site codes:
get_general_acute_site_codes <- function(formatted_eric) {
  data <- formatted_eric |>
    dplyr::filter(site_type == "general acute hospital") |>
    dplyr::select(site_code) |>
    unique()
  
  return(data)
  
}

# To filter single bedroom matches by site type, to remove matches that are
# the other hospitals of interest, to add in organisation and site names:
site_type_filter <- function(data, filtered_sites, ref_org_sites) {
  site_codes_of_interest <- c("REM",
                              "REN",
                              "RGM",
                              "RGN",
                              "RAL",
                              "RVJ",
                              "RWF")
  
  data <- data |>
    dplyr::left_join(ref_org_sites,
                     by = c("matching_organisation_code" = "trust_code")) |>
    dplyr::inner_join(filtered_sites, "site_code") |>
    dplyr::filter(!matching_organisation_code %in% site_codes_of_interest)
  
  return(data)
  
}


# To get the floor space by sites and date:
get_floor_space_by_site <- function(data) {
  floor_space <- data |>
    filter(!is.na(occupied_floor_area_m2)) |>
    dplyr::select(organisation_code,
                  site_code,
                  effective_snapshot_date,
                  occupied_floor_area_m2)
  
  return(floor_space)
  
}

# To find sites with similar floor space. Default is
# to look at sites with a floor space in m2 that is +/- 30% of the
# base sites.

find_floor_space_matches <- function(data,
                                     date_pre_single_bedrooms,
                                     site_code_pick,
                                     range = 0.3) {
  # Get the floor space of the sites we want
  # to find matches for:
  floor_space_base <- data |>
    dplyr::filter(effective_snapshot_date == date_pre_single_bedrooms
                  & site_code == site_code_pick) |>
    dplyr::pull(occupied_floor_area_m2)
  
  fsb_high <- floor_space_base * (1 + range)
  fsb_low <- floor_space_base * (1 - range)
  
  # Find other sites at the same time that have a similar floor space:
  similar_sites <- data |>
    dplyr::filter(effective_snapshot_date == date_pre_single_bedrooms,
                  #occupied_floor_area_m2 > fsb_low,
                  #occupied_floor_area_m2 < fsb_high,
                  site_code != site_code_pick) |>
    dplyr::mutate(
      difference_floor_space = round((occupied_floor_area_m2 - floor_space_base) / floor_space_base,
                                     2
      ),
      site_code_og = site_code_pick,
      org_code_og = stringr::str_sub(site_code_og, 1, 3)
    ) |>
    select(7, 6, 1:5)
  
  return(similar_sites)
  
}

# To create a dataframe of all the sites with their matching sites for floor
# space. By necessity some of the codes and dates of sites are changed
# to ensure baseline size is captured

combine_floor_space_matches <- function(floor_space) {
  data <- rbind(
    # royal_liverpool_matches
    find_floor_space_matches(floor_space,
                             "2022-03-31",
                             "REMRQ"),
    # clatterbridge_matches
    find_floor_space_matches(floor_space,
                             "2022-03-31",
                             "REN22"),
    # royal_papworth_matches
    find_floor_space_matches(floor_space,
                             "2022-03-31",
                             "RGM21"),
    # peterborough_matches
    find_floor_space_matches(floor_space,
                             "2012-03-31",
                             "RGN80"),
    # chase_farm_matches
    find_floor_space_matches(floor_space,
                             "2018-03-31",
                             "RALC7"),
    # southmead_matches
    find_floor_space_matches(floor_space,
                             "2016-03-31",
                             "RVJ01"),
    # tunbridge_wells_matches
    find_floor_space_matches(floor_space,
                             "2012-03-31",
                             "RWFTW")
  )
  
  return(data)
  
}

# join floor space to single-bed matches

control_stage2 <- function(data1, data2) {
  data <- data1 |>
    left_join(
      data2 |> select(
        org_code_og,
        site_code,
        occupied_floor_area_m2,
        difference_floor_space
      ),
      by = c("organisation_code" = "org_code_og", "site_code" =
               "site_code")
    )# |>
  #mutate(fs_match = if_else(is.na(difference_floor_space),0,1))
  
  return(data)
  
}

# To get the median ages by sites and date:
get_med_age_by_site <- function(filepth) {
  med_age <- read.csv(filepth) |>
    janitor::clean_names() |>
    filter(!is.na(age_med))
  
  return(med_age)
  
}

# To find sites with similar patient ages (median). Default is
# to look at sites with median age that is +/- 5 years of the
# base sites.

find_med_age_matches <- function(data,
                                 month_pre_single_bedrooms,
                                 site_code_pick,
                                 range = 5) {
  # Get the median of the sites we want
  # to find matches for:
  med_age_base <- data |>
    dplyr::filter(yr_mth == month_pre_single_bedrooms
                  & der_provider_site_code == site_code_pick) |>
    dplyr::pull(age_med)
  
  age_high <- med_age_base + range
  age_low <- med_age_base - range
  
  # Find other sites at the same time that have a similar ages:
  similar_sites <- data |>
    dplyr::filter(yr_mth == month_pre_single_bedrooms,
                  #age_med > age_low,
                  #age_med < age_high,
                  der_provider_site_code != site_code_pick) |>
    dplyr::mutate(
      difference_med_age = round(((
        age_med - med_age_base
      ) / med_age_base), 2),
      site_code_og = site_code_pick,
      org_code_og = stringr::str_sub(site_code_og, 1, 3)
    ) |>
    select(8, 7, 1:6)
  
  return(similar_sites)
  
}

# To create a dataframe of all the sites with their matching sites for patient
# age. By necessity some of the codes and dates of sites are changed
# to ensure baseline size is captured

combine_med_age_matches <- function(med_age) {
  data <- rbind(
    # royal_liverpool_matches
    find_med_age_matches(med_age,
                         "2022-11",
                         "REMRQ"),
    # clatterbridge_matches
    find_med_age_matches(med_age,
                         "2020-07",
                         "REN22"),
    # royal_papworth_matches
    find_med_age_matches(med_age,
                         "2019-06",
                         "RGM22"),
    # peterborough_matches
    find_med_age_matches(med_age,
                         "2010-12",
                         "RGN80"),
    # chase_farm_matches
    find_med_age_matches(med_age,
                         "2018-10",
                         "RALC7"),
    # southmead_matches
    find_med_age_matches(med_age,
                         "2014-06",
                         "RVJ01"),
    # tunbridge_wells_matches
    find_med_age_matches(med_age,
                         "2011-10",
                         "RWFTW")
  )
  
  return(data)
  
}

# join median age to single-bed matches

control_stage3 <- function(data1, data2) {
  data <- data1 |>
    left_join(
      data2 |> select(
        org_code_og,
        der_provider_site_code,
        age_med,
        difference_med_age
      ),
      by = c("organisation_code" = "org_code_og",
             "site_code" = "der_provider_site_code")
    )# |>
  #mutate(age_match = if_else(is.na(difference_med_age),0,1))
  
  return(data)
  
}

# To get the elective ratio by sites and date:
get_elec_ratio_by_site <- function(filepth) {
  elec_ratio <- read.csv(filepth) |>
    janitor::clean_names() |>
    filter(!is.na(elec_emrg_ratio))
  
  return(elec_ratio)
  
}

# To find sites with similar elective ratio. Default is
# to look at sites with ratio that is +/- 5 years of the
# base sites.

find_elec_ratio_matches <- function(data,
                                 year_pre_single_bedrooms,
                                 site_code_pick,
                                 range = 0.2) {
  # Get the median of the sites we want
  # to find matches for:
  elec_ratio_base <- data |>
    dplyr::filter(der_financial_year == year_pre_single_bedrooms
                  & der_provider_site_code == site_code_pick) |>
    dplyr::pull(elec_emrg_ratio)
  
  ratio_high <- elec_ratio_base * (1+range)
  ratio_low <- elec_ratio_base * (1-range)
  
  # Find other sites at the same time that have a similar ratio:
  similar_sites <- data |>
    dplyr::filter(der_financial_year == year_pre_single_bedrooms,
                  #elec_ratio_base > ratio_low,
                  #elec_ratio_base < ratio_high,
                  der_provider_site_code != site_code_pick) |>
    dplyr::mutate(
      difference_elec_ratio = round(((
        elec_emrg_ratio - elec_ratio_base
      ) / elec_ratio_base), 2),
      site_code_og = site_code_pick,
      org_code_og = stringr::str_sub(site_code_og, 1, 3)
    ) |>
    select(9, 8, 2, 3:7)
  
  return(similar_sites)
  
}

# To create a dataframe of all the sites with their matching sites for patient
# age. By necessity some of the codes and dates of sites are changed
# to ensure baseline size is captured

combine_elec_ratio_matches <- function(elec_ratio) {
  data <- rbind(
    # royal_liverpool_matches
    find_elec_ratio_matches(elec_ratio,
                         "2022/23",
                         "REMRQ"),
    # clatterbridge_matches
    find_elec_ratio_matches(elec_ratio,
                         "2020/21",
                         "REN22"),
    # royal_papworth_matches
    find_elec_ratio_matches(elec_ratio,
                         "2019/20",
                         "RGM22"),
    # peterborough_matches
    find_elec_ratio_matches(elec_ratio,
                         "2011/12",
                         "RGN80"),
    # chase_farm_matches
    find_elec_ratio_matches(elec_ratio,
                         "2019/20",
                         "RALC7"),
    # southmead_matches
    find_elec_ratio_matches(elec_ratio,
                         "2014/15",
                         "RVJ01"),
    # tunbridge_wells_matches
    find_elec_ratio_matches(elec_ratio,
                         "2011/12",
                         "RWFTW")
  )
  
  return(data)
  
}

# join elective ratio to single-bed matches

control_stage4 <- function(data1, data2) {
  data <- data1 |>
    left_join(
      data2 |> select(
        org_code_og,
        der_provider_site_code,
        elec_emrg_ratio,
        difference_elec_ratio
      ),
      by = c("organisation_code" = "org_code_og",
             "site_code" = "der_provider_site_code")
    )# |>
  #mutate(age_match = if_else(is.na(difference_med_age),0,1))
  
  return(data)
  
}

# rank floor space, age and elective ratio variables (unsigned differences) for each site

ranking_control_var <- function(df) {
  data <- df |>
    group_by(organisation_code) |>
    mutate(
      floor_rank = rank(abs(difference_floor_space), ties.method = "average"),
      age_rank = rank(abs(difference_med_age), ties.method = "average"),
      elec_rank = rank(abs(difference_elec_ratio), ties.method = "average")
      ##put any additional ranking variables/steps in here,
    ) |>
    mutate(sum_ranks = rowSums(across(contains("_rank")))) |>
    group_by(organisation_code) |>
    mutate(rank_of_ranks = rank(sum_ranks, ties.method = "first")) |>
    ungroup()
  
  return(data)
  
}

get_controls_table <- function(data, organisation){
  
  table <- data |>
    filter(rank_of_ranks <= 20 & organisation_code == organisation) |>
    mutate(
      site_name = site_name |>
        stringr::str_to_title() |>
        stringr::str_replace_all(c(
          " Nhs " = " NHS ",
          " And " = " and ",
          " Utc " = " UTC "
        )),
      trust_name = trust_name |>
        stringr::str_to_title() |>
        stringr::str_replace_all(c(
          " Nhs " = " NHS ",
          " And " = " and ",
          " Utc " = " UTC "
        ))
    ) |>
    select(
      "Provider" = trust_name,
      "Provider code" = matching_organisation_code,
      "Site name" = site_name,
      "Site code" = site_code
    ) |>
    as_flextable(
      hide_grouplabel = TRUE,
      max_row = 20,
      show_coltype = FALSE
    ) |>
    align(part = "header", align = "center") |>
    bg(bg = "#f9bf07", part = "header") |>
    bold(bold = TRUE, part = "header") |>
    fontsize(size = 12, part = "all") |>
    padding(
      padding = 2,
      part = "all",
      padding.top = NULL
    ) |>
    autofit()
 
  return(table)
  
}
