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
                              "RWF"
  )
  
  data <- data |>
    dplyr::left_join(ref_org_sites,
                     by = c("matching_organisation_code" = "trust_code")) |>
    dplyr::inner_join(filtered_sites, "site_code") |>
    dplyr::filter(!matching_organisation_code %in% site_codes_of_interest)
  
  return(data)
  
}

