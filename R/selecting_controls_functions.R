#### Single bedroom matches ####
# To sum the number of available beds by organisation and date:
get_available_beds_by_organisation <- function(data) {
  
  data <- data |>
    dplyr::summarise(total_available = sum(available, na.rm = TRUE), 
                     .by = c(organisation_code, effective_snapshot_date)
                     ) 
  
  return(data)
  
}

# To get the percentage of single bedrooms by organisation and date:
get_percentage_single_bedrooms <- function(data, available_beds) {
  
  data <- data |> 
    dplyr::summarise(total_single_bedrooms = sum(total_single_bedrooms,
                                                 na.rm = TRUE
                                                 ),
              .by = c(organisation_code, effective_snapshot_date)
              ) |>
    dplyr::left_join(available_beds, by = c("organisation_code", 
                                            "effective_snapshot_date"
                                            )
                     ) |> 
    dplyr::select(effective_snapshot_date,
                  organisation_code,
                  total_available,
                  total_single_bedrooms
                  ) |>
    dplyr::mutate(percentage_single_bedrooms = total_single_bedrooms 
                  / total_available 
                  * 100
                  ) |>
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
                                        significant_change_level = 5
                                        ) {
  
  # Get the percentage and number of single bedrooms of the organisation we want
  # to find matches for:
  percentage_single_bedrooms_base <- data |> 
    dplyr::filter(effective_snapshot_date == date_pre_single_bedrooms
                  & organisation_code == organisation
                  ) |>
    dplyr::pull(percentage_single_bedrooms)
  
  number_single_bedrooms_base <- data |> 
    dplyr::filter(effective_snapshot_date == date_pre_single_bedrooms
                  & organisation_code == organisation
                  ) |>
    dplyr::pull(total_single_bedrooms)
  
  # Find other organisations at the same time that have a similar percentage of
  # single bedrooms:
  similar_organisations <- data |>
    dplyr::filter(effective_snapshot_date == date_pre_single_bedrooms
                  & organisation_code != organisation
                  & percentage_single_bedrooms > 
                    percentage_single_bedrooms_base - range 
                  & percentage_single_bedrooms < 
                    percentage_single_bedrooms_base + range 
                  ) |>
    dplyr::mutate(difference_percentage_single_bedrooms = 
                    round(percentage_single_bedrooms_base - 
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
                    percentage_single_bedrooms.y
                  ) |>
    dplyr::filter(abs(significant_change) < significant_change_level) |>
    dplyr::select(organisation_code) |>
    unique() |>
    dplyr::left_join(similar_organisations, "organisation_code") |>
    dplyr::select("matching_organisation_code" = organisation_code,
                  difference_percentage_single_bedrooms,
                  difference_number_single_bedrooms
                  ) |>
    dplyr::mutate(organisation_code = organisation, .before = 1)
  
  return(similar_organisations_that_remain_stable)
  
}