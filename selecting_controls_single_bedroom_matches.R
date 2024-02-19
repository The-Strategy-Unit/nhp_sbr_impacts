library(dplyr)

eric <- read.csv("data/formatted_eric.csv")
bed_occupancy <- read.csv("data/formatted_bed_occupancy.csv")

available_beds <- bed_occupancy |>
  summarise(total_available = sum(available, na.rm = TRUE), 
            .by = c(organisation_code, effective_snapshot_date)
            ) 

single_bedrooms <- eric |> 
  summarise(total_single_bedrooms = sum(total_single_bedrooms, na.rm = TRUE),
            .by = c(organisation_code, effective_snapshot_date)
            ) |>
  left_join(available_beds, by = c("organisation_code", "effective_snapshot_date")) |> 
  select(effective_snapshot_date,
         organisation_code,
         total_available,
         total_single_bedrooms
         ) |>
  mutate(percentage_single_bedrooms = total_single_bedrooms 
         / total_available 
         * 100
         ) |>
  filter(percentage_single_bedrooms <= 100)

# To find organisations with similar percentage of single bedrooms. Default is 
  # to look at organisations with a percentage of single bedrooms that is the 
  # percentage of single bedrooms in the base organisation +/- 5% of the 
  # percentage of single bedrooms in the base organisation. If the percentage 
  # of single bedrooms in the comparison organisations significantly changes
  # (default is 5 percentage points) in subsequent years, then that organisation
  # is excluded.
find_single_bedroom_matches <- function(date_pre_single_bedrooms,
                                        organisation,
                                        range = 0.05,
                                        significant_change_level = 5
                                        ) {
  
  # Get the percentage and number of single bedrooms of the organisation we want
  # to find matches for:
  percentage_single_bedrooms_base <- single_bedrooms |> 
    filter(effective_snapshot_date == date_pre_single_bedrooms
           & organisation_code == organisation) |>
    pull(percentage_single_bedrooms)
  
  number_single_bedrooms_base <- single_bedrooms |> 
    filter(effective_snapshot_date == date_pre_single_bedrooms
           & organisation_code == organisation) |>
    pull(total_single_bedrooms)
  
  # Find other organisations at the same time that have a similar percentage of
    # single bedrooms:
  similar_organisations <- single_bedrooms |>
    filter(effective_snapshot_date == date_pre_single_bedrooms
           & organisation_code != organisation
           & percentage_single_bedrooms > percentage_single_bedrooms_base - 
                                        range * percentage_single_bedrooms_base
           & percentage_single_bedrooms < percentage_single_bedrooms_base + 
                                        range * percentage_single_bedrooms_base
    ) |>
    mutate(difference_percentage_single_bedrooms = 
             round(percentage_single_bedrooms_base - percentage_single_bedrooms,
                   2
                   ),
           difference_number_single_bedrooms = 
             number_single_bedrooms_base - total_single_bedrooms
           )
  
  # Exclude the organisations have a significant change in the percentage of
    # single bedrooms in subsequent years:
  similar_organisations_that_remain_stable <- single_bedrooms |>
    filter(effective_snapshot_date > date_pre_single_bedrooms) |>
    right_join(similar_organisations, "organisation_code") |>
    mutate(significant_change = percentage_single_bedrooms.x - 
             percentage_single_bedrooms.y
           
    ) |>
    filter(abs(significant_change) < significant_change_level) |>
    select(organisation_code) |>
    unique() |>
    left_join(similar_organisations, "organisation_code") |>
    select(organisation_code,
           difference_percentage_single_bedrooms,
           difference_number_single_bedrooms
           )
  
  return(similar_organisations_that_remain_stable)
  
}



royal_liverpool_matches <- find_single_bedroom_matches("2022-03-31", "REM")
clatterbridge_matches <- find_single_bedroom_matches("2020-03-31", "REN")
royal_papworth_matches <- find_single_bedroom_matches("2019-03-31", "RGM")
peterborough_matches <- find_single_bedroom_matches("2010-03-31", "RGN")
chase_farm_matches <- find_single_bedroom_matches("2018-03-31", "RAL")
southmead_matches <- find_single_bedroom_matches("2014-03-31" , "RVJ") # or RVY???
tunbridge_wells_matches <- find_single_bedroom_matches("2011-03-31", "RWF")
