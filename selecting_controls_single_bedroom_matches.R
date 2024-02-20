available_beds <- read.csv("data/formatted_bed_occupancy.csv") |>
  get_available_beds_by_organisation()

single_bedrooms <- read.csv("data/formatted_eric.csv") |>
  get_percentage_single_bedrooms(available_beds)

single_bedroom_matches <- combine_single_bedroom_matches(single_bedrooms)



