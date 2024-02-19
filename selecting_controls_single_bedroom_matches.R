available_beds <- read.csv("data/formatted_bed_occupancy.csv") |>
  get_available_beds_by_organisation()

single_bedrooms <- read.csv("data/formatted_eric.csv") |>
  get_percentage_single_bedrooms(available_beds)

royal_liverpool_matches <- find_single_bedroom_matches(single_bedrooms, 
                                                       "2022-03-31", 
                                                       "REM"
                                                       )

clatterbridge_matches <- find_single_bedroom_matches(single_bedrooms, 
                                                     "2020-03-31", 
                                                     "REN"
                                                     )

royal_papworth_matches <- find_single_bedroom_matches(single_bedrooms, 
                                                      "2019-03-31", 
                                                      "RGM"
                                                      )

peterborough_matches <- find_single_bedroom_matches(single_bedrooms, 
                                                    "2010-03-31", 
                                                    "RGN"
                                                    )

chase_farm_matches <- find_single_bedroom_matches(single_bedrooms, 
                                                  "2018-03-31", 
                                                  "RAL"
                                                  )

southmead_matches <- find_single_bedroom_matches(single_bedrooms, 
                                                 "2014-03-31", 
                                                 "RVJ"
                                                 ) 

tunbridge_wells_matches <- find_single_bedroom_matches(single_bedrooms, 
                                                       "2011-03-31", 
                                                       "RWF"
                                                       )

bind_rows_and_clear <- function(dataframes) {
  
  data <- do.call(dplyr::bind_rows, mget(dataframes, envir = .GlobalEnv))
  rm(list = dataframes, envir = .GlobalEnv)
  
  return(data)
}

single_bedroom_matches <- bind_rows_and_clear(c("royal_liverpool_matches", 
                                                "clatterbridge_matches",
                                                "royal_papworth_matches",
                                                "peterborough_matches",
                                                "chase_farm_matches",
                                                "southmead_matches",
                                                "tunbridge_wells_matches"
                                                )
                                              )