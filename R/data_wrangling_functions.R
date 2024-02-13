#### General functions ####
# To get data from a csv file at a URL.
scrape_csv <- function(url) {
  
  download <- RCurl::getURL(url)
  
  data <- read.csv(text = download) |>
    janitor::clean_names()
  
  return(data)
  
}

# To get data from a xls file at a URL. Default is to read sheet 1, but can
  # specify other sheet.
scrape_xls <- function(url, sheet = 1) {
  
  tmp = tempfile(fileext = "")
  
  download.file(url = url, destfile = tmp, mode = "wb")
  
  data <- readxl::read_excel(path = tmp, sheet = sheet, skip = 1) |>
    janitor::clean_names()
  
  return(data)
  
}

#### ERIC functions ####
# To ensure column names are the same across all ERIC data frames.
rename_eric_data <- function(data) {
  
  lookup <- c(trust_code = "organisation_code",
              trust_name = "organisation_name",
              gross_internal_floor_area_m2 = "gross_internal_site_floor_area_m2",
              gross_internal_floor_area_m2 = "gross_internal_floor_area_m2",
              gross_internal_floor_area_m2 = "gross_internal_floor_area_m",
              site_heated_volume_m3 = "site_heated_volume_m",
              age_profile_2005_to_2014_percent = "age_profile_2005_to_present_percent",
              age_profile_2015_to_2024_percent = "age_profile_2015_to_present_percent",
              occupied_floor_area_m2 = "occupied_floor_area_m"
  )
  
  data <- data |>
    dplyr::rename(tidyselect::any_of(lookup)) |>
    dplyr::rename_with(~stringr::str_remove(., '_percent'))
  
  return(data)
  
}

# To create a column of NA for the missing columns in an ERIC dataframe.
missing_columns_eric_data <- function(data) {
  
  cols <- c(cleaning_service_cost = NA, 
            cleaning_staff_wte = NA,
            occupied_floor_area_m2 = NA
  )
  
  data <- data |> 
    tibble::add_column(!!!cols[!names(cols) %in% names(data)])
  
  return(data)
  
}

# To calculate the total number of single bedrooms for the ERIC site files.
calculate_number_single_bedrooms <- function(data) {
  
  data <- data |> 
    dplyr::mutate(total_single_bedrooms = 
                    ifelse(substr(year, 1, 1) == "0",
                           as.numeric(available_beds_no)
                            * as.numeric(percentage_of_single_bedrooms_for_patients)
                            / 100,
                           as.numeric(single_bedrooms_for_patients_with_en_suite_facilities_no)
                            + as.numeric(single_bedrooms_for_patients_without_en_suite_facilities_no)
                           )
                  )
  
  return(data)
  
}

# To wrangle the ERIC site data including calculating the number of single 
  # bedrooms.
wrangle_eric_site <- function(data) {
  
  year <- data |>
    substitute() |>
    deparse() |>
    substr(6, 10)
  
  data <- data |>
    rename_eric_data() |>
    missing_columns_eric_data() |>
    dplyr::mutate(year = year,   
                  across(c(gross_internal_floor_area_m2,
                           site_heated_volume_m3,
                           occupied_floor_area_m2,
                           cleaning_staff_wte,
                           starts_with(c("age_profile"))
                           ),
                         as.numeric
                         ),
                  cleaning_service_cost = str_replace_all(cleaning_service_cost,
                                                          c("Not Applicabale" = "NA",
                                                            "," = "")
                                                          ) |>
                    as.numeric(),
                  site_type = str_replace_all(site_type, "[:digit:]. ", "") |> 
                    str_to_lower()
    ) |>
    calculate_number_single_bedrooms() |> 
    dplyr::select(year, 
                  trust_code,
                  site_code,
                  site_type,
                  gross_internal_floor_area_m2,
                  occupied_floor_area_m2,
                  site_heated_volume_m3,
                  total_single_bedrooms,
                  (starts_with("age") & !contains("100")),
                  cleaning_service_cost,
                  cleaning_staff_wte
    )
  
  return(data)
}

# To wrangle the ERIC trust data.
wrangle_eric_trust <- function(data) {
  
  year <- data |>
    substitute() |>
    deparse() |>
    substr(6, 10)
  
  data <- data |>
    dplyr::mutate(year = year,
                  number_of_cleaning_staff_wte = as.numeric(number_of_cleaning_staff_wte)) |>
    dplyr::select(year, 
                  trust_code = organisation_code, 
                  trust_cleaning_service_cost = cleaning_services_costs, 
                  trust_cleaning_staff_wte = number_of_cleaning_staff_wte
    ) 
  
  return(data)
  
}