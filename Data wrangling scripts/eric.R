# ERIC data

#### functions ####
# uses functions from data_wrangling_functions.R file
# these functions will be added to other file once happy with them
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
    dplyr::rename_with(~str_remove(., '_percent'))
  
  return(data)
  
}

missing_columns_eric_data <- function(data) {
  # if other columns are of interest, can add them here so if in some but not all
  cols <- c(cleaning_service_cost = NA, 
            cleaning_staff_wte = NA,
            occupied_floor_area_m2 = NA
  )
  
  data <- data |> 
    tibble::add_column(!!!cols[!names(cols) %in% names(data)])
  
}

wrangle_eric_for_beds <- function(data) {
  
  year <- data |>
    substitute() |>
    deparse() |>
    substr(6, 10)
  
  data <- data |>
    rename_eric_data() |>
    missing_columns_eric_data() |>
    dplyr::mutate(year = year,
                  total_single_bedrooms =
                    ifelse(substr(year, 1, 1) == "0",
                           (as.numeric(available_beds_no)
                            * as.numeric(percentage_of_single_bedrooms_for_patients)
                            / 100),
                           (as.numeric(single_bedrooms_for_patients_with_en_suite_facilities_no)
                            + as.numeric(single_bedrooms_for_patients_without_en_suite_facilities_no))
                    ),
                  gross_internal_floor_area_m2 = as.numeric(gross_internal_floor_area_m2),
                  site_heated_volume_m3 = as.numeric(site_heated_volume_m3),
                  across(starts_with("age_profile"), as.character),
                  occupied_floor_area_m2 = as.numeric(occupied_floor_area_m2)
    ) |> 
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


#### reading data in ####
eric_22_23 <- scrape_csv("https://files.digital.nhs.uk/41/5787C9/ERIC%20-%202022_23%20-%20Site%20data.csv")
eric_21_22 <- scrape_csv("https://files.digital.nhs.uk/EE/7E330D/ERIC%20-%20202122%20-%20Site%20Data%20v3.csv")
eric_20_21 <- scrape_csv("https://files.digital.nhs.uk/0F/46F719/ERIC%20-%20202021%20-%20Site%20data%20v2.csv")
eric_19_20 <- scrape_csv("https://files.digital.nhs.uk/11/BC1043/ERIC%20-%20201920%20-%20SiteData%20-%20v2.csv")
eric_18_19 <- scrape_csv("https://files.digital.nhs.uk/63/ADBFFF/ERIC%20-%20201819%20-%20SiteData%20v4.csv")

eric_17_18 <- scrape_csv("https://files.digital.nhs.uk/A8/188D99/ERIC-201718-SiteData.csv")
eric_16_17 <- scrape_csv("https://files.digital.nhs.uk/publication/q/3/eric-201617-site-data.csv")

eric_15_16 <- scrape_csv("https://files.digital.nhs.uk/publicationimport/pub21xxx/pub21992/est-ret-info-col-2015-2016-site-data.csv") |> 
  janitor::row_to_names(1) |>
  janitor::clean_names() 

eric_14_15 <- scrape_csv("https://files.digital.nhs.uk/publicationimport/pub18xxx/pub18726/est-ret-info-col-2014-2015-dat.csv") |> 
  janitor::row_to_names(1) |>
  janitor::clean_names() 

eric_13_14 <- scrape_xls("https://files.digital.nhs.uk/CF/698629/ERIC-201314-Data.XLS",
                         "Site Data"
                         )

eric_12_13 <- scrape_xls("https://files.digital.nhs.uk/98/1EB9FA/ERIC-2012-13-Data.XLS",
                         "Site Data"
                         )

eric_11_12 <- scrape_xls("https://files.digital.nhs.uk/41/0BEE05/ERIC-201112-Data.XLS",
                         "Site Data"
                         )

eric_10_11 <- scrape_xls("https://files.digital.nhs.uk/AE/D02AB9/ERIC-201011-Data.xls",
                         "Site Data"
                         )

eric_09_10 <- scrape_xls("https://files.digital.nhs.uk/C7/22BBC6/ERIC-200910-Data.xls",
                         "Site Data"
                         )

eric_08_09 <- scrape_xls("https://files.digital.nhs.uk/7B/DBAA4A/ERIC-200809-Data.xls",
                         "Site Data"
                         )

#### wrangling data ####
library(dplyr)
library(stringr)

eric_data <- bind_rows(wrangle_eric_for_beds(eric_08_09),
             wrangle_eric_for_beds(eric_09_10),
             wrangle_eric_for_beds(eric_10_11),
             wrangle_eric_for_beds(eric_11_12),
             wrangle_eric_for_beds(eric_12_13),
             wrangle_eric_for_beds(eric_13_14),
             wrangle_eric_for_beds(eric_14_15),
             wrangle_eric_for_beds(eric_15_16),
             wrangle_eric_for_beds(eric_16_17),
             wrangle_eric_for_beds(eric_17_18),
             wrangle_eric_for_beds(eric_18_19),
             wrangle_eric_for_beds(eric_19_20),
             wrangle_eric_for_beds(eric_20_21),
             wrangle_eric_for_beds(eric_21_22),
             wrangle_eric_for_beds(eric_22_23)
             ) |>
  mutate(site_type = str_replace_all(site_type, "[:digit:]. ", "")
         |> str_to_lower()
  ) 







