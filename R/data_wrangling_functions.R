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
scrape_xls <- function(url, sheet = 1, skip = 0) {
  
  tmp = tempfile(fileext = "")
  
  download.file(url = url, destfile = tmp, mode = "wb")
  
  data <- readxl::read_excel(path = tmp, sheet = sheet, skip = skip) |>
    janitor::clean_names()
  
  return(data)
  
}

#### ERIC functions ####
# To calculate the total number of single bedrooms in eric data after 2010.
calculate_single_bedrooms <- function(data) {
  
  data <- data |>
    dplyr::filter(grepl("Single", measure)) |>
    tidyr::pivot_wider(names_from = "measure", values_from = "value") |>
    dplyr::mutate(across(contains("Single"), as.numeric),
                  total_single_bedrooms = 
                    `Single bedrooms for patients with en-suite facilities (No.)` +
                    `Single bedrooms for patients without en-suite facilities (No.)`
                  ) |>
    tidyr::pivot_longer(names_to = "measure", 
                        values_to = "value", 
                        cols = total_single_bedrooms
                        ) |>
    dplyr::select(-contains("Single")) |>
    rbind(data)
  
  return(data)
  
}

# The number single bedrooms for 2009-03-31 and 2010-03-31 are not in UDAL, but 
  # can be calculated from fields in NHS-Digital files.
get_eric_before_2010 <- function(year) {
  
  years <- paste0(year, 
                  stringr::str_sub(year + 1, 3, 4)
  )
  
  url <- stringr::str_replace("https://files.digital.nhs.uk/C7/22BBC6/ERIC-YEARS-Data.xls",
                     "YEARS",
                     "200910"
                     )
  
  data <- scrape_xls(url, "Site Data") |>
    dplyr::mutate(effective_snapshot_date = as.Date(stringr::str_replace("YEAR-03-31", 
                                                                         "YEAR", 
                                                                         as.character(year))
                                                    ),
                  level = "Site",
                  total_single_bedrooms = as.numeric(available_beds_no) *
                    as.numeric(percentage_of_single_bedrooms_for_patients_percent) 
                  / 100
                  ) |>
    tidyr::pivot_longer(names_to = "measure",
                        values_to = "value",
                        cols = total_single_bedrooms
                        ) |>
    dplyr::select(effective_snapshot_date,
                  organisation_code,
                  site_code,
                  organisation_type,
                  site_type,
                  measure,
                  value,
                  level
    ) 
  
  return(data)
  
}
