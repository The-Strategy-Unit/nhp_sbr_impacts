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

#### Workforce functions ####
# To wrangle the workforce data: 
wrangle_workforce <- function(month, sheetname_hc, sheetname_fte, skip = 6) { 
  
  sheetnumber <- substr(sheetname_hc, 1, 1) 
  
  if(missing(sheetname_fte)) {
    
    sheetname_fte <- stringr::str_replace_all(sheetname_hc,
                                     c("HC" = "FTE",
                                       "[[:digit:]]" = 
                                         as.numeric(sheetnumber) + 1
                                     )
    )
    
  }
  
  url <- workforce_links |>
    dplyr::filter(date == as.Date(month)) |>
    dplyr::pull(url)
  
  data_hc <- scrape_xls(url,
                        sheet = sheetname_hc,
                        skip = skip
                        ) |>
    dplyr::mutate(data_type = "HC")
  
  data_fte <- scrape_xls(url,
                         sheet = sheetname_fte, 
                         skip = skip
                         ) |>
    dplyr::mutate(data_type = "FTE")
  
  data <- data_hc |>
    dplyr::bind_rows(data_fte) |>
    dplyr::rename(org_code = x4) |>
    dplyr::select(-starts_with("x")) |>
    dplyr::filter(!is.na(org_code)) |>
    dplyr::mutate(across(-c(org_code, data_type), as.numeric)) |>
    tidyr::pivot_longer(names_to = "staff_group",
                 values_to = "total",
                 cols = -c(org_code, data_type)
    ) |>
    dplyr::mutate(effective_snapshot_date = as.Date(month),
           report_period_length = "Snapshot"
    )
  
  return(data)
  
}

# To provide the different arguments needed for the wrangle_workforce function, 
  # since the format of workforce files from NHS-Digital varies over time:
get_workforce <- function(month){
  
  if (month >= as.Date("2016-01-31") & month < as.Date("2016-05-31")) {
    
    wrangle_workforce(month = month,
                             sheetname_hc = "1. HEE Org Main Staff Gp HC",
                             sheetname_fte = "2. HEE Org Main Staff Gp FTE ",
                             skip = 3
    )
    
  } else if (month == as.Date("2016-05-31")) {
    
    wrangle_workforce(month = month,
                             sheetname_hc = "1. HEE Org Main Staff Gp HC",
                             skip = 4
    )
    
  } else if (month == as.Date("2016-06-30")) {
    
    wrangle_workforce(month = month,
                             "1. HEE Org Main Staff Gp HC ",
                             "2. HEE Org Main Staff Gp FTE",
                             skip = 4
    )
    
  } else if (month >= as.Date("2016-07-31") & month < as.Date("2017-03-31")) {
    
    wrangle_workforce(month = month,
                             sheetname_hc = "1. HEE Org Main Staff Gp HC ",
                             sheetname_fte = "2. HEE Org Main Staff Gp FTE"
    )
    
  } else if (month >= as.Date("2017-03-31") & month < as.Date("2019-03-31")) {
    
    wrangle_workforce(month = month,
                             sheetname_hc = "1. HEE Org Main Staff Gp HC"
    )
    
    
  } else if (month >= as.Date("2019-03-31") & month < as.Date("2019-07-31")) {
    
    wrangle_workforce(month = month,
                             sheetname_hc = "2. HEE, Org & SG - HC"
    )
    
  } else {
    
    print("Please enter a month end between 2016-01-31 and 2019-06-30.")
    
  }
  
}