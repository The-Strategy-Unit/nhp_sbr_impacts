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

# To read in the csv files of the outputs of sql queries and to apply common
  # data manipulations:
read_sql_output <- function(filename) {
  
  data <- read.csv(filename) |>
    janitor::clean_names() |>
    dplyr::mutate(effective_snapshot_date = as.Date(effective_snapshot_date, 
                                                    format = "%d/%m/%Y"
                                                    )
           )
  
  return(data)
}

# To standardise the staff groups across data sources.
  # There are probably some more groups that can be matched between workforce 
  # data from UDAL and from NHS-D and the turnover data from UDAL.
standardise_staff_group <- function(staff_group) {
  
  staff_group <- staff_group |> 
    stringr::str_to_lower() |>
    stringr::str_replace_all(c(" [[:punct:]]" = "",
                      "&" = " ",
                      "," = "",
                      " " = "_",
                      "managers" = "manager",
                      "all_staff_groups" = "total"
                      )
                    )
  
  return(staff_group)
  
}

#-----------------------------------------------------------------------------#

#### Bed Occupancy functions ####
get_bed_occupancy_from_udal <- function(bed_occupancy_file) {
  
  data <- read_sql_output(bed_occupancy_file) |>
    mutate(bed_occupancy = as.numeric(bed_occupancy))
  
  return(data)
  
}

#-----------------------------------------------------------------------------#

#### ERIC functions ####
get_eric_data_from_udal <- function(eric_09_15, eric_16_23) {
  
  data <- read.csv(eric_09_15) |>
    bind_rows(read.csv(eric_16_23)) |>
    get_single_bedrooms() |>
    mutate(value = as.numeric(value),
           effective_snapshot_date = as.Date(effective_snapshot_date, 
                                             format = "%d/%m/%Y"
           )
    )
  
  return(data)
}

combine_eric_data <- function(eric_udal, eric_09, eric_10) {
  
  data <- eric_udal |>
    bind_rows(eric_09,
              eric_10
    ) |>
    mutate(site_type = str_replace_all(site_type, "[:digit:]. ", "") |> 
             str_to_lower(),
           organisation_type = str_to_lower(organisation_type)
    ) |>
    pivot_wider(names_from = "measure", values_from = "value") |>
    clean_names()
  
  return(data)
  
}

# To calculate the total number of single bedrooms in eric UDAL data.
get_single_bedrooms <- function(data) {
  
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
get_single_bedrooms_for_2009_10 <- function(year) {
  
  years <- paste0(year, 
                  stringr::str_sub(year + 1, 3, 4)
  )
  
  url <- stringr::str_replace("https://files.digital.nhs.uk/C7/22BBC6/ERIC-YEARS-Data.xls",
                     "YEARS",
                     "200910"
                     )
  
  data <- scrape_xls(url, "Site Data", skip = 1) |>
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

#-----------------------------------------------------------------------------#

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
    dplyr::mutate(effective_snapshot_date = as.Date(month)
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

## Wrangling waiting times data



## Wrangling friends and family inpatient scores
friends_and_family_scores_data_formatting<-function(data1, data2){
  
  #pre 2022-07 data
  friends_and_family_scores_data1<-read.csv(data1)|>
    clean_names()|>
    mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%Y-%m-%d"))|> #Format date
    mutate(month=floor_date(effective_snapshot_date, "month"))|> #Format date to monthly
    mutate(count=ifelse(count=="NULL", 0, count))|>
    mutate(count=as.numeric(count))|>
    mutate(positive_responses=ifelse(likely_to_recommend=="Very Good"|likely_to_recommend=="Good"|
                                       likely_to_recommend=="Likely"|likely_to_recommend=="Extremely Likely" , "yes", "no"))|> #flag those that are +ive responses
    filter(grouped_by=="Site")|>
    group_by(month, site_code)|>
    mutate(percent=round(((count/sum(count))*100),1))|> #calculate %
    filter(positive_responses=="yes")|>
    summarise(percent=sum(percent)) #calculate %
  
  #Post 2022-07 data  
  friends_and_family_scores_data2<-read.csv(data2)|>
    clean_names()|>
    mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%d/%m/%Y"))|> #Format date
    mutate(month=floor_date(effective_snapshot_date, "month"))|> #Format date to monthly
    filter(measure_category=="Percentage Positive")|>
    mutate(percent=as.numeric(measure_value)*100)|>
    select(-effective_snapshot_date, -measure_category, -measure_value, -measure_name)|>
    filter(month>='2022-08-01')
  
  friends_and_family_scores_data<-rbind(friends_and_family_scores_data1,friends_and_family_scores_data2)
  
  
  write.csv(friends_and_family_scores_data, "Data/formatted_friends_and_family_data.csv", row.names=FALSE) 
  
  
}


## Wrangling staff sickness data
staff_sickness_absence_formatting<-function(data){
  
  sickness_absence_data<-read.csv(data)|>
    clean_names()|>
    mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%d/%m/%Y"))|> #Format date
    mutate(month=floor_date(effective_snapshot_date, "month"))|> #Format date to monthly
    select(-organisation_type,-effective_snapshot_date )|>
    mutate(fte_days_sick=as.numeric(fte_days_sick))|>
    mutate(fte_days_sick=ifelse(is.na(fte_days_sick),0, fte_days_sick))|>
    mutate(fte_days_available=as.numeric(fte_days_available))|>
    mutate(fte_days_available=ifelse(is.na(fte_days_available),0, fte_days_available))|>
    mutate(percent=(fte_days_sick/fte_days_available)*100)
  
  write.csv(sickness_absence_data,"Data/formatted_staff_sickness_absence.csv", row.names=FALSE )
  
}


## Wrangling healthcare acquired infections data

hcai_formatting<-function(cdiff_pre_2018, cdiff, ecoli, kleb, mssa, mrsa, p_aeruginosa){
  
  hai_cdiff_pre_2018_data<-read.csv(cdiff_pre_2018)|>
    clean_names()|>
    mutate(collection="C.difficile")|>
    rename(organisation_code=provider_code)|>
    mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%Y-%m-%d")) |>#Format date
    mutate(month = yearmonth(effective_snapshot_date))|> #Format date to monthly
    select(-count_of_cases_str, -effective_snapshot_date)
  
  hai_cdiff_data<-read.csv(cdiff)|>
    mutate(Collection="C.difficile")|>
    mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%Y-%m-%d"))#Format date
  
  hai_ecoli_data<-read.csv(ecoli)|>
    mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%d/%m/%Y")) #Format date
  
  hai_klebsiella_data<-read.csv(kleb)|>
    mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%Y-%m-%d"))#Format date
  
  hai_mssa_data<-read.csv(mssa)|>
    mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%d/%m/%Y")) #Format date
  
  hai_mrsa_data<-read.csv(mrsa)|>
    mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%d/%m/%Y")) #Format date
  
  hai_p_aeruginosa_data<-read.csv(p_aeruginosa)|>
    mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%d/%m/%Y")) #Format date
  
  
  HCAI_data<-rbind(hai_cdiff_data,  hai_klebsiella_data,hai_ecoli_data, hai_mssa_data,  hai_mrsa_data ,hai_p_aeruginosa_data )|> 
    clean_names()|>
    mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%Y-%m-%d")) |>#Format date
    mutate(month=floor_date(effective_snapshot_date, "month"))|>
    rename(count_of_cases=figure)|>
    mutate(count_of_cases=as.numeric(count_of_cases))|>
    filter(metric=="HOHA cases"|metric=="Hospital-onset"|metric=="Hospital-onset, healthcare associated")|> #Select only hospital acquired
    filter(organisation_type=="NHS Acute Trust"|organisation_type=="NHS acute trust")|>
    select(-effective_snapshot_date, -organisation_type, -metric)|>
    rbind(hai_cdiff_pre_2018_data)
  
  write.csv(HCAI_data, "Data/formatted_HCAI_data.csv", row.names=FALSE)
}



