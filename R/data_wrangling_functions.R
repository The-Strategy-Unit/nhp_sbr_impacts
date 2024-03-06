#### General functions ####
# To get data from a xls file at a URL. Default is to read sheet 1, but can
# specify other sheet.
scrape_xls <- function(url, sheet = 1, skip = 0) {
  tmp = tempfile(fileext = "")
  
  download.file(url = url,
                destfile = tmp,
                mode = "wb")
  
  data <-
    readxl::read_excel(path = tmp,
                       sheet = sheet,
                       skip = skip) |>
    janitor::clean_names()
  
  return(data)
  
}

# To read in the csv files of the outputs of sql queries and to apply common
# data manipulations:
read_sql_output <- function(filename) {
  data <- read.csv(filename) |>
    janitor::clean_names() |>
    dplyr::mutate(effective_snapshot_date = as.Date(effective_snapshot_date,
                                                    format = "%d/%m/%Y"))
  
  return(data)
}

# To standardise the staff groups across data sources.
# There are probably some more groups that can be matched between workforce
# data from UDAL and from NHS-D and the turnover data from UDAL.
standardise_staff_group <- function(staff_group) {
  staff_group <- staff_group |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(
      c(
        " [[:punct:]]" = "",
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
wrangle_bed_occupancy <- function(bed_occupancy_file) {
  data <- read_sql_output(bed_occupancy_file) |>
    dplyr::mutate(bed_occupancy = as.numeric(bed_occupancy))
  
  return(data)
  
}

#-----------------------------------------------------------------------------#

#### ERIC functions ####
# To wrangle the eric data from sql and combine the two files:
wrangle_eric <- function(eric_09_15, eric_16_23) {
  data <- read.csv(eric_09_15) |>
    dplyr::bind_rows(read.csv(eric_16_23)) |>
    get_single_bedrooms() |>
    dplyr::mutate(
      value = as.numeric(value),
      effective_snapshot_date = as.Date(effective_snapshot_date,
                                        format = "%d/%m/%Y")
    )
  
  return(data)
}

# To calculate the total number of single bedrooms in eric UDAL data.
get_single_bedrooms <- function(data) {
  data <- data |>
    dplyr::filter(grepl("Single", measure)) |>
    tidyr::pivot_wider(names_from = "measure", values_from = "value") |>
    dplyr::mutate(
      across(contains("Single"), as.numeric),
      total_single_bedrooms =
        `Single bedrooms for patients with en-suite facilities (No.)` +
        `Single bedrooms for patients without en-suite facilities (No.)`
    ) |>
    tidyr::pivot_longer(names_to = "measure",
                        values_to = "value",
                        cols = total_single_bedrooms) |>
    dplyr::select(-contains("Single")) |>
    rbind(data)
  
  return(data)
  
}

# The number single bedrooms for 2009-03-31 and 2010-03-31 are not in UDAL, but
# can be calculated from fields in NHS-Digital files.
get_single_bedrooms_for_2009_10 <- function(year) {
  years <- paste0(year,
                  stringr::str_sub(year + 1, 3, 4))
  
  url <-
    stringr::str_replace(
      "https://files.digital.nhs.uk/C7/22BBC6/ERIC-YEARS-Data.xls",
      "YEARS",
      "200910"
    )
  
  data <- scrape_xls(url, "Site Data", skip = 1) |>
    dplyr::mutate(
      effective_snapshot_date = as.Date(stringr::str_replace(
        "YEAR-03-31",
        "YEAR",
        as.character(year)
      )),
      level = "Site",
      total_single_bedrooms = as.numeric(available_beds_no) *
        as.numeric(percentage_of_single_bedrooms_for_patients_percent)
      / 100
    ) |>
    tidyr::pivot_longer(names_to = "measure",
                        values_to = "value",
                        cols = total_single_bedrooms) |>
    dplyr::select(
      effective_snapshot_date,
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

# To combine the eric data from sql/UDAL with the data from NHS-Digital:
combine_eric_data <- function(eric_udal, eric_09, eric_10) {
  data <- eric_udal |>
    dplyr::bind_rows(eric_09,
                     eric_10) |>
    dplyr::mutate(
      site_type = str_replace_all(site_type, "[:digit:]. ", "") |>
        str_to_lower(),
      organisation_type = str_to_lower(organisation_type),
      month = effective_snapshot_date |>
        as.Date("%Y-%m-%d") |> #Format date to monthly
        floor_date("month")
    ) |>
    tidyr::pivot_wider(names_from = "measure", values_from = "value") |>
    janitor::clean_names() |>
    dplyr::relocate(month)
  
  return(data)
  
}

#-----------------------------------------------------------------------------#

#### SHMI functions ####
# To wrangle the SHMI data from sql:
wrangle_shmi <- function(shmi_file) {
  data <- read_sql_output(shmi_file) |>
    dplyr::mutate(across(c(shmi_value, observed, expected, spells), as.numeric)) |>
    dplyr::rename("organisation_code" = provider_code)
  
  return(data)
  
}

#-----------------------------------------------------------------------------#

#### Turnover functions ####
# To wrangle the turnover data from sql:
wrangle_turnover <- function(turnover_file) {
  data <- read_sql_output("Data/sql_turnover.csv") |>
    dplyr::rename(
      "organisation_code" = org_code,
      "turnover_headcount" = head_count,
      "turnover_fte" = fte
    ) |>
    dplyr::mutate(staff_group = standardise_staff_group(staff_group))
  
  
  return(data)
  
}

#-----------------------------------------------------------------------------#

#### Workforce functions ####
# To wrangle the workforce data. Sheetnames and where data is kept in the Excel
# files changes over time, so contains an if statement to setup variables,
# before scraping and wrangling data.
wrangle_workforce <- function(month, workforce_links) {
  skip <- 6
  
  if (month >= as.Date("2016-01-31") &
      month < as.Date("2016-05-31")) {
    sheetname_hc <- "1. HEE Org Main Staff Gp HC"
    sheetname_fte <- "2. HEE Org Main Staff Gp FTE "
    skip <- 3
    
  } else if (month == as.Date("2016-05-31")) {
    sheetname_hc <- "1. HEE Org Main Staff Gp HC"
    sheetnumber <- substr(sheetname_hc, 1, 1)
    sheetname_fte <- stringr::str_replace_all(sheetname_hc,
                                              c(
                                                "HC" = "FTE",
                                                "[[:digit:]]" =
                                                  as.numeric(sheetnumber) + 1
                                              ))
    skip <- 4
    
  } else if (month == as.Date("2016-06-30")) {
    sheetname_hc <- "1. HEE Org Main Staff Gp HC "
    sheetname_fte <- "2. HEE Org Main Staff Gp FTE"
    skip <- 4
    
  } else if (month >= as.Date("2016-07-31") &
             month < as.Date("2017-03-31")) {
    sheetname_hc <- "1. HEE Org Main Staff Gp HC "
    sheetname_fte <- "2. HEE Org Main Staff Gp FTE"
    
  } else if (month >= as.Date("2017-03-31") &
             month < as.Date("2019-03-31")) {
    sheetname_hc <- "1. HEE Org Main Staff Gp HC"
    sheetnumber <- substr(sheetname_hc, 1, 1)
    sheetname_fte <- stringr::str_replace_all(sheetname_hc,
                                              c(
                                                "HC" = "FTE",
                                                "[[:digit:]]" =
                                                  as.numeric(sheetnumber) + 1
                                              ))
    
  } else if (month >= as.Date("2019-03-31") &
             month < as.Date("2019-07-31")) {
    sheetname_hc <- "2. HEE, Org & SG - HC"
    sheetnumber <- substr(sheetname_hc, 1, 1)
    sheetname_fte <- stringr::str_replace_all(sheetname_hc,
                                              c(
                                                "HC" = "FTE",
                                                "[[:digit:]]" =
                                                  as.numeric(sheetnumber) + 1
                                              ))
    
  } else {
    print("Please enter a month end between 2016-01-31 and 2019-06-30.")
    
  }
  
  url <- workforce_links |>
    dplyr::filter(date == as.Date(month)) |>
    dplyr::pull(url)
  
  data_hc <- scrape_xls(url,
                        sheet = sheetname_hc,
                        skip = skip) |>
    dplyr::mutate(data_type = "HC")
  
  data_fte <- scrape_xls(url,
                         sheet = sheetname_fte,
                         skip = skip) |>
    dplyr::mutate(data_type = "FTE")
  
  data <- data_hc |>
    dplyr::bind_rows(data_fte) |>
    dplyr::rename(org_code = x4) |>
    dplyr::select(-starts_with("x")) |>
    dplyr::filter(!is.na(org_code)) |>
    dplyr::mutate(across(-c(org_code, data_type), as.numeric)) |>
    tidyr::pivot_longer(
      names_to = "staff_group",
      values_to = "total",
      cols = -c(org_code, data_type)
    ) |>
    dplyr::mutate(effective_snapshot_date = as.Date(month))
  
  return(data)
  
}

# To combine and finish formatting the workforce data:
combine_workforce <- function(workforce_udal, workforce_links) {
  workforce <- workforce_udal |>
    dplyr::mutate(effective_snapshot_date = as.Date(effective_snapshot_date,
                                                    format = "%d/%m/%Y")) |>
    dplyr::bind_rows(
      wrangle_workforce("2016-01-31", workforce_links),
      wrangle_workforce("2016-02-29", workforce_links),
      wrangle_workforce("2016-03-31", workforce_links),
      wrangle_workforce("2016-04-30", workforce_links),
      wrangle_workforce("2016-05-31", workforce_links),
      wrangle_workforce("2016-06-30", workforce_links),
      wrangle_workforce("2016-07-31", workforce_links),
      wrangle_workforce("2016-08-31", workforce_links),
      # wrangle_workforce("2016-09-30", workforce_links), # Sept already available
      wrangle_workforce("2016-10-31", workforce_links),
      wrangle_workforce("2016-11-30", workforce_links),
      wrangle_workforce("2016-12-31", workforce_links),
      
      wrangle_workforce("2017-01-31", workforce_links),
      wrangle_workforce("2017-02-28", workforce_links),
      
      wrangle_workforce("2017-03-31", workforce_links),
      wrangle_workforce("2017-04-30", workforce_links),
      wrangle_workforce("2017-05-31", workforce_links),
      wrangle_workforce("2017-06-30", workforce_links),
      wrangle_workforce("2017-07-31", workforce_links),
      wrangle_workforce("2017-08-31", workforce_links),
      # wrangle_workforce("2017-09-30", workforce_links), # Sept already available
      wrangle_workforce("2017-10-31", workforce_links),
      wrangle_workforce("2017-11-30", workforce_links),
      wrangle_workforce("2017-12-31", workforce_links),
      
      wrangle_workforce("2018-01-31", workforce_links),
      wrangle_workforce("2018-02-28", workforce_links),
      wrangle_workforce("2018-03-31", workforce_links),
      wrangle_workforce("2018-04-30", workforce_links),
      wrangle_workforce("2018-05-31", workforce_links),
      wrangle_workforce("2018-06-30", workforce_links),
      wrangle_workforce("2018-07-31", workforce_links),
      wrangle_workforce("2018-08-31", workforce_links),
      #  wrangle_workforce("2018-09-30", workforce_links), # Sept already available
      wrangle_workforce("2018-10-31", workforce_links),
      wrangle_workforce("2018-11-30", workforce_links),
      wrangle_workforce("2018-12-31", workforce_links),
      
      wrangle_workforce("2019-01-31", workforce_links),
      wrangle_workforce("2019-02-28", workforce_links),
      wrangle_workforce("2019-03-31", workforce_links),
      wrangle_workforce("2019-04-30", workforce_links),
      wrangle_workforce("2019-05-31", workforce_links),
      wrangle_workforce("2019-06-30", workforce_links)
    ) |>
    dplyr::mutate(staff_group = standardise_staff_group(staff_group)) |>
    tidyr::pivot_wider(names_from = data_type, values_from = total) |>
    dplyr::rename(
      "organisation_code" = org_code,
      "workforce_headcount" = HC,
      "workforce_fte" = FTE
    )
  
}

#-----------------------------------------------------------------------------#
## Wrangling RTT waiting times
rtt_data_formatting <- function(data) {
  #Functions to pull xls files for RTT waiting times prior to April 2011
  
  read_rtt <- function(url, name) {
    tmp = tempfile(fileext = "")
    
    download.file(url = url,
                  destfile = tmp,
                  mode = "wb")
    df <-
      read_excel(tmp, sheet = "Provider", range = cell_limits(c(14, 3), c(NA, NA))) |>
      select(1, 3, 5:57) |>
      gather(key = number_of_weeks_since_referral, value = number_of_incomplete_pathways, -1, -2) |>
      mutate(month = name) |>
      clean_names() |>
      rename(organisation_code = org_code)
    
    assign(name, df, envir = .GlobalEnv)
    
  }
  
  
  read_rtt2 <- function(url, name) {
    tmp = tempfile(fileext = "")
    
    download.file(url = url,
                  destfile = tmp,
                  mode = "wb")
    df <-
      read_excel(tmp, sheet = "Providers", range = cell_limits(c(6, 2), c(NA, NA))) |>
      select(1, 3, 5:57) |>
      gather(key = number_of_weeks_since_referral, value = number_of_incomplete_pathways, -1, -2) |>
      mutate(month = name) |>
      clean_names() |>
      rename(organisation_code = code)
    
    assign(name, df, envir = .GlobalEnv)
    
  }
  
  read_rtt3 <- function(url, name) {
    tmp = tempfile(fileext = "")
    
    download.file(url = url,
                  destfile = tmp,
                  mode = "wb")
    df <-
      read_excel(tmp, sheet = "Providers", range = cell_limits(c(7, 2), c(NA, NA))) |>
      select(1, 3, 5:57) |>
      gather(key = number_of_weeks_since_referral, value = number_of_incomplete_pathways, -1, -2) |>
      mutate(month = name) |>
      clean_names() |>
      rename(organisation_code = code)
    
    assign(name, df, envir = .GlobalEnv)
    
  }
  
  read_rtt4 <- function(url, name) {
    tmp = tempfile(fileext = "")
    
    download.file(url = url,
                  destfile = tmp,
                  mode = "wb")
    df <-
      read_excel(tmp, sheet = "Providers", range = cell_limits(c(9, 2), c(NA, NA))) |>
      select(1, 3, 5:57) |>
      gather(key = number_of_weeks_since_referral, value = number_of_incomplete_pathways, -1, -2) |>
      mutate(month = name) |>
      clean_names() |>
      rename(organisation_code = code)
    
    assign(name, df, envir = .GlobalEnv)
    
  }
  
  
  #2010-2011
  read_rtt(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130104202122mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_126945.xls",
    "Mar 2011"
  )
  read_rtt(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130104202122mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_128319.xls",
    "Feb 2011"
  )
  read_rtt(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130104202122mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_128312.xls",
    "Jan 2011"
  )
  read_rtt(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_128301.xls",
    "Dec 2010"
  )
  read_rtt3(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_123623.xls",
    "Nov 2010"
  )
  read_rtt3(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_122781.xls",
    "Oct 2010"
  )
  read_rtt3(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_121819.xls",
    "Sep 2010"
  )
  read_rtt3(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_132303.xls",
    "Aug 2010"
  )
  read_rtt3(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_119386.xls",
    "Jul 2010"
  )
  read_rtt3(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_118704.xls",
    "Jun 2010"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_117449.xls",
    "May 2010"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_123542.xls",
    "Apr 2010"
  )
  
  
  #2009-2010
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_132297.xls",
    "Mar 2010"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_115407.xls",
    "Feb 2010"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_114103.xls",
    "Jan 2010"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_112615.xls",
    "Dec 2009"
  )
  read_rtt4(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_111339.xls",
    "Nov 2009"
  )
  read_rtt4(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_110153.xls",
    "Oct 2009"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_108744.xls",
    "Sep 2009"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_110194.xls",
    "Aug 2009"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_132304.xls",
    "Jul 2009"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_110211.xls",
    "Jun 2009"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_110222.xls",
    "May 2009"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_110218.xls",
    "Apr 2009"
  )
  
  #2008-2009
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_099876.xls",
    "Mar 2009"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_100042.xls",
    "Feb 2009"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_099989.xls",
    "Jan 2009"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020040mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_095414.xls",
    "Dec 2008"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020040mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_102155.xls",
    "Nov 2008"
  )
  read_rtt2(
    "https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020040mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_102163.xls",
    "Oct 2008"
  )
  
  # merge pre 2011 files together
  pre2011_rtt_data <-
    rbind(
      `Mar 2011`,
      `Feb 2011`,
      `Jan 2011`,
      `Dec 2010`,
      `Nov 2010`,
      `Oct 2010`,
      `Sep 2010`,
      `Aug 2010`,
      `Jul 2010`,
      `Jun 2010`,
      `May 2010`,
      `Apr 2010`,
      `Mar 2010`,
      `Feb 2010`,
      `Jan 2010`,
      `Dec 2009`,
      `Nov 2009`,
      `Oct 2009`,
      `Sep 2009`,
      `Aug 2009`,
      `Jul 2009`,
      `Jun 2009`,
      `May 2009`,
      `Apr 2009`,
      `Mar 2009`,
      `Feb 2009`,
      `Jan 2009`,
      `Dec 2008`,
      `Nov 2008`,
      `Oct 2008`
    )
  
  pre2011_rtt_data <- pre2011_rtt_data |>
    mutate(month = zoo::as.yearmon(month, format = "%b %Y")) |> #Format date
    mutate(month = as.Date(month, frac = 0)) |> #Format date to monthly
    mutate(weeks = gsub("^>", "", number_of_weeks_since_referral)) |>
    mutate(weeks = sub("\\-.*", "", weeks)) |>
    mutate(weeks = sub("\\plus.*", "", weeks)) |>
    mutate(weeks = as.numeric(weeks)) |>
    mutate(number_of_incomplete_pathways_with_dta = NA) |>
    mutate(treatment_function_code = gsub("^IP", "", treatment_function_code))
  
  assign("pre2011_rtt_data", pre2011_rtt_data, envir = .GlobalEnv)
  
  # RTT Waiting times April 2011 to Oct 2023
  rtt_data <- read.csv(data) |>
    clean_names() |>
    mutate(effective_snapshot_date = as.Date(effective_snapshot_date, "%Y-%m-%d")) |> #Format date
    mutate(month = floor_date(effective_snapshot_date, "month")) |> #Format date to monthly
    mutate(weeks = gsub("^>", "", number_of_weeks_since_referral)) |>
    mutate(weeks = sub("\\-.*", "", weeks)) |>
    mutate(weeks = sub("\\+.*", "", weeks)) |>
    mutate(weeks = as.numeric(weeks)) |>
    select(-effective_snapshot_date)
  
  assign("rtt_data", rtt_data, envir = .GlobalEnv)
  
  
  formatted_rtt_data <- rbind(rtt_data, pre2011_rtt_data) |>
    filter(treatment_function_code == "999")#999 is the total for each provider
  
  
  return(formatted_rtt_data)
  
}


## Wrangling friends and family inpatient scores
friends_and_family_scores_data_formatting <-
  function(data1, data2) {
    #pre 2022-07 data
    friends_and_family_scores_data1 <- read.csv(data1) |>
      clean_names() |>
      mutate(effective_snapshot_date = as.Date(effective_snapshot_date, "%Y-%m-%d")) |> #Format date
      mutate(month = floor_date(effective_snapshot_date, "month")) |> #Format date to monthly
      mutate(count = ifelse(count == "NULL", 0, count)) |>
      mutate(count = as.numeric(count)) |>
      mutate(
        positive_responses = ifelse(
          likely_to_recommend == "Very Good" | likely_to_recommend == "Good" |
            likely_to_recommend == "Likely" |
            likely_to_recommend == "Extremely Likely" ,
          "yes",
          "no"
        )
      ) |> #flag those that are +ive responses
      filter(grouped_by == "Site") |>
      group_by(month, site_code) |>
      mutate(percent = round(((count / sum(
        count
      )) * 100), 1)) |> #calculate %
      filter(positive_responses == "yes") |>
      summarise(percent = sum(percent)) #calculate %
    
    #Post 2022-07 data
    friends_and_family_scores_data2 <- read.csv(data2) |>
      clean_names() |>
      mutate(effective_snapshot_date = as.Date(effective_snapshot_date, "%d/%m/%Y")) |> #Format date
      mutate(month = floor_date(effective_snapshot_date, "month")) |> #Format date to monthly
      filter(measure_category == "Percentage Positive") |>
      mutate(percent = as.numeric(measure_value) * 100) |>
      select(-effective_snapshot_date,
             -measure_category,
             -measure_value,
             -measure_name) |>
      filter(month >= '2022-08-01')
    
    friends_and_family_scores_data <-
      rbind(friends_and_family_scores_data1,
            friends_and_family_scores_data2)
    
    return(friends_and_family_scores_data)
    
    
  }


## Wrangling staff sickness data
staff_sickness_absence_formatting <- function(data) {
  sickness_absence_data <- read.csv(data) |>
    clean_names() |>
    mutate(effective_snapshot_date = as.Date(effective_snapshot_date, "%d/%m/%Y")) |> #Format date
    mutate(month = floor_date(effective_snapshot_date, "month")) |> #Format date to monthly
    select(-organisation_type, -effective_snapshot_date) |>
    mutate(fte_days_sick = as.numeric(fte_days_sick)) |>
    mutate(fte_days_sick = ifelse(is.na(fte_days_sick), 0, fte_days_sick)) |>
    mutate(fte_days_available = as.numeric(fte_days_available)) |>
    mutate(fte_days_available = ifelse(is.na(fte_days_available), 0, fte_days_available)) |>
    mutate(percent = (fte_days_sick / fte_days_available) * 100)
  
  return(sickness_absence_data)
  
}


## Wrangling healthcare acquired infections data

hcai_formatting <-
  function(cdiff_pre_2018,
           cdiff,
           ecoli,
           kleb,
           mssa,
           mrsa,
           p_aeruginosa) {
    hai_cdiff_pre_2018_data <- read.csv(cdiff_pre_2018) |>
      clean_names() |>
      mutate(collection = "C.difficile") |>
      rename(organisation_code = provider_code) |>
      mutate(effective_snapshot_date = as.Date(effective_snapshot_date, "%Y-%m-%d")) |>
      #Format date
      mutate(month = yearmonth(effective_snapshot_date)) |> #Format date to monthly
      select(-count_of_cases_str, -effective_snapshot_date)
    
    hai_cdiff_data <- read.csv(cdiff) |>
      mutate(Collection = "C.difficile") |>
      mutate(Effective_Snapshot_Date = as.Date(Effective_Snapshot_Date, "%Y-%m-%d"))#Format date
    
    hai_ecoli_data <- read.csv(ecoli) |>
      mutate(Effective_Snapshot_Date = as.Date(Effective_Snapshot_Date, "%d/%m/%Y")) #Format date
    
    hai_klebsiella_data <- read.csv(kleb) |>
      mutate(Effective_Snapshot_Date = as.Date(Effective_Snapshot_Date, "%Y-%m-%d"))#Format date
    
    hai_mssa_data <- read.csv(mssa) |>
      mutate(Effective_Snapshot_Date = as.Date(Effective_Snapshot_Date, "%d/%m/%Y")) #Format date
    
    hai_mrsa_data <- read.csv(mrsa) |>
      mutate(Effective_Snapshot_Date = as.Date(Effective_Snapshot_Date, "%d/%m/%Y")) #Format date
    
    hai_p_aeruginosa_data <- read.csv(p_aeruginosa) |>
      mutate(Effective_Snapshot_Date = as.Date(Effective_Snapshot_Date, "%d/%m/%Y")) #Format date
    
    
    HCAI_data <- rbind(
      hai_cdiff_data,
      hai_klebsiella_data,
      hai_ecoli_data,
      hai_mssa_data,
      hai_mrsa_data ,
      hai_p_aeruginosa_data
    ) |>
      clean_names() |>
      mutate(effective_snapshot_date = as.Date(effective_snapshot_date, "%Y-%m-%d")) |>
      #Format date
      mutate(month = floor_date(effective_snapshot_date, "month")) |>
      rename(count_of_cases = figure) |>
      mutate(count_of_cases = as.numeric(count_of_cases)) |>
      filter(
        metric == "HOHA cases" |
          metric == "Hospital-onset" |
          metric == "Hospital-onset, healthcare associated"
      ) |> #Select only hospital acquired
      filter(organisation_type == "NHS Acute Trust" |
               organisation_type == "NHS acute trust") |>
      select(-effective_snapshot_date, -organisation_type, -metric) |>
      rbind(hai_cdiff_pre_2018_data)
    
    return(HCAI_data)
  }

##load the beddays csv file

fun_load_beddays <- function(filepth) {
  data <- read.csv(filepth) |>
    clean_names() |>
    mutate(yearmon = lubridate::ym(period))
  
  return(data)
  
}

##load the inpatient activity csv file, clean and calculate row variables

fun_load_sus_apcs <- function(filepth, beddays) {
  data <- read.csv(filepth) |>
    clean_names() |>
    mutate(
      los = as.integer(str_replace(los, "NULL", "0"))
      ,
      mon2 = case_when(mth < 10 ~ paste0("0", as.factor(mth))
                       , TRUE ~ as.factor(mth))
      ,
      yearmon = lubridate::ym(paste0(as.factor(yr), mon2))
    ) |>
    select(-mon2) |>
    left_join(
      beddays |> select(der_provider_site_code, yearmon, beddays),
      by = c("der_provider_site_code", "yearmon")
    ) |>
    mutate(
      beddays = if_else(beddays < 0, 0, beddays)
      ,
      avg_los = los / spells
      ,
      ff_rate = fall_fracs / beddays * 1000
      ,
      death_rate = deaths / spells * 100
    )
  
  return(data)
}

##load the cost file yearly csv

fun_load_sus_cost_yr <- function(filepth) {
  data <- read.csv(filepth) |>
    clean_names()
  
  return(data)
}

##load the cost file monthly csv

fun_load_sus_cost_mth <- function(filepth) {
  data <- read.csv(filepth) |>
    clean_names()
  
  return(data)
}

##load the readmissions monthly csv

fun_load_sus_readmit <- function(filepth) {
  data <- read.csv(filepth) |>
    clean_names()
  
  return(data)
}

##load the separate LoS monthly csv

fun_load_sus_los <- function(filepth) {
  data <- read.csv(filepth) |>
    clean_names()
  
  return(data)
}

##load the falls and injuries data

fun_load_sus_falls <- function(filepth) {
  data <- read.csv(filepth) |>
    clean_names()
  
  return(data)
}
