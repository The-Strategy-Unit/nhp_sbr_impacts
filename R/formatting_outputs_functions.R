# These are functions used to format tables for reports.

# Used to format the table for the control pools.
format_controls_table <- function(data, org_code) {
  table <- data |>
    filter(organisation_code == org_code) |>
    select(
      "Provider" = trust_name,
      "Provider code" = matching_organisation_code,
      "Site" = site_name,
      "Site code" = site_code
    ) |>
    as_flextable(
      hide_grouplabel = TRUE,
      max_row = 20,
      show_coltype = FALSE
    ) |>
    align(part = "header", align = "center") |>
    bg(bg = "#f9bf07", part = "header") |>
    bold(bold = TRUE, part = "header") |>
    fontsize(size = 12, part = "all") |>
    padding(padding = 2,
            part = "all",
            padding.top = NULL) |>
    autofit()
  
  return(table)
  
}

# Used to get the table for the market matched controls for each indicator
# at provider level and hospital.
get_market_matched_controls_table_provider <- function(hospitals,
                                                       controls,
                                                       indicator) {
  # start with blank dataframe:
  market_matched_controls <- data.frame()
  
  # add in control sites for each hospital for given indicator:
  for (i in tar_objects(c(starts_with(indicator)) &
                        !ends_with("output"))) {
    market_matched_controls <- market_matched_controls |>
      rbind(
        data.frame("matching_organisation_code" = tar_read_raw(i)$ControlName) |>
          mutate(organisation_code = str_sub(i, start = -3))
      )
    
  }
  
  # create table by joining to controls and hospitals to get names
  market_matched_controls <- market_matched_controls |>
    left_join(controls,
              c("matching_organisation_code", "organisation_code")) |>
    left_join(hospitals |>
                select(name, organisation_code),
              "organisation_code") |>
    select(name,
           "Provider" = trust_name,
           "Provider code" = matching_organisation_code)  |>
    format_market_matched_controls_table()
  
  return(market_matched_controls)
  
}

# Used to get the table for the market matched controls for each indicator
# at site level and hospital.
get_market_matched_controls_table_site <- function(hospitals,
                                                   controls,
                                                   indicator) {
  # start with blank dataframe:
  market_matched_controls <- data.frame()
  
  # add in control sites for each hospital for given indicator:
  for (i in tar_objects(c(starts_with(indicator)) &
                        !ends_with("output"))) {
    market_matched_controls <- market_matched_controls |>
      rbind(
        data.frame("matching_site_code" = tar_read_raw(i)$ControlName) |>
          mutate(organisation_code = str_sub(i, start = -3))
      )
    
  }
  
  # create table by joining to controls and hospitals to get names
  market_matched_controls <- market_matched_controls |>
    left_join(controls,
              c("matching_site_code" = "site_code", "organisation_code")) |>
    left_join(hospitals |>
                select(name, organisation_code),
              "organisation_code") |>
    select(
      name,
      "Provider" = trust_name,
      "Provider code" = matching_organisation_code,
      "Site" = site_name,
      "Site code" = matching_site_code
    )  |>
    format_market_matched_controls_table()
  
  return(market_matched_controls)
  
}

# Used to format the market matched controls.
format_market_matched_controls_table <- function(data) {
  data <- data |>
    arrange(name, Provider) |>
    as_grouped_data(group = "name") |>
    as_flextable(
      hide_grouplabel = TRUE,
      max_row = 20,
      show_coltype = FALSE
    ) |>
    align(part = "header", align = "center") |>
    bg(bg = "#f9bf07", part = "header") |>
    bold(bold = TRUE, part = "header") |>
    bold(j = 1,
         part = "body",
         i = ~ !is.na(name)) |>
    fontsize(size = 12, part = "all") |>
    padding(padding = 2,
            part = "all",
            padding.top = NULL) |>
    autofit()
  
  return(data)
  
}

# Used so titles are consistent across whole document
get_title <- function(indicator) {
  titles <- data.frame(
    stringsAsFactors = FALSE,
    short_name = c(
      "los",
      "wait_time_median",
      "wait_time_number",
      "bed_occup",
      "readmissions",
      "cleaning",
      "deaths",
      "falls_fractures",
      "hcai",
      "cdiff",
      "friends_family",
      "staff_sickness",
      "staff_turnover",
      "staff_survey"
    ),
    title = c(
      "Length of stay",
      "Median waiting time",
      "Waiting list size",
      "Bed occupancy",
      "Emergency readmissions",
      "Cleaning costs",
      "Deaths in hospital",
      "Falls and injuries in hospital",
      "Healthcare acquired infections - combined rate",
      "Healthcare acquired c.difficile infections",
      "Patient experience - Friends and Family Test",
      "Staff sickness",
      "Staff turnover",
      "Staff survey"
    )
  )
  
  title <- titles |>
    filter(short_name == indicator) |>
    pull(title)
  
  return(title)
}# These are functions used to format tables for reports.

# Used to format the table for the control pools.
format_controls_table <- function(data, org_code) {
  table <- data |>
    filter(organisation_code == org_code) |>
    select(
      "Provider" = trust_name,
      "Provider code" = matching_organisation_code,
      "Site" = site_name,
      "Site code" = site_code
    ) |>
    as_flextable(
      hide_grouplabel = TRUE,
      max_row = 20,
      show_coltype = FALSE
    ) |>
    align(part = "header", align = "center") |>
    bg(bg = "#f9bf07", part = "header") |>
    bold(bold = TRUE, part = "header") |>
    fontsize(size = 12, part = "all") |>
    padding(padding = 2,
            part = "all",
            padding.top = NULL) |>
    autofit()
  
  return(table)
  
}

# Used to get the table for the market matched controls for each indicator
# at provider level and hospital.
get_market_matched_controls_table_provider <- function(hospitals,
                                                       controls,
                                                       indicator) {
  # start with blank dataframe:
  market_matched_controls <- data.frame()
  
  # add in control sites for each hospital for given indicator:
  for (i in tar_objects(c(starts_with(indicator)) &
                        !ends_with("output"))) {
    market_matched_controls <- market_matched_controls |>
      rbind(
        data.frame("matching_organisation_code" = tar_read_raw(i)$ControlName) |>
          mutate(organisation_code = str_sub(i, start = -3))
      )
    
  }
  
  # create table by joining to controls and hospitals to get names
  market_matched_controls <- market_matched_controls |>
    left_join(controls,
              c("matching_organisation_code", "organisation_code")) |>
    left_join(hospitals |>
                select(name, organisation_code),
              "organisation_code") |>
    select(name,
           "Provider" = trust_name,
           "Provider code" = matching_organisation_code)  |>
    format_market_matched_controls_table()
  
  return(market_matched_controls)
  
}

# Used to get the table for the market matched controls for each indicator
# at site level and hospital.
get_market_matched_controls_table_site <- function(hospitals,
                                                   controls,
                                                   indicator) {
  # start with blank dataframe:
  market_matched_controls <- data.frame()
  
  # add in control sites for each hospital for given indicator:
  for (i in tar_objects(c(starts_with(indicator)) &
                        !ends_with("output"))) {
    market_matched_controls <- market_matched_controls |>
      rbind(
        data.frame("matching_site_code" = tar_read_raw(i)$ControlName) |>
          mutate(organisation_code = str_sub(i, start = -3))
      )
    
  }
  
  # create table by joining to controls and hospitals to get names
  market_matched_controls <- market_matched_controls |>
    left_join(controls,
              c("matching_site_code" = "site_code", "organisation_code")) |>
    left_join(hospitals |>
                select(name, organisation_code),
              "organisation_code") |>
    select(
      name,
      "Provider" = trust_name,
      "Provider code" = matching_organisation_code,
      "Site" = site_name,
      "Site code" = matching_site_code
    )  |>
    format_market_matched_controls_table()
  
  return(market_matched_controls)
  
}

# Used to format the market matched controls.
format_market_matched_controls_table <- function(data) {
  data <- data |>
    arrange(name, Provider) |>
    as_grouped_data(group = "name") |>
    as_flextable(
      hide_grouplabel = TRUE,
      max_row = 20,
      show_coltype = FALSE
    ) |>
    align(part = "header", align = "center") |>
    bg(bg = "#f9bf07", part = "header") |>
    bold(bold = TRUE, part = "header") |>
    bold(j = 1,
         part = "body",
         i = ~ !is.na(name)) |>
    fontsize(size = 12, part = "all") |>
    padding(padding = 2,
            part = "all",
            padding.top = NULL) |>
    autofit()
  
  return(data)
  
}

# Used so titles are consistent across whole document
get_title <- function(indicator) {
  titles <- data.frame(
    stringsAsFactors = FALSE,
    short_name = c(
      "los",
      "wait_time_median",
      "bed_occup",
      "readmissions",
      "cleaning",
      "deaths",
      "falls_fractures",
      "hcai",
      "cdiff",
      "friends_family",
      "staff_sickness",
      "staff_turnover",
      "staff_survey"
    ),
    title = c(
      "Length of stay",
      "Median waiting time",
      "Bed occupancy",
      "Emergency readmissions",
      "Cleaning costs",
      "Deaths in hospital",
      "Falls and injuries in hospital",
      "Healthcare acquired infections - combined rate",
      "Healthcare acquired c.difficile infections",
      "Patient experience - Friends and Family Test",
      "Staff sickness",
      "Staff turnover",
      "Staff survey"
    )
  )
  
  title <- titles |>
    filter(short_name == indicator) |>
    pull(title)
  
  return(title)
}

get_sbr_details <- function(data){
  data <- data |>
    dplyr::mutate(
      pre_sbr_perc = round(pre_sbr * 100 / post_all, 2),
      post_sbr_perc = round(post_sbr * 100 / post_all, 2),
      date = format(date, "%b %Y"))
  
  return(data)
}

get_sbr_details_table <- function(data){
  table <- data |>
    dplyr::select(
      "Hospital" = name,
      "Switch month" = date,
      "SBR% before switch" = pre_sbr_perc,
      "SBR% after switch" = post_sbr_perc
    ) |>
    na.omit() |>
    flextable::as_flextable(
      hide_grouplabel = TRUE,
      max_row = 20,
      show_coltype = FALSE
    ) |>
    flextable::align(part = "header", align = "center") |>
    flextable::bg(bg = "#f9bf07", part = "header") |>
    flextable::bold(bold = TRUE, part = "header") |>
    flextable::fontsize(size = 12, part = "all") |>
    flextable::padding(padding = 2,
                       part = "all",
                       padding.top = NULL) |>
    flextable::autofit()
  
  return(table)
}

get_number_control_sites <- function(data){
  data <- data |>
    dplyr::filter(rank_of_ranks <= 20) |>
    dplyr::summarise(n(), .by = organisation_code)
  
  return(data)
}