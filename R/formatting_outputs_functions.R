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
      "Waiting time - median (in weeks)",
      "Waiting time - Total number of patients waiting",
      "Bed occupancy",
      "Emergency readmissions",
      "Cleaning costs",
      "Deaths in hospital",
      "Falls, fractures and injuries in hospital",
      "Healthcare acquaired infections - combined rate",
      "Healthcare acquaired infections - C.Difficile",
      "Patient experience - Positive responses to Friends and Family Test",
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