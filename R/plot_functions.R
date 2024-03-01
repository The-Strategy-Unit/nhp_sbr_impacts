## Plot functions

# To give nicer y axis labels for the indicator plots:
get_y_axis_for_indicator_plots <- function(plotting_variable) {
  if (plotting_variable == "bed_occupancy") {
    y_axis <- "Bed Occupancy (%)"
  }
  
  if (plotting_variable == "leaving_rate") {
    y_axis <- "Staff Leaving Rate (%)"
  }
  
  if (plotting_variable == "cleaning_staff_wte") {
    y_axis <- "Cleaning Staff WTE"
  }
  
  if (plotting_variable == "staff_sickness_percent") {
    y_axis <- "Staff sickness absence (%)"
  }
  
  if (plotting_variable == "friends_and_family_percent") {
    y_axis <- "Positive responses (%)"
  }
  
  if (plotting_variable == "combined_rate") {
    y_axis <- "Combined rate of HCAI"
  }
  
  if (plotting_variable == "ff_rate") {
    y_axis <- "Rate of falls and fractures over beddays"
  }
  
  if (plotting_variable == "death_rate") {
    y_axis <- "Number of Hospital Deaths per 1000 Admissions"
  }
  
  if (plotting_variable == "median_by_prov") {
    y_axis <- "Median RTT waiting time (weeks)"
  }
  
  if (plotting_variable == "avg_los") {
    y_axis <- "Average Length of Stay"
  }
  
  return(y_axis)
  
}

# To create the plot for an indicator:
plot_indicator <- function(site_of_interest,
                           controls,
                           plotting_variable,
                           grouping,
                           sbr_date) {
  plot <- ggplot2::ggplot(site_of_interest,
                          ggplot2::aes(month,
                                       !!sym(plotting_variable))) +
    ggplot2::geom_line() +
    ggplot2::geom_line(data = controls,
                       ggplot2::aes(month,
                                    !!sym(plotting_variable),
                                    group = !!sym(grouping)),
                       alpha = 0.1) +
    ggplot2::theme_bw() +
    ggplot2::ylab(get_y_axis_for_indicator_plots(plotting_variable)) +
    ggplot2::xlab("Month") +
    ggplot2::geom_vline(
      xintercept = as.numeric(as.Date(sbr_date)),
      col = "purple",
      linetype = "dotted"
    ) +
    ggplot2::geom_vline(
      xintercept = as.numeric(as.Date("2020-03-01")),
      col = "blue",
      linetype = "dotted"
    ) +
    ggplot2::geom_text(aes(
      x = as.Date(sbr_date),
      label = "SBR",
      y = 0
    ),
    hjust = "bottom",
    col = "purple") +
    ggplot2::geom_text(aes(
      x = as.Date("2020-03-01"),
      label = "COVID-19",
      y = 0
    ),
    col = "blue")
  
  return(plot)
  
}

# To get the plot for an indicator that is at organisation level:
get_indicator_organisation_level_plot <-
  function(hospital_of_interest,
           hospitals,
           controls,
           indicator,
           plotting_variable) {
    org_code_of_interest <- hospitals |>
      dplyr::filter(alias == hospital_of_interest) |>
      dplyr::pull(organisation_code)
    
    sbr_date <- hospitals |>
      dplyr::filter(organisation_code == org_code_of_interest) |>
      dplyr::pull(date_sbr)
    
    site_of_interest <- indicator |>
      dplyr::filter(organisation_code == org_code_of_interest)
    
    controls <- controls |>
      dplyr::filter(organisation_code == org_code_of_interest) |>
      dplyr::select(site_code, matching_organisation_code) |>
      unique() |>
      dplyr::left_join(indicator,
                       c("matching_organisation_code" = "organisation_code"))
    
    plot <- plot_indicator(
      site_of_interest,
      controls,
      plotting_variable,
      "matching_organisation_code",
      sbr_date
    )
    
    return(plot)
    
  }

# To get the plot for an indicator that is at site level:
get_indicator_site_level_plot <- function(hospital_of_interest,
                                          hospitals,
                                          controls,
                                          indicator,
                                          plotting_variable) {
  site_code_of_interest <- hospitals |>
    dplyr::filter(alias == hospital_of_interest) |>
    dplyr::pull(site_code)
  
  sbr_date <- hospitals |>
    dplyr::filter(site_code == site_code_of_interest) |>
    dplyr::pull(date_sbr)
  
  site_of_interest <- indicator |>
    dplyr::filter(site_code == site_code_of_interest)
  
  controls <- controls |>
    dplyr::filter(organisation_code ==
                    stringr::str_sub(site_code_of_interest, 1, 3)) |>
    dplyr::select(site_code) |>
    unique() |>
    dplyr::left_join(indicator, "site_code")
  
  plot <- plot_indicator(site_of_interest,
                         controls,
                         plotting_variable,
                         "site_code",
                         sbr_date)
  
  return(plot)
  
}
