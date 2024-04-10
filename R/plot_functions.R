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
  
  if (plotting_variable == "cleaning_service_cost") {
    y_axis <- "Cleaning Service Costs (£)"
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
  
  if (plotting_variable == "hosp_rate_1000") {
    y_axis <- "Number of Hospital Deaths per 1000 Admissions"
  }
  
  if (plotting_variable == "median_by_prov") {
    y_axis <- "Median RTT waiting time (weeks)"
  }
  
  if (plotting_variable == "avg_los") {
    y_axis <- "Average Length of Stay"
  }
  
  if (plotting_variable == "percentage_single_bedrooms") {
    y_axis <- "Single beds as % of all"
  }
  
  if (plotting_variable == "perc") {
    y_axis <- "Percentage of emergency readmissions"
  }
  
  return(y_axis)
  
}

# To create the plot for an indicator:
plot_indicator <- function(site_of_interest,
                           controls,
                           plotting_variable,
                           grouping,
                           sbr_date,
                           frequency) {
  covid_date <- as.Date("2020-03-01")
  
  colour_covid <- "blue"
  colour_sbr <- "orange"
  
  if (grouping == "site_code") {
    level <- "site \n level"
  } else {
    level <- "provider \n level"
  }
  
  max_y <- controls |>
    # rbind(site_of_interest) |>
    summarise(max(!!sym(plotting_variable), na.rm = TRUE)) |>
    pull()
  
  plot <- ggplot2::ggplot(site_of_interest,
                          ggplot2::aes(month,
                                       !!sym(plotting_variable))) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 0.5) +
    
    # controls
    ggplot2::geom_line(data = controls,
                       ggplot2::aes(month,
                                    !!sym(plotting_variable),
                                    group = !!sym(grouping)),
                       alpha = 0.1) +
    
    # labels
    ggplot2::ylab(get_y_axis_for_indicator_plots(plotting_variable)) +
    ggplot2::xlab("Month") +
    
    # covid and sbr annotations
    ggplot2::geom_vline(xintercept = as.numeric(as.Date(sbr_date)),
                        col = colour_sbr,
                        linetype = "dotted") +
    ggplot2::geom_vline(
      xintercept = as.numeric(covid_date),
      col = colour_covid,
      linetype = "dotted"
    ) +
    ggplot2::geom_text(aes(
      x = as.Date(sbr_date),
      label = "SBR",
      y = 0
    ),
    col = colour_sbr) +
    ggplot2::geom_text(aes(x = covid_date,
                           label = "COVID-19",
                           y = 0),
                       col = colour_covid) +
    
    # level annotation
    annotate("text",
             x = as.Date("2023-01-01"),
             y = max_y,
             label = level) +
    
    # general format
    ggplot2::scale_x_date(breaks = seq.Date(as.Date("2008-03-01"),
                                            as.Date("2023-10-01"),
                                            "year"),
                          minor_breaks = seq.Date(as.Date("2008-03-01"),
                                            as.Date("2023-10-01"),
                                            frequency),
                          date_labels = "%b%y") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 90))
  
  return(plot)
  
}

# To get the plot for an indicator that is at organisation level:
get_indicator_organisation_level_plot <-
  function(org_code_of_interest,
           hospitals,
           controls,
           indicator,
           plotting_variable,
           frequency) {
    sbr_date <- hospitals |>
      dplyr::filter(organisation_code == org_code_of_interest) |>
      dplyr::pull(switch_month)
    
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
      sbr_date,
      frequency
    )
    
    return(plot)
    
  }

# To get the plot for an indicator that is at site level:
get_indicator_site_level_plot <- function(org_code_of_interest,
                                          hospitals,
                                          controls,
                                          indicator,
                                          plotting_variable,
                                          frequency) {
  site_code_of_interest <- hospitals |>
    dplyr::filter(organisation_code == org_code_of_interest) |>
    dplyr::pull(site_code)
  
  sbr_date <- hospitals |>
    dplyr::filter(site_code == site_code_of_interest) |>
    dplyr::pull(switch_month)
  
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
                         sbr_date,
                         frequency)
  
  return(plot)
  
}

# Plot of availability of indicators over time

indicator_availability_over_time <-
  function(friends_and_family_cia_format,
           staff_turnover_cia_format,
           staff_sickness_cia_format,
           staff_survey_cia_format,
           hcai_cia_format,
           falls_and_fractures_cia_format,
           sus_deaths_cia_format,
           rtt_waiting_time_cia_format,
           bed_occupancy_cia_format,
           length_of_stay_cia_format,
           emergency_readmissions_cia_format,
           cleaning_costs_cia_format) {
    measure <-
      c(
        "Patient experience-\nfriends and family test",
        "Staff turnover",
        "Staff sickness",
        "Staff survey",
        "Healthcare acquired infections",
        "Falls and fractures",
        "Hospital deaths",
        "RTT waiting times",
        "Bed occupancy",
        "Length of stay",
        "Emergency readmissions",
        "Cleaning costs"
      )
    level <-
      c("site",
        "trust",
        "trust",
        "trust",
        "trust",
        "site",
        "site",
        "trust",
        "trust",
        "site",
        "site",
        "site")
    group <-
      c(
        "Patient & Staff Experience",
        "Patient & Staff Experience",
        "Patient & Staff Experience",
        "Patient & Staff Experience",
        "Health & Safety",
        "Health & Safety",
        "Health & Safety",
        "Productivity & Efficiency",
        "Productivity & Efficiency",
        "Productivity & Efficiency",
        "Productivity & Efficiency",
        "Productivity & Efficiency"
      )
    start_date <-
      c(
        min(friends_and_family_cia_format$month),
        min(staff_turnover_cia_format$month),
        min(staff_sickness_cia_format$month),
        min(staff_survey_cia_format$month),
        min(hcai_cia_format$month),
        min(falls_and_fractures_cia_format$month),
        min(sus_deaths_cia_format$month),
        min(rtt_waiting_time_cia_format$month),
        min(bed_occupancy_cia_format$month),
        min(length_of_stay_cia_format$month),
        min(emergency_readmissions_cia_format$month),
        min(cleaning_costs_cia_format$month)
      )
    end_date <-
      c(
        max(friends_and_family_cia_format$month),
        max(staff_turnover_cia_format$month),
        max(staff_sickness_cia_format$month),
        max(staff_survey_cia_format$month),
        max(hcai_cia_format$month),
        max(falls_and_fractures_cia_format$month),
        max(sus_deaths_cia_format$month),
        max(rtt_waiting_time_cia_format$month),
        max(bed_occupancy_cia_format$month),
        max(length_of_stay_cia_format$month),
        max(emergency_readmissions_cia_format$month),
        max(cleaning_costs_cia_format$month)
      )
    
    
    indicators <-
      data.frame(group, measure, level, start_date, end_date) |>
      gather(key = type, value = range, -group, -measure, -level) |>
      mutate(range = as.Date(range)) |>
      mutate(measure = factor(
        measure,
        levels = c(
          "Staff turnover",
          "Staff sickness",
          "Patient experience-\nfriends and family test",
          "Staff survey",
          "Healthcare acquired infections",
          "Falls and fractures",
          "Hospital deaths",
          "Cleaning costs",
          "Emergency readmissions",
          "Bed occupancy",
          "RTT waiting times",
          "Length of stay"
        )
      ))
    
    name <-
      c(
        "Royal Liverpool",
        "Clatterbridge",
        "Papworth",
        "Peterborough",
        "Chase Farm",
        "Southmead",
        "Tunbridge Wells"
      )
    date <-
      c(
        '2022-10-01',
        '2020-06-01',
        '2019-05-01',
        '2010-11-01',
        '2018-09-01',
        '2014-05-01',
        '2011-01-01'
      )
    
    sites <- data.frame(name, date) |>
      mutate(date = as.Date(date))
    
    
    a <-
      ggplot(data = (indicators |> 
                       filter(group == "Productivity & Efficiency")), 
             aes(y = measure, x = range)) +
      geom_path(lineend = "round",
                color = "#f9bf07",
                linewidth = 5) +
      labs(title = "Productivity & Efficiency", y = NULL, x = NULL) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 12.5),
        plot.margin = unit(c(4.5, 1, 0, 1), "lines"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust = -0.45, colour = "#5881c1"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      geom_vline(
        data = sites,
        aes(xintercept = date),
        linetype = "dashed",
        color = "#2c2825",
        linewidth = 0.8
      ) +
      geom_text(
        data = sites |> filter(name != "Peterborough" &
                                 name != "Tunbridge Wells"),
        aes(x = date, y = Inf, label = name),
        colour = "#2c2825",
        angle = 40,
        hjust = 0,
        vjust = -0.2,
        text = element_text(size = 11.5)
      ) +
      geom_text(
        data = sites |> filter(name == "Peterborough"),
        aes(x = date, y = Inf, label = name),
        colour = "#2c2825",
        angle = 40,
        hjust = 0.1,
        vjust = -0.4,
        text = element_text(size = 11)
      ) +
      geom_text(
        data = sites |> filter(name == "Tunbridge Wells"),
        aes(x = date, y = Inf, label = name),
        colour = "#2c2825",
        angle = 40,
        hjust = 0,
        vjust = 0.6,
        text = element_text(size = 11)
      ) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      coord_cartesian(clip = 'off')
    
    b <-
      ggplot(data = (indicators |> 
                       filter(group == "Health & Safety")), 
             aes(y = measure, x = range)) +
      geom_path(lineend = "round",
                color = "#f9bf07",
                linewidth = 5) +
      labs(title = "Health & Safety", y = NULL, x = NULL) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 12.5),
        plot.margin = unit(c(0, 1, 0, 1), "lines"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust = -0.4, colour = "#5881c1"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      geom_vline(
        data = sites,
        aes(xintercept = date),
        linetype = "dashed",
        color = "#2c2825",
        linewidth = 0.8
      ) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      coord_cartesian(clip = 'off')
    
    c <-
      ggplot(data = (indicators |> 
                       filter(group == "Patient & Staff Experience")), 
             aes(y = measure, x = range)) +
      geom_path(lineend = "round",
                color = "#f9bf07",
                linewidth = 5) +
      labs(title = "Patient & Staff Experience", y = NULL, x = NULL) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 12.5),
        axis.text.x = element_text(size = 11),
        plot.margin = unit(c(0, 1, 1, 1), "lines"),
        plot.title = element_text(hjust = -0.45, colour = "#5881c1"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      geom_vline(
        data = sites,
        aes(xintercept = date),
        linetype = "dashed",
        color = "#2c2825",
        linewidth = 0.8
      ) +
      scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y",
        limits = as.Date(c('2008-04-01', '2023-12-31'))
      ) +
      coord_cartesian(clip = 'off')
    
    figure <- ggarrange(a, b, c, ncol = 1)
    
  }

#### Availability of indicators by site table format


indicator_availability_by_site <-
  function(friends_and_family_cia_format,
           staff_turnover_cia_format,
           staff_sickness_cia_format,
           staff_survey_cia_format,
           hcai_cia_format,
           falls_and_fractures_cia_format,
           sus_deaths_cia_format,
           rtt_waiting_time_cia_format,
           bed_occupancy_cia_format,
           length_of_stay_cia_format,
           emergency_readmissions_cia_format,
           cleaning_costs_cia_format) {
    measure <-
      c(
        "Patient experience-\nfriends and family test",
        "Staff turnover",
        "Staff sickness",
        "Staff survey",
        "Healthcare acquired infections",
        "Falls and fractures",
        "Hospital deaths",
        "RTT waiting times",
        "Bed occupancy",
        "Length of stay",
        "Emergency readmissions",
        "Cleaning costs"
      )
    level <-
      c("site",
        "trust",
        "trust",
        "trust",
        "trust",
        "site",
        "site",
        "trust",
        "trust",
        "site",
        "site",
        "site")
    group <-
      c(
        "Patient & Staff Experience",
        "Patient & Staff Experience",
        "Patient & Staff Experience",
        "Patient & Staff Experience",
        "Health & Safety",
        "Health & Safety",
        "Health & Safety",
        "Productivity & Efficiency",
        "Productivity & Efficiency",
        "Productivity & Efficiency",
        "Productivity & Efficiency",
        "Productivity & Efficiency"
      )
    start_date <-
      c(
        min(friends_and_family_cia_format$month),
        min(staff_turnover_cia_format$month),
        min(staff_sickness_cia_format$month),
        min(staff_survey_cia_format$month),
        min(hcai_cia_format$month),
        min(falls_and_fractures_cia_format$month),
        min(sus_deaths_cia_format$month),
        min(rtt_waiting_time_cia_format$month),
        min(bed_occupancy_cia_format$month),
        min(length_of_stay_cia_format$month),
        min(emergency_readmissions_cia_format$month),
        min(cleaning_costs_cia_format$month)
      )
    end_date <-
      c(
        max(friends_and_family_cia_format$month),
        max(staff_turnover_cia_format$month),
        max(staff_sickness_cia_format$month),
        max(staff_survey_cia_format$month),
        max(hcai_cia_format$month),
        max(falls_and_fractures_cia_format$month),
        max(sus_deaths_cia_format$month),
        max(rtt_waiting_time_cia_format$month),
        max(bed_occupancy_cia_format$month),
        max(length_of_stay_cia_format$month),
        max(emergency_readmissions_cia_format$month),
        max(cleaning_costs_cia_format$month)
      )
    
    
    indicators <-
      data.frame(group, measure, level, start_date, end_date) |>
      gather(key = type, value = range, -group, -measure, -level) |>
      mutate(range = as.Date(range)) |>
      mutate(measure = factor(
        measure,
        levels = c(
          "Staff turnover",
          "Staff sickness",
          "Patient experience-\nfriends and family test",
          "Staff survey",
          "Healthcare acquired infections",
          "Falls and fractures",
          "Hospital deaths",
          "Emergency readmissions",
          "Bed occupancy",
          "RTT waiting times",
          "Length of stay",
          "Cleaning costs"
        )
      ))
    
    name <-
      c(
        "Royal Liverpool",
        "Clatterbridge",
        "Papworth",
        "Peterborough",
        "Chase Farm",
        "Southmead",
        "Tunbridge Wells"
      )
    date <-
      c(
        '2022-10-01',
        '2020-06-01',
        '2019-05-01',
        '2010-11-01',
        '2018-09-01',
        '2014-05-01',
        '2011-01-01'
      )
    
    sites <- data.frame(name, date) |>
      mutate(date = as.Date(date))
    
    
    indicator_availability_by_site <- indicators |>
      cross_join(sites) |>
      filter(type == "start_date") |>
      mutate(difference = as.numeric(date - range)) |> #difference in days
      mutate(sufficient_baseline = ifelse(difference >= 730|measure=="Staff survey", "yes", "no")) |>
      select(-difference, -date, -range, -type) |>
      pivot_wider(names_from = name, values_from = sufficient_baseline) |>
      mutate(measure = factor(
        measure,
        levels = c(
          "Patient experience-\nfriends and family test",
          "Staff sickness",
          "Staff turnover",
          "Staff survey",
          "Hospital deaths",
          "Falls and fractures",
          "Healthcare acquired infections",
          "Length of stay" ,
          "RTT waiting times",
          "Bed occupancy",
          "Emergency readmissions",
          "Cleaning costs"
        )
      )) |>
      arrange(measure) |>
      mutate(group = factor(
        group,
        levels = c(
          "Productivity & Efficiency",
          "Health & Safety",
          "Patient & Staff Experience"
        )
      )) |>
      arrange(group)
    
    colormatrix1 <-
      ifelse(
        indicator_availability_by_site == "no",
        "#FBE0DC",
        ifelse(
          indicator_availability_by_site == "yes",
          "#d5eed1",
          "#FFFFFF"
        )
      ) |>
      as.data.frame() |>
      slice(1:5) |>
      select(-group) |>
      as.matrix()
    
    colormatrix2 <-
      ifelse(
        indicator_availability_by_site == "no",
        "#FBE0DC",
        ifelse(
          indicator_availability_by_site == "yes",
          "#d5eed1",
          "#FFFFFF"
        )
      ) |>
      as.data.frame() |>
      slice(6:8) |>
      select(-group) |>
      as.matrix()
    
    
    
    colormatrix3 <-
      ifelse(
        indicator_availability_by_site == "no",
        "#FBE0DC",
        ifelse(
          indicator_availability_by_site == "yes",
          "#d5eed1",
          "#FFFFFF"
        )
      ) |>
      as.data.frame() |>
      slice(9:12) |>
      select(-group) |>
      as.matrix()
    
    
    as_grouped_data(indicator_availability_by_site, groups = "group") |>
      as_flextable(hide_grouplabel = TRUE) |>
      set_header_labels(measure = "Variable",
                        level = "Level") |>
      align(part = "header", align = "center") |>
      align(part = "body", align = "center") |>
      align(j = 1:2,  align = "left") |>
      bg(bg = "#f9bf07", part = "header") |>
      bg(i = 2:6, part = "body", bg = colormatrix1) |>
      bg(i = 8:10, bg = colormatrix2) |>
      bg(i = 12:15, bg = colormatrix3) |>
      align(
        j = 1,
        i = ~ !is.na(group),
        part = "body",
        align = "left"
      ) |>
      bold(j = 1,
           i = ~ !is.na(group),
           part = "body") |>
      bold(bold = TRUE, part = "header") |>
      fontsize(size = 12, part = "all") |>
      padding(padding = 2,
              part = "all",
              padding.top = NULL) |>
      autofit() |>
      htmltools_value(ft.align = "left")
    
  }
