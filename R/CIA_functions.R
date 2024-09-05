# Function to find the best matches FOR ORGANISATIONS

select_matches <- function(organisation,
                           site,
                           data,
                           switch_month,
                           variable,
                           control_pool) {
  matches <- control_pool |> 
    filter(organisation_code == organisation)
  
  if (is.na(site)) {
    dataset <- data |>
      filter(
        organisation_code == organisation |
          organisation_code %in% matches$matching_organisation_code
      ) |>
      left_join(matches[, c("matching_organisation_code", "trust_name")],
                by = c("organisation_code" = "matching_organisation_code")) |> # Add name of matched sites
      distinct()
    # filter(month!=switch_month |organisation_code!=organisation) #remove month of switch
  }
  
  if (!is.na(site)) {
    dataset <- data |>
      filter(site_code == site |
               site_code %in% matches$site_code) |>
      left_join(matches[, c("site_code", "trust_name")], by = c("site_code")) |> # Add name of matched sites
      distinct() |>
      rename(organisation_code = site_code)
    # filter(month!=switch_month |organisation_code!=organisation) #remove month of switch
  }
  
  if ((organisation == "RWF" & variable == "positive_responses")|
      (organisation=="RAL" & variable == "cleaning_service_cost")){
    number <- 0
  }
  else{
    number = 1
  }
  
  # Find best matches
  cia_matches <- MarketMatching::best_matches(
    data = dataset,
    id_variable = "organisation_code",
    date_variable = "month",
    matching_variable = {
      {
        variable
      }
    },
    parallel = FALSE,
    warping_limit = 1,
    dtw_emphasis = number,
    matches = 5,
    start_match_period = (as.Date(switch_month) %m-% months(120)),
    end_match_period = (as.Date(switch_month))
  )
  
  # start_match_period=(as.Date(min(data$month))),
  
  return(cia_matches)
  
}

# Function to run the CIA model
cia_analysis <- function(organisation,
                         data,
                         variable,
                         prior_sd,
                         control_pool,
                         hospitals) {
  if ("site_code" %in% colnames(data)) {
    site <- hospitals |>
      dplyr::filter(organisation_code == organisation) |>
      dplyr::pull(site_code)
  } else {
    site <- NA
  }
  
  switch_month <- hospitals |>
    dplyr::filter(organisation_code == organisation) |>
    dplyr::pull(switch_month)
  
  cia_matches <-
    select_matches(organisation,
                   site,
                   data,
                   switch_month,
                   variable,
                   control_pool)
  
  #View the best matches
  if (is.na(site)) {
    value <- cia_matches$BestMatches |>
      filter(organisation_code == organisation)
    
    test_site <- organisation
  }
  
  if (!is.na(site)) {
    value <- cia_matches$BestMatches |>
      filter(organisation_code == site)
    # filter(month!=switch_month |organisation_code!=organisation) #remove month of switch
    
    test_site <- site
  }
  
  # Run the causal impact analysis
  model_results <-
    MarketMatching::inference(
      matched_markets = cia_matches,
      analyze_betas = TRUE,
      test_market = test_site,
      end_post_period = (as.Date(switch_month) %m+% months(24)),
      alpha = 0.05,
      prior_level_sd = prior_sd
    )
  
  # See which hospital receives the highest weight in determining the outcome
  coeff <- model_results$Coefficients
  
  #Store predicted values in dataframe
  pred <- model_results$Predictions
  
  return(model_results)
  
}


#Evaluating the model
evaluating_model <- function(organisation,
                             site,
                             data,
                             switch_month,
                             variable,
                             prior_sd,
                             model_results,
                             control_pool) {
  cia_matches <- select_matches(organisation,
                                site,
                                data,
                                switch_month,
                                variable,
                                control_pool)
  
  if (is.na(site)) {
    test_site <- organisation
  }
  
  if (!is.na(site)) {
    test_site <- site
  }
  
  #Prospective Pseudo Power Curves -will help you evaluate if your choice of test and control markets creates a sufficient model to measure a realistic lift from a future intervention.
  power <-
    MarketMatching::test_fake_lift(
      matched_markets = cia_matches,
      test_market = test_site,
      end_fake_post_period = (as.Date(switch_month) %m+% months(24)),
      prior_level_sd = prior_sd,
      steps = 10,
      max_fake_lift = 0.1
    )
  
  #Plot the actuals
  a <- model_results$PlotActuals +
    su_theme()
  
  # Check out the DW and MAPE of the model
  b <- model_results$PlotPriorLevelSdAnalysis +
    su_theme()
  
  #And plot the graph- Ideally, a curve that starts at high probability on the left side, reaches its minimum at zero lift, and then rises again symmetrically. If the curve does not reach its minimum at zero there may be systemic model bias in the post period.
  c <- power$ResultsGraph +
    su_theme()
  
  figure <- ggarrange(a, b, c,
                      ncol = 1)
  
}

# Function to plot out the CIA results
add_vline_to_cia_summary_plot <- function(switch_date){
  geom_vline(
    aes(xintercept = as.Date(switch_date)),
    linetype = "solid",
    linewidth = 0.6,
    color = "#ec6555"
  )
}

add_hline_to_cia_summary_plot <- function(){
  geom_hline(
    aes(yintercept = 0),
    linetype = "dotted",
    color = "#686f73",
    linewidth = 0.4
  )
}

cia_summary_plots <- function(model_results, ylab, switch_date) {
  # Plot out actual vs expected
  
  max <-  c(model_results$PlotActualVersusExpected$data$upper_bound,
            model_results$PlotActualVersusExpected$data$Response
    )
  
  a <- model_results$PlotActualVersusExpected +
    su_theme() +
    labs(title = NULL, y = ylab) +
    theme(
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 11),
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "top",
      plot.margin = margin(10, 10, 15, 10)
    ) +
    annotate(
      geom = "text",
      x = as.Date(switch_date),
      y = -Inf,
      vjust = -0.3,
      hjust = 1.04,
      label = "SBR switch",
      color = "#ec6555"
    ) +
    add_vline_to_cia_summary_plot(switch_date) +
    scale_color_manual(values = c("#f9bf07", "#2c2825")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    guides(colour = guide_legend(reverse = T)) +
    scale_y_continuous(limits = c(0, max(max) * 1.03), labels = scales::comma)
  
  # Plot pointwise effect
  b <- model_results$PlotPointEffect +
    su_theme() +
    theme(axis.text = element_text(size = 9),
          axis.title = element_text(size = 11)) +
    add_hline_to_cia_summary_plot() +
    add_vline_to_cia_summary_plot(switch_date) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  # Plot out cumulative effect
  c <- model_results$PlotCumulativeEffect +
    su_theme() +
    theme(axis.text = element_text(size = 9),
          axis.title = element_text(size = 11)) +
    add_hline_to_cia_summary_plot() +
    add_vline_to_cia_summary_plot(switch_date) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  figure <- ggarrange(a, b, c, ncol = 1)
}

# Function to extract model details

extract_model_details <- function(model) {
  name <- deparse(substitute(model))
  
  df <- (model$CausalImpactObject$summary) |>
    mutate(site = name) |>
    mutate(sig = ifelse(p < 0.05, "Sig", "Non-sig")) |>
    mutate(p = round(p, 3)) |>
    mutate(Actual = ifelse(Actual < 100, round(Actual, 2), round(Actual, 0))) |>
    mutate(Pred = ifelse(Pred < 100, round(Pred, 2), round(Pred, 0))) |>
    mutate(Pred.lower = ifelse(Pred < 100, round(Pred.lower, 2), round(Pred.lower, 0))) |>
    mutate(Pred.upper = ifelse(Pred < 100, round(Pred.upper, 2), round(Pred.upper, 0))) |>
    mutate(Pred.lower = ifelse(Pred < 100, round(Pred.lower, 2), round(Pred.lower, 0))) |>
    mutate(AbsEffect = ifelse(
      Pred < 100,
      round(AbsEffect, 2),
      round(AbsEffect, 0)
    )) |>
    mutate(AbsEffect.lower = ifelse(
      Pred < 100,
      round(AbsEffect.lower, 2),
      round(AbsEffect.lower, 0)
    )) |>
    mutate(AbsEffect.upper = ifelse(
      Pred < 100,
      round(AbsEffect.upper, 2),
      round(AbsEffect.upper, 0)
    )) |>
    mutate(Predicted = paste0(
      format(Pred, big.mark = ",", scientific = FALSE),
      " (",
      Pred.lower,
      " to ",
      Pred.upper,
      ")"
    )) |>
    mutate(Effect = paste0(
      round(RelEffect, 2),
      " (",
      round(RelEffect.lower, 2),
      " to ",
      round(RelEffect.upper, 2),
      ")"
    )) |>
    mutate(Absolute = paste0(
      format(AbsEffect, big.mark = ",", scientific = FALSE),
      " (",
      AbsEffect.lower,
      " to ",
      AbsEffect.upper,
      ")"
    )) |>
    mutate(
      site = case_when(
        site == "rem" ~ "Royal Liverpool",
        site == "ren" ~ "Clatterbridge",
        site == "rgm" ~ "Papworth",
        site == "rgn" ~ "Peterborough",
        site == "ral" ~ "Chase Farm",
        site == "rvj" ~ "Southmead",
        site == "rwf" ~ "Tunbridge Wells"
      )
    )
  
  df <- df |>
    filter(row.names(df) %in% c('Average'))
  
  return(df)
}

#Summary table of model parameters

model_output <- function(rem, ren, rgm, rgn, ral, rvj, rwf) {
  ifelse((!is.na(rem)), r_rem <- extract_model_details(rem), r_rem <-
           NA)
  ifelse((!is.na(ren)), r_ren <-
           extract_model_details(ren), r_ren <- NA)
  ifelse((!is.na(rgm)), r_rgm <-
           extract_model_details(rgm), r_rgm <- NA)
  ifelse((!is.na(rgn)), r_rgn <-
           extract_model_details(rgn), r_rgn <- NA)
  ifelse((!is.na(ral)), r_ral <-
           extract_model_details(ral), r_ral <- NA)
  ifelse((!is.na(rvj)), r_rvj <-
           extract_model_details(rvj), r_rvj <- NA)
  ifelse((!is.na(rwf)), r_rwf <-
           extract_model_details(rwf), r_rwf <- NA)
  
  df <- rbind(r_rem, r_ren, r_rgm, r_rgn, r_ral, r_rvj, r_rwf)
  return(df)
  
}

# Function to generate forest plot

forest_plot <- function(data, caption, subtitle) {
  results_data <- data |>
    filter(!is.na(site)) |>
    mutate(sig = case_when(
      (RelEffect.upper > 0 & RelEffect < 0) |
        (RelEffect.lower < 0 & RelEffect > 0)
      ~ "Non-sig",
      (RelEffect.upper < 0 &
         RelEffect < 0) ~ "Positive Effect",
      (RelEffect.lower > 0 &
         RelEffect > 0) ~ "Negative Effect"
    ))
  
  dataset <- deparse(substitute(data))
  
  
  if (dataset %in% c("bed_occupancy_output",
                     "friends_and_family_output",
                     "staff_survey_output")) {
    results_data <- results_data |>
      mutate(sig = case_when(
        (RelEffect.upper > 0 & RelEffect < 0) |
          (RelEffect.lower < 0 & RelEffect > 0)
        ~ "Non-sig",
        (RelEffect.upper < 0 &
           RelEffect < 0) ~ "Negative Effect",
        (RelEffect.lower > 0 &
           RelEffect > 0) ~ "Positive Effect"
      ))
  }
  
  
  results_data <- results_data |>
    mutate(site = fct_reorder(site, RelEffect)) |>
    mutate(site = fct_rev(site)) |>
    mutate(p = p * 2) |> # convert from 1 to 2-tailed p value
    mutate(p = as.character(p)) |>
    mutate(id = RelEffect) |>
    bind_rows(data.frame(Effect = "Relative Effect (95% CI)",
                         p = "p-value",
                         id = 100))
  
  
  max1 <- max(results_data$RelEffect.upper,
              -(results_data$RelEffect.lower),
              na.rm = TRUE)
  
  low <- min((results_data$RelEffect.lower),
             -(max1 / 2), na.rm = TRUE)
  
  up <- max(results_data$RelEffect.upper,
            (max1 / 2), na.rm = TRUE)
  
  p_mid <- results_data |>
    ggplot(aes(x = RelEffect, y = (fct_reorder(site, id)))) +
    theme_classic() +
    geom_point(aes(x = RelEffect, colour = sig),
               shape = 15,
               size = 3) +
    geom_linerange(aes(
      xmin = RelEffect.lower,
      xmax = RelEffect.upper,
      colour = sig
    )) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_manual(
      values = c(
        "Non-Sig" = "#686f73",
        "Positive Effect" = "#129957" ,
        "Negative Effect" = "#ec6555"
      )
    ) +
    labs(
      x = "Relative Effect Size",
      y = "",
      subtitle = NULL,
      title = NULL
    ) +
    coord_cartesian(ylim = c(1, nrow(results_data)),
                    xlim = c(low * 1.03,
                             up * 1.03)) +
    annotate(
      "text",
      x = low / 1.5,
      y = nrow(results_data),
      size = 3,
      label = "Decrease with SBR",
      colour = "#686f73"
    ) +
    annotate(
      "text",
      x = up / 2,
      y = nrow(results_data),
      size = 3,
      label = "Increase with SBR",
      colour = "#686f73"
    ) +
    theme(
      legend.position = "none",
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
    )
  
  p_left <-  results_data |>
    ggplot(aes(y = fct_reorder(site, id))) +
    geom_text(
      aes(x = 0, label = site),
      hjust = 0,
      size = 3,
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 2, label = Effect),
      hjust = 0,
      size = 3,
      fontface = ifelse(
        results_data$Effect == "Relative Effect (95% CI)",
        "bold",
        "plain"
      )
    ) +
    theme_void() +
    coord_cartesian(xlim = c(0, 4))
  
  p_right <- results_data |>
    ggplot() +
    geom_text(
      aes(
        x = 0,
        y = fct_reorder(site, id),
        label = p
      ),
      hjust = 0,
      size = 3,
      fontface = ifelse(results_data$p == "p-value", "bold", "plain")
    ) +
    theme_void()
  
  layout <- c(area(
    t = 0,
    l = 0,
    b = 30,
    r = 6
  ),
  area(
    t = 1,
    l = 7,
    b = 30,
    r = 13
  ),
  area(
    t = 0,
    l = 13,
    b = 30,
    r = 15
  ))
  # final plot arrangement
  (p_left + p_mid + p_right + plot_layout(design = layout)) +
    plot_annotation(
      caption = str_wrap(caption, 135),
      subtitle = str_wrap(subtitle, 90),
      theme = theme(
        plot.caption = element_text(hjust = 0, size = 9),
        plot.subtitle = element_text(
          hjust = 0,
          size = 13,
          face = "bold",
          colour = "#686f73"
        )
      )
    )
  
}


# Function to calculate mean effect for DGHs

meta_analysis<-function(data){
  
  results_data <- data |>
    filter(!is.na(site)) |>
    filter(site != "Papworth" &
             site != "Clatterbridge" &
             site != "Chase Farm")
  
  count<-print(nrow(results_data))
  
    output <-
      rma.uni(RelEffect,
              (RelEffect.sd) ^ 2,
              method = "DL",
              data = results_data)
    
    df <- as.data.frame(output$b) |>
      rename(mean = V1) |>
      cbind(`95% lower` = output$ci.lb) |>
      cbind(`95% upper` = output$ci.ub) |>
      cbind(n=count)
    
  
  return(df)
  
}

# Function to generate mean forest plot for DGHs

mean_forest_plot_DGH_Acute <- function(data, subtitle) {
 
  results_data <- data |>
    filter(!is.na(site)) |>
    filter(site != "Papworth" &
             site != "Clatterbridge" &
             site != "Chase Farm") |>
    mutate(sig = case_when(
      (RelEffect.upper > 0 & RelEffect < 0) |
        (RelEffect.lower < 0 & RelEffect > 0)
      ~ "Non-sig",
      (RelEffect.upper < 0 &
         RelEffect < 0) ~ "Positive Effect",
      (RelEffect.lower > 0 &
         RelEffect > 0) ~ "Negative Effect"
    )) |>
    mutate(group = "individual")
  
  dataset <- deparse(substitute(data))
  
  if (dataset %in% c("bed_occupancy_output",
                     "friends_and_family_output",
                     "staff_survey_output")) {
    results_data <- results_data |>
      mutate(sig = case_when(
        (RelEffect.upper > 0 & RelEffect < 0) |
          (RelEffect.lower < 0 & RelEffect > 0)
        ~ "Non-sig",
        (RelEffect.upper < 0 &
           RelEffect < 0) ~ "Negative Effect",
        (RelEffect.lower > 0 &
           RelEffect > 0) ~ "Positive Effect"
      ))
  }
  
  if (count(results_data) > 1) {
    output <-
      rma.uni(RelEffect,
              (RelEffect.sd) ^ 2,
              method = "DL",
              data = results_data)
    
    df <- as.data.frame(output$b) |>
      rename(mean = V1) |>
      cbind(`95% lower` = output$ci.lb) |>
      cbind(`95% upper` = output$ci.ub)
    
    weights <- as.data.frame(weights(output))
    
    weights <- weights |>
      mutate(`weights(output)` = (`weights(output)` / sum(`weights(output)`)) *
               10)
    
    mean_weight <- (mean(weights$`weights(output)`))
    
  }
  
  if (count(results_data) <= 1) {
    mean <- c(data$RelEffect)
    `95% upper` <- c(data$RelEffect.upper)
    `95% lower` <- c(data$RelEffect.lower)
    
    df <- cbind(mean, `95% upper`, `95% lower`) |>
      as.data.frame() |>
      filter(!is.na(mean))
    
  }
  
  mean <- mean(data$RelEffect, na.rm = TRUE)
  
  
  results_data <- results_data |>
    mutate(site = fct_reorder(site, RelEffect)) |>
    mutate(site = fct_rev(site)) |>
    mutate(p = p * 2) |> # convert from 1 to 2-tailed p value
    mutate(p = as.character(p)) |>
    mutate(id = RelEffect) |>
    cbind(weights) |>
    rename(weight = `weights(output)`) |>
    bind_rows(
      data.frame(
        site = "MEAN EFFECT",
        RelEffect = df$mean,
        RelEffect.upper = df$`95% upper`,
        RelEffect.lower = df$`95% lower`,
        Effect = paste0(
          round(df$mean, 2),
          " (",
          round(df$`95% lower`, 2),
          " to ",
          round(df$`95% upper`, 2),
          ")"
        ),
        id = -100,
        sig = "mean",
        weight = mean_weight,
        group = "mean"
      )
    ) |>
    bind_rows(data.frame(Effect = "Relative Effect (95% CI)",
                         p = "p-value",
                         id = 100))
  
  max1 <- max(results_data$RelEffect.upper,
              -(results_data$RelEffect.lower),
              na.rm = TRUE)
  
  low <- min((results_data$RelEffect.lower),
             -(max1 / 2), na.rm = TRUE)
  
  up <- max(results_data$RelEffect.upper,
            (max1 / 2), na.rm = TRUE)
  
  p_mid <- results_data |>
    ggplot(aes(x = RelEffect, y = (fct_reorder(site, id)))) +
    theme_classic() +
    geom_point(aes(
      x = RelEffect,
      colour = sig,
      size = weight,
      shape = group
    )) +
    geom_linerange(aes(
      xmin = RelEffect.lower,
      xmax = RelEffect.upper,
      colour = sig
    ),
    size = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_shape_manual(values = c("individual" = 15, "mean" = 16)) +
    scale_color_manual(
      values = c(
        "mean" = "black",
        "Non-Sig" = "#686f73",
        "Positive Effect" = "#129957" ,
        "Negative Effect" = "#ec6555"
      )
    ) +
    labs(x = "Relative Effect Size", y = "",
         caption = "Square size indicates the weight of each point in calculating the mean (i.e. larger square indicates a greater contribution to the mean)") +
    coord_cartesian(ylim = c(1, nrow(results_data)),
                    xlim = c(low, up)) +
    annotate(
      "text",
      x = low / 1.5,
      y = nrow(results_data),
      size = 3,
      label = "Decrease with SBR",
      colour = "#686f73"
    ) +
    annotate(
      "text",
      x = up / 2,
      y = nrow(results_data),
      size = 3,
      label = "Increase with SBR",
      colour = "#686f73"
    ) +
    theme(
      legend.position = "none",
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      plot.caption.position = "panel",
      plot.caption = element_text(hjust = .85)
    )
  
  p_left <- results_data |>
    ggplot(aes(y = fct_reorder(site, id))) +
    geom_text(
      aes(x = 0, label = site),
      hjust = 0,
      size = 3,
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 2, label = Effect),
      hjust = 0,
      size = 3,
      fontface = ifelse(
        results_data$Effect == "Relative Effect (95% CI)",
        "bold",
        "plain"
      )
    ) +
    theme_void() +
    coord_cartesian(xlim = c(0, 4))
  
  p_right <- results_data |>
    ggplot() +
    geom_text(
      aes(
        x = 0,
        y = fct_reorder(site, id),
        label = p
      ),
      hjust = 0,
      size = 3,
      fontface = ifelse(results_data$p == "p-value", "bold", "plain")
    ) +
    theme_void()
  
  layout <- c(area(
    t = 0,
    l = 0,
    b = 17,
    r = 6
  ),
  area(
    t = 1,
    l = 7,
    b = 17,
    r = 13
  ),
  area(
    t = 0,
    l = 13,
    b = 17,
    r = 15
  ))
  # final plot arrangement
  p_left + p_mid + p_right + plot_layout(design = layout) +
    plot_annotation(subtitle = subtitle,
                    theme = theme(
                      plot.subtitle = element_text(
                        hjust = 0,
                        size = 13,
                        face = "bold",
                        colour = "#686f73"
                      )
                    ))
}

# Function for model output table
model_effects_table <- function(data, title) {
  name <- deparse(substitute(data))
  
  if (name == "cleaning_costs_output") {
    note <- "Actual, Predicted and Absolute difference values expressed as £ per sqm"
  }
  else{
    note <- NA
  }
  
  data |>
    filter(!is.na(site)) |>
    arrange(desc(RelEffect)) |>
    mutate(
      sig = case_when(
        RelEffect > 0 & RelEffect.lower > 0 ~ "Sig Increase",
        RelEffect < 0 & RelEffect.upper < 0 ~ "Sig Decrease",
        (RelEffect.upper > 0 & RelEffect < 0) |
          (RelEffect.lower < 0 & RelEffect > 0)
        ~ "Non-sig",
      )
    ) |>
    mutate(p = p * 2) |> # convert from 1 to 2-tailed p value
    mutate(p = as.character(p)) |>
    select(site, Actual, Predicted, Absolute, Effect, p, sig) |>
    flextable() |>
    set_header_labels(
      site = "Site",
      Actual = "Actual \nEffect",
      Predicted = "Predicted Effect \n(95% CI)",
      Absolute = "Absolute Effect \n(95% CI)",
      Effect = "Relative Effect \n(95% CI)",
      p = "p-value",
      sig = "Significance"
    ) |>
    add_header_lines(values = title) |>
    align(i = 2, part = "header", align = "center") |>
    align(i = 1, part = "header", align = "left") |>
    align(part = "body", align = "center") |>
    align(j = 2:7, align = "right") |>
    align(j = 2:7,
          align = "right",
          part = "header") |>
    align(j = 1,  align = "left") |>
    bg(i = 2,
       bg = "#f9bf07",
       part = "header") |>
    bold(bold = TRUE, part = "header") |>
    fontsize(size = 10.5, part = "all") |>
    fontsize(i = 1, part = "header", size = 13) |>
    padding(padding = 2,
            part = "all",
            padding.top = NULL) |>
    padding(i = 1,
            padding = 6,
            part = "header") |>
    hline_top(border = fp_border_default(width = 0), part = "header") |>
    color(i = 1,
          color = "#686f73",
          part = "header") |>
    add_footer_lines(value = note) |>
    autofit() |>
    htmltools_value(ft.align = "left")
  
}

# Function for summary table of all sites and indicators

summary_table_indicators_and_sites <-
  function(LoS_output,
           waiting_time_median_output,
           bed_occupancy_output,
           emergency_readmissions_output,
           cleaning_costs_output,
           sus_deaths_output,
           falls_and_fractures_output,
           cdiff_output,
           friends_and_family_output,
           staff_sickness_output,
           staff_turnover_output,
           staff_survey_output)   {
    waiting_time_median_output2 <- waiting_time_median_output |>
      mutate(measure = "Waiting time",
             group="Productivity & Efficiency")
    LoS_output2  <- LoS_output |>
      mutate(measure = "Length of stay",
             group="Productivity & Efficiency")
    emergency_readmissions_output2  <-
      emergency_readmissions_output |>
      mutate(measure = "Emergency readmissions",
             group="Productivity & Efficiency")
    cleaning_costs_output2  <-
      cleaning_costs_output |>
      mutate(measure = "Cleaning costs",
             group="Productivity & Efficiency")
    bed_occupancy_output2  <- bed_occupancy_output |>
      mutate(measure = "Bed occupancy",
             group="Productivity & Efficiency")
    cdiff_output2  <- cdiff_output |>
      mutate(measure = "Healthcare acquired C.diff",
             group="Health & Safety")
    falls_and_fractures_output2  <- falls_and_fractures_output |>
      mutate(measure = "Falls and injuries",
             group="Health & Safety")
    sus_deaths_output2  <- sus_deaths_output |>
      mutate(measure = "Hospital deaths",
             group="Health & Safety")
    friends_and_family_output2  <- friends_and_family_output |>
      mutate(measure = "Patient experience",
             group="Patient & Staff experience")
    staff_sickness_output2  <- staff_sickness_output |>
      mutate(measure = "Staff sickness",
             group="Patient & Staff experience")
    staff_survey_output2  <- staff_survey_output |>
      mutate(measure = "Staff survey",
             group="Patient & Staff experience")
    staff_turnover_output2  <- staff_turnover_output |>
      mutate(measure = "Staff turnover",
             group="Patient & Staff experience")
    
    combined_outputs <- rbind(
        LoS_output2,
        waiting_time_median_output2,
        bed_occupancy_output2,
        emergency_readmissions_output2,
        cleaning_costs_output2,
        sus_deaths_output2,
        falls_and_fractures_output2,
        cdiff_output2,
        friends_and_family_output2,
        staff_sickness_output2,
        staff_turnover_output2,
        staff_survey_output2
      ) |>
      filter(!is.na(site)) |>
      mutate(p = p * 2) |> # convert from 1 to 2-tailed p value
      mutate(sig = case_when(
        RelEffect > 0 & p < 0.05 ~ "\U2191",
        RelEffect < 0 & p < 0.05 ~ "\U2193",
        p >= 0.05 ~ "-"
      )) |>
      select(site, measure, group, sig) |>
      pivot_wider(names_from = site, values_from = sig) |>
      select(
        measure,
        group,
        `Royal Liverpool`,
        Clatterbridge,
        Papworth,
        `Chase Farm`,
        Southmead,
        `Tunbridge Wells`,
        Peterborough
      )|>
      as.data.frame()
    
  
    
    colormatrix <- ifelse(
      is.na(combined_outputs),
      "grey80" ,
      ifelse(
        combined_outputs == "-",
        "#FFFFD9",
        ifelse((
          combined_outputs == "\U2191" &
            (
              combined_outputs$measure != "Patient experience" &
                combined_outputs$measure != "Bed occupancy" &
                combined_outputs$measure != "Staff survey"
            )
        ) |
          ((
            combined_outputs$measure %in% c(
              "Patient experience" ,
              "Bed occupancy",
              "Staff survey"
            )
          ) &
            combined_outputs == "\U2193"
          ),
        "#FBE0DC",
        "#d5eed1"
        )
      )
    ) |>
      as.data.frame() |>
      mutate(measure = "#FFFFFF") |>
      select(-group)|>
      add_row(measure="#FFFFFF", .before=1)|>
      add_row(measure="#FFFFFF", .before=7)|>
      add_row(measure="#FFFFFF", .before=11)|>
      as.matrix()
    
    colormatrix2 <- ifelse(
      is.na(combined_outputs),
      "grey80" ,
      ifelse(
        combined_outputs == "-",
        "#d1a005",
        ifelse((
          combined_outputs == "\U2191" &
            (
              combined_outputs$measure != "Patient experience" &
                combined_outputs$measure != "Bed occupancy" &
                combined_outputs$measure != "Staff survey"
            )
        ) |
          ((
            combined_outputs$measure %in% c(
              "Patient experience" ,
              "Bed occupancy",
              "Staff survey"
            )
          )
          &
            combined_outputs == "\U2193"
          ),
        "#ec6555",
        "#6C9380"
        )
      )
    ) |>
      as.data.frame() |>
      mutate(measure = "black") |>
      select(-group)|>
      add_row(measure="black",  .before=1)|>
      add_row(measure="black", .before=7)|>
      add_row(measure="black", .before=11)|>
      as.matrix()
    
  
    
  as_grouped_data(combined_outputs, groups = "group") |>
      as_flextable(hide_grouplabel = TRUE) |>
      set_header_labels(measure = "") |>
      align(part = "header", align = "center") |>
      align(part = "body", align = "center") |>
      bg(bg = "#f9bf07", part = "header") |>
      color(color = colormatrix2, part = "body") |>
      bg(part = "body", bg = colormatrix) |>
      bold(bold = TRUE, part = "header") |>
      fontsize(size = 11, part = "all") |>
      fontsize(j = 2:8, size = 14, part = "body") |>
      padding(padding = 2,
              part = "all",
              padding.top = NULL) |>
    align(
      j = 1,
      i = ~ !is.na(group),
      part = "body",
      align = "left"
    ) |>
    bold(j = 1,
         i = ~ !is.na(group),
         part = "body") |>
      autofit() |>
      add_footer_lines(value = c(
        paste0(
          "\U2191",
          " indicates a significant increase post-switch to single rooms, while",
          " \U2193",
          " indicates a significant decrease. Where a result is considered positive, e.g. decreased length of stay or increased patient experience score, it is coloured green, while a negative result is shown in red.
               '-' indicates a non-significant change, which is shown in yellow. Grey shading indicates where there was insufficient data to analyse."
        )
      ))
    
  }

# summary forest plot of main effects- TO BE FINISHED- NOT WORKING YET

summary_forest_plot <- function(LoS_output,
                                waiting_time_median_output,
                                bed_occupancy_output,
                                emergency_readmissions_output,
                                cleaning_costs_output,
                                sus_deaths_output,
                                falls_and_fractures_output,
                                cdiff_output,
                                friends_and_family_output,
                                staff_sickness_output,
                                staff_survey_output) {
  
  
  waiting_time_median_output2 <- meta_analysis(waiting_time_median_output) |>
    mutate(measure = "Waiting time")
  
  LoS_output2  <- meta_analysis(LoS_output) |>
    mutate(measure = "Length of stay")
  
  emergency_readmissions_output2  <- meta_analysis(emergency_readmissions_output) |>
    mutate(measure = "Emergency readmissions")
  
  cleaning_costs_output2  <- meta_analysis(cleaning_costs_output) |>
    mutate(measure = "Cleaning costs")
  
  bed_occupancy_output2  <- meta_analysis(bed_occupancy_output) |>
    mutate(measure = "Bed occupancy")
  
  cdiff_output2  <- meta_analysis(cdiff_output) |>
    mutate(measure = "Healthcare acquired c.diff")
  
  falls_and_fractures_output2  <- meta_analysis(falls_and_fractures_output) |>
    mutate(measure = "Falls and injuries")
  
  sus_deaths_output2  <- meta_analysis(sus_deaths_output) |>
    mutate(measure = "Hospital deaths")
  
  friends_and_family_output2  <- meta_analysis(friends_and_family_output) |>
    mutate(measure = "Patient experience")
  
  staff_sickness_output2  <- meta_analysis(staff_sickness_output) |>
    mutate(measure = "Staff sickness")
  
  staff_survey_output2  <- meta_analysis(staff_survey_output) |>
    mutate(measure = "Staff survey")
  
  dataset <- deparse(substitute(data))

  results_data <- rbind(
      LoS_output2,
      waiting_time_median_output2,
      bed_occupancy_output2,
      emergency_readmissions_output2,
      cleaning_costs_output2,
      sus_deaths_output2,
      falls_and_fractures_output2,
      cdiff_output2,
      friends_and_family_output2,
      staff_sickness_output2,
      staff_survey_output2
    )
  
  results_data<-results_data|>
    filter(n>1)|>
    mutate(mean=mean*100,
           `95% lower`=`95% lower`*100,
           `95% upper`=`95% upper`*100 )|>
    mutate(value=paste0(round(mean,1), " (", round(`95% lower`,1), " to ",round(`95% upper`,1), ")"))|>
    mutate(n=paste0("n=", n))|>
    mutate(sig = case_when(
      (`95% upper` > 0 & mean < 0) |
        (`95% lower`< 0 & mean > 0)
      ~ "Non-sig",
      (`95% upper` < 0 &
         mean < 0) ~ "Positive Effect",
      (`95% lower` > 0 &
         mean > 0) ~ "Negative Effect"
    ))|>
    mutate(n=as.character(n))|>
    mutate(sig=ifelse((measure %in% c("Patient experience", "Bed occupancy", "Staff survey")) & 
                   sig=="Positive Effect", "Negative Effect", sig ))|>
    bind_rows(data.frame(value = "Mean % change (95% CI)",
                         measure=""))|>
    mutate(measure = factor(
      measure,
      levels = c(
        "Staff sickness",
        "Staff survey",
        "Patient experience",
        "Healthcare acquired c.diff",
        "Falls and injuries",
        "Hospital deaths",
        "Cleaning costs",
        "Emergency readmissions",
        "Bed occupancy",
        "Waiting time",
        "Length of stay",
        ""
      )
    ))
  
  max1 <- max(results_data$`95% upper`,
              -(results_data$`95% lower`),
              na.rm = TRUE)
  
  low <- min((results_data$`95% lower`),
             -(max1 / 2), na.rm = TRUE)
  
  up <- max(results_data$`95% upper`,
            (max1 / 2), na.rm = TRUE)

  
  p_mid <- results_data |>
    ggplot(aes(x = mean, y = measure)) +
    theme_classic() +
    geom_point(aes(
      x = mean,
      colour = sig,
    ), size=2.5) +
    geom_linerange(aes(
      xmin = `95% lower`,
      xmax = `95% upper`,
      colour = sig
    ),
    size = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_shape_manual(values = c("individual" = 15, "mean" = 16)) +
    scale_color_manual(
      values = c(
        "Non-sig" = "#686f73",
        "Positive Effect" = "#129957" ,
        "Negative Effect" = "#ec6555"
      )
    ) +
    labs(x = "Mean % change", y = "")+
    coord_cartesian(ylim = c(1, nrow(results_data))) +
    annotate(
      "text",
      x=low/1.6,
      y = nrow(results_data),
      size = 3,
      label = "Decrease with SBR",
      colour = "#686f73"
    ) +
    annotate(
      "text",
      x=up/1.7,
      y = nrow(results_data),
      size = 3,
      label = "Increase with SBR",
      colour = "#686f73"
    ) +
    theme(
      legend.position = "none",
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      plot.caption.position = "panel",
      plot.caption = element_text(hjust = .85)
    )+
    coord_cartesian(xlim = c(-60, 30))
  
  p_left <- results_data |>
    ggplot(aes(y = measure)) +
    geom_text(
      aes(x = 0, label = measure),
      hjust = 0,
      size =3,
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 2.5, label = value),
      hjust = 0,
      size = 3,
      fontface = ifelse(
        results_data$value == "Mean % change (95% CI)",
        "bold",
        "plain"
     ))+
        geom_text(
          aes(x = 4.2, label = n),
          hjust = 0,
          size = 3
           ) +
    theme_void() +
    coord_cartesian(xlim = c(0, 4.5))
  

  
  layout <- c(area(
    t = 0,
    l = 0,
    b = 17,
    r = 7
  ),
  area(
    t = 1,
    l = 8,
    b = 17,
    r = 15
  ))
  # final plot arrangement
  p_left + p_mid+ plot_layout(design = layout) +
    plot_annotation(subtitle = "Summary of mean % changes for general acute hospitals",
                    caption ="Grey indicates no significant effect, while red indicates a significant negative effect",
                    theme = theme(
                      plot.subtitle = element_text(
                        hjust = 0,
                        size = 13,
                        face = "bold",
                        colour = "#686f73"
                      )
                    )) 

}
  

 

