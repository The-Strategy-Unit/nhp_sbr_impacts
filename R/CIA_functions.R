# Function to find the best matches FOR ORGANISATIONS

select_matches<-function(organisation,site ,data, switch_month, variable,single_bedroom_matches ){
  
  matches<-(single_bedroom_matches)|>
    filter(organisation_code==organisation)
  
  if (is.na(site)) {
    dataset<-data|>
      filter(organisation_code==organisation | organisation_code %in% matches$matching_organisation_code)|>
      left_join(matches[,c("matching_organisation_code", "trust_name")],    by=c("organisation_code"="matching_organisation_code"))|> # Add name of matched sites
      distinct()
    # filter(month!=switch_month |organisation_code!=organisation) #remove month of switch
  }
  
  if (!is.na(site)) {
    dataset<-data|>
      filter(site_code==site | site_code %in% matches$site_code)|>
      left_join(matches[,c("site_code", "trust_name")], by=c("site_code"))|> # Add name of matched sites
      distinct()|>
      rename(organisation_code=site_code)
    # filter(month!=switch_month |organisation_code!=organisation) #remove month of switch
  }
  
  # Find best matches
  cia_matches <- MarketMatching::best_matches(data=dataset,
                                     id_variable="organisation_code",
                                     date_variable="month",
                                     matching_variable={{variable}},
                                     parallel=FALSE,
                                     warping_limit=1, 
                                     dtw_emphasis=1, 
                                     matches=5, 
                                     start_match_period=(as.Date(switch_month) %m-% months(120)),
                                     end_match_period=(as.Date(switch_month)))
  
  # start_match_period=(as.Date(min(data$month))),
  
  return(cia_matches)
  
}



# Function to run the CIA model
cia_analysis<-function(organisation, site, data, switch_month, variable, prior_sd, single_bedroom_matches  ){
  
  cia_matches<-select_matches(organisation, site, data, switch_month, variable,single_bedroom_matches  )
  
  #View the best matches
  if (is.na(site)) {
    value<-cia_matches$BestMatches|>
      filter(organisation_code==organisation)
    
    test_site<-organisation
  }
  
  if (!is.na(site)) {
      value<-cia_matches$BestMatches|>
      filter(organisation_code==site)
    # filter(month!=switch_month |organisation_code!=organisation) #remove month of switch
      
      test_site<-site
  }

  
  # Run the causal impact analysis
  model_results <- MarketMatching::inference(matched_markets = cia_matches,
                                       analyze_betas=TRUE,
                                       test_market = test_site,
                                       end_post_period =(as.Date(switch_month) %m+% months(24)),
                                       alpha=0.05, 
                                       prior_level_sd= prior_sd)
  
  # See which hospital receives the highest weight in determining the outcome
  coeff <- model_results$Coefficients
  
  #Store predicted values in dataframe
  pred <- model_results$Predictions
  
  return(model_results)
  
}


#Evaluating the model
evaluating_model<-function(organisation, site, data, switch_month, variable, prior_sd, model_results,single_bedroom_matches ){
  
  cia_matches<-  select_matches(organisation, site, data, switch_month, variable,single_bedroom_matches )
  
  if (is.na(site)) {
    test_site<-organisation
  }
  
  if (!is.na(site)) {
    test_site<-site
  }
  
  #Prospective Pseudo Power Curves -will help you evaluate if your choice of test and control markets creates a sufficient model to measure a realistic lift from a future intervention. 
  power <- MarketMatching::test_fake_lift(matched_markets = cia_matches, 
                                          test_market = test_site, 
                                          end_fake_post_period = (as.Date(switch_month) %m+% months(24)), 
                                          prior_level_sd = prior_sd, 
                                          steps=10,
                                          max_fake_lift=0.1)
  
  #Plot the actuals 
  a<-model_results$PlotActuals+
    su_theme()
  
  # Check out the DW and MAPE of the model
  b<-model_results$PlotPriorLevelSdAnalysis+
    su_theme()
  
  #And plot the graph- Ideally, a curve that starts at high probability on the left side, reaches its minimum at zero lift, and then rises again symmetrically. If the curve does not reach its minimum at zero there may be systemic model bias in the post period. 
  c<-power$ResultsGraph+
    su_theme()
  
  figure<-ggarrange(a,b,c,
                    ncol=1)
  
}



# Function to plot out the CIA results

cia_summary_plots<-function(model_results, ylab, switch_date){
  
  # Plot out actual vs expected
  
  max<-c(model_results$PlotActualVersusExpected$data$upper_bound, model_results$PlotActualVersusExpected$data$Response)
  
  a<-model_results$PlotActualVersusExpected+
    su_theme()+
    labs(title=NULL, y=ylab)+
    theme(axis.text = element_text(size=9.5), 
          axis.title = element_text(size=11), 
          legend.title =element_blank(),
          legend.text=element_text(size=10),
          legend.position = "top")+
    annotate(geom="text", x=as.Date(switch_date),  y=-Inf, vjust=-0.3, hjust=1.04, label="SBR switch", color="#ec6555")+
    geom_vline(aes(xintercept =as.Date(switch_date)),  linetype="solid", linewidth=0.6, color="#ec6555")+
    scale_color_manual(values=c("#f9bf07","#2c2825" ))+
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
    guides(colour = guide_legend(reverse=T))+
    scale_y_continuous(limits=c(0,max(max)*1.1))
  
  # Plot pointwise effect
  b<-model_results$PlotPointEffect+
    su_theme()+
    theme(axis.text = element_text(size=9.5), axis.title = element_text(size=11))+
    geom_hline(aes(yintercept =0), linetype="dotted", color="#686f73", linewidth=0.4)+
    geom_vline(aes(xintercept =as.Date(switch_date)), linetype="solid", linewidth=0.6, color="#ec6555")+
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  
  # Plot out cumulative effect
  c<-model_results$PlotCumulativeEffect+
    su_theme()+
    theme(axis.text = element_text(size=9.5), axis.title = element_text(size=11))+
    geom_hline(aes(yintercept =0), linetype="dotted", color="#686f73", linewidth=0.4)+
    geom_vline(aes(xintercept =as.Date(switch_date)), linetype="solid", linewidth=0.6, color="#ec6555")+
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  
  figure<-ggarrange(a,b,c, ncol=1)
}

# Function to extract model details

extract_model_details<-function(model) {

  name<-deparse(substitute(model))
  
  df<-(model$CausalImpactObject$summary)|>
    mutate(site=name)|>
    mutate(sig=ifelse(p<0.05, "Sig", "Non-sig"))|>
    mutate(p=round(p,3))|>
    mutate(Actual=round(Actual,2))|>
    mutate(Pred=round(Pred,2))|>
    mutate(Predicted=paste0(round(Pred,2), " (",round(Pred.lower,2)," - ",round(Pred.upper,2), ")" ))|>
    mutate(Effect=paste0(round(RelEffect,2), " (",round(RelEffect.lower,2)," - ",round(RelEffect.upper,2), ")" ))|>
    mutate(Absolute=paste0(round(AbsEffect,2), " (",round(AbsEffect.lower,2)," - ",round(AbsEffect.upper,2), ")" ))|>
    mutate(site=case_when(site=="rem" ~ "Royal Liverpool",
                          site=="ren" ~ "Clatterbridge",
                          site=="rgm" ~ "Papworth",
                          site=="rgn" ~ "Peterborough",
                          site=="ral" ~ "Chase Farm",
                          site=="rvj" ~ "Southmead",
                          site=="rwf" ~ "Tunbridge Wells"))
  
  df<-df |>
    filter(row.names(df) %in% c('Average'))

  return(df)
}



#Summary table of model parameters

model_output<-function(rem, ren, rgm,rgn,ral,rvj, rwf){
  
  ifelse((!is.na(rem)), r_rem<-extract_model_details(rem), r_rem<-NA)
  ifelse((!is.na(ren)), r_ren<-extract_model_details(ren), r_ren<-NA)
  ifelse((!is.na(rgm)), r_rgm<-extract_model_details(rgm), r_rgm<-NA)
  ifelse((!is.na(rgn)), r_rgn<-extract_model_details(rgn), r_rgn<-NA)
  ifelse((!is.na(ral)), r_ral<-extract_model_details(ral), r_ral<-NA)
  ifelse((!is.na(rvj)), r_rvj<-extract_model_details(rvj), r_rvj<-NA)
  ifelse((!is.na(rwf)), r_rwf<-extract_model_details(rwf), r_rwf<-NA)
  
  df<-rbind(r_rem,r_ren,r_rgm,r_rgn,r_ral,r_rvj, r_rwf)
  return(df)
  
}






# Function to generate forest plot

forest_plot<-function(data){
  
  results_data<-data|>
    filter(!is.na(site))
    
  if(count(results_data)>1){
  output<-bayesmeta(y = results_data[,"RelEffect"],
                    sigma = results_data[,"RelEffect.sd"],
                    label = results_data[,"site"])
  
  df<-as.data.frame(output$summary)
  
  df$value <- row.names(df)
  
  df<-df|>
    select(mu, value)|>
    pivot_wider(names_from = value, values_from=mu)
  }
  
  if(count(results_data)<=1){

    mean<-c(data$RelEffect)
    `95% upper`<-c(data$RelEffect.upper)
    `95% lower`<-c(data$RelEffect.lower) 
    
    df<-cbind(mean,`95% upper`, `95% lower`)|>
      as.data.frame()|>
      filter(!is.na(mean))

  }
  
  
  mean<-mean(data$RelEffect, na.rm=TRUE)
  
  results_data<-results_data|>
    mutate(site = fct_reorder(site, RelEffect))|>
    mutate(site=fct_rev(site))|>
    mutate(p=as.character(p))|>
    mutate(id = RelEffect)|>
    bind_rows(data.frame(
      site="MEAN EFFECT",
      RelEffect = df$mean,
      RelEffect.upper=df$`95% upper`,
      RelEffect.lower=df$`95% lower`,
      Effect= paste0(round(df$mean,2), " (",round(df$`95% lower`,2), " - ", round(df$`95% upper`,2), ")" ),
      id=-100,
      sig="NA"))|>
    bind_rows(
      data.frame(
        Effect = "Relative Effect (95% CI)",
        p = "p-value",
        id=100) )
  
  a<-min(results_data$RelEffect.lower, na.rm=TRUE)
  b<-max(results_data$RelEffect.upper, na.rm=TRUE)
  
  if (((b-0)/-(a-0))>2){
    a<-a*(1.7)
  }
  
  
  p_mid<-results_data |>
    ggplot(aes(x = RelEffect, y = (fct_reorder(site,id) ))) +
    theme_classic()+
    geom_point(aes(x=RelEffect, colour=sig), shape=15, size=3) +
    geom_linerange(aes(xmin=RelEffect.lower, xmax=RelEffect.upper, colour=sig)) +
    geom_vline(xintercept = 0, linetype="dashed") +
    scale_color_manual(values=c("black","#686f73","#ec6555" ))+
    labs(x="Relative Effect Size", y="")+
    coord_cartesian(ylim=c(1,nrow(results_data)), 
                    xlim=c(min(a)-0.02,
                           max(b)+0.02))+
    annotate("text", x = min(a)/1.5, y = nrow(results_data), size=3, label = "Decrease with SBR", colour="#686f73") +
    annotate("text", x = max(b)/2, y = nrow(results_data), size=3,  label = "Increase with SBR", colour="#686f73")+ 
    theme(legend.position="none",
          axis.line.y = element_blank(),
          axis.ticks.y= element_blank(),
          axis.text.y= element_blank(),
          axis.title.y= element_blank())
  
  
  
  p_left <-
    results_data |>
    ggplot(aes(y = fct_reorder(site,id))) +
    geom_text(aes(x = 0, label = site), hjust = 0, size=3, fontface = "bold")+
    geom_text(
      aes(x = 2, label = Effect),
      hjust = 0,
      size=3,
      fontface = ifelse(results_data$Effect =="Relative Effect (95% CI)", "bold", "plain"))+
    theme_void() +
    coord_cartesian(xlim = c(0, 4))
  
  
  p_right <-
    results_data |>
    ggplot() +
    geom_text(
      aes(x = 0, y = fct_reorder(site,id), label = p),
      hjust = 0,
      size=3,
      fontface = ifelse(results_data$p == "p-value", "bold", "plain")
    ) +
    theme_void() 
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 6), 
    area(t =1, l = 7, b = 30, r = 13), 
    area(t = 0, l = 13, b = 30, r = 15) 
  )
  # final plot arrangement
  p_left + p_mid + p_right + plot_layout(design = layout)
  
}

# Function for model output table
model_effects_table<-function(data){

data|>
  filter(!is.na(site))|>
  arrange(desc(RelEffect))|>
  mutate(sig=case_when(RelEffect>0 & p<0.05 ~ "Sig Increase", 
                       RelEffect<0 & p<0.05 ~ "Sig Decrease",
                       p>=0.05 ~ "Non-Sig"))|>
  select(site, Actual, Predicted, Absolute, Effect, p, sig)|>
  flextable()|>
  set_header_labels(site="Site",
                    Actual="Actual \nEffect",
                    Predicted="Predicted Effect \n(95% CI)",
                    Absolute="Absolute Effect \n(95% CI)",
                    Effect="Relative Effect \n(95% CI)",
                    p="p-value",
                    sig="Significance")|>
  align(part = "header", align = "center")|>
  align(part = "body", align = "center")|>
  align(j=2:7,align = "right")|>
  align(j=2:7,align = "right", part="header")|>
  align(j=1,  align = "left")|>
  bg(bg = "#f9bf07", part = "header") |>
  bold(bold = TRUE, part="header")|>
  fontsize(size = 10.5, part = "all")|>
  padding(padding = 2, part = "all", padding.top=NULL) |>
  autofit()|>
  htmltools_value(ft.align = "left") 

}

# Function for summary table of all sites and indicators

summary_table_indicators_and_sites<-function(waiting_time_median_output,
                                             waiting_time_number_output,
                                             LoS_output,
                                             emergency_readmissions_output,
                                             bed_occupancy_output,
                                             hcai_output,
                                             falls_and_fractures_output,
                                             sus_deaths_output,
                                             friends_and_family_output,
                                             staff_sickness_output)   {
  
  waiting_time_median_output<-waiting_time_median_output|>
    mutate(measure="Waiting time (median)")
  waiting_time_number_output<-waiting_time_number_output|>
    mutate(measure="Waiting time (number)")
  LoS_output  <-LoS_output|>
    mutate(measure="Length of stay")
  emergency_readmissions_output  <-emergency_readmissions_output|>
    mutate(measure="Emergency readmissions")
  bed_occupancy_output  <-bed_occupancy_output|>
    mutate(measure="Bed occupancy")
  hcai_output  <-hcai_output|>
    mutate(measure="Healthcare acquired infections")
  falls_and_fractures_output  <-falls_and_fractures_output|>
    mutate(measure="Falls and fractures")
  sus_deaths_output  <-sus_deaths_output|>
    mutate(measure="Hospital deaths")
  friends_and_family_output  <-friends_and_family_output|>
    mutate(measure="Patient experience- friends and family test")
  staff_sickness_output  <-staff_sickness_output|>
    mutate(measure="Staff sickness")
  
  
  combined_outputs<-rbind(waiting_time_median_output,waiting_time_number_output,LoS_output, emergency_readmissions_output,
                          bed_occupancy_output, hcai_output, falls_and_fractures_output, sus_deaths_output,
                          friends_and_family_output, staff_sickness_output )|>
    mutate(sig=case_when(RelEffect>0 & p<0.05 ~ "Increase", 
                         RelEffect<0 & p<0.05 ~ "Decrease",
                         p>=0.05 ~ "NS"))|>
    select(site, measure, sig)|>
    filter(!is.na(site))|>
    pivot_wider(names_from = site, values_from=sig )
  
  colormatrix<-ifelse(is.na(combined_outputs), "#FFFFFF" , 
                      ifelse(combined_outputs=="NS","#FFFFD9", 
                             ifelse((combined_outputs=="Increase" & 
                                       (combined_outputs$measure!="Patient experience- friends and family test" &
                                          combined_outputs$measure!="Staff sickness" & combined_outputs$measure!="Bed occupancy"))|((combined_outputs$measure=="Patient experience- friends and family test" | combined_outputs$measure=="Staff sickness"| combined_outputs$measure=="Bed occupancy")& combined_outputs=="Decrease"), "#FBE0DC", "#d5eed1")))|>
    as.data.frame()|>
    mutate(measure= "#FFFFFF")|>
    as.matrix()
  
  
  combined_outputs|>
    flextable()|>
    set_header_labels(measure= "")|>
    align(part = "header", align = "center")|>
    align(part = "body", align = "center")|>
    bg(bg = "#f9bf07", part = "header") |>
    bg( part="body", bg=colormatrix)|>
    bold(bold = TRUE, part="header")|>
    fontsize(size = 12, part = "all")|>
    padding(padding = 2, part = "all", padding.top=NULL) |>
    autofit()|>
    htmltools_value(ft.align = "left") 
  
  
  
  
  
  
}
