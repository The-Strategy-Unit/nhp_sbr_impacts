# Function to find the best matches FOR ORGANISATIONS

select_matches<-function(organisation, data, switch_month, variable){
  
  matches<-tar_read(single_bedroom_matches)|>
    filter(organisation_code==organisation)
  
  dataset<-data|>
    filter(organisation_code==organisation | organisation_code %in% matches$matching_organisation_code)|>
    left_join(matches[,c("matching_organisation_code", "trust_name")],    by=c("organisation_code"="matching_organisation_code"))|> # Add name of matched sites
    distinct()
  # filter(month!=switch_month |organisation_code!=organisation) #remove month of switch
  
  # Find best matches
  mm <- MarketMatching::best_matches(data=dataset,
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
  
  return(mm)
  
}



# Function to run the CIA FOR ORGANISATIONS  
cia_analysis<-function(organisation, data, switch_month, variable, prior_sd){
  
  mm<-select_matches(organisation, data, switch_month, variable)
  
  #View the best matches
  value<-mm$BestMatches|>
    filter(organisation_code==organisation)
  
  
  # Run the causal impact analysis
  results <- MarketMatching::inference(matched_markets = mm,
                                       analyze_betas=TRUE,
                                       test_market = organisation,
                                       end_post_period =(as.Date(switch_month) %m+% months(24)),
                                       alpha=0.05, 
                                       prior_level_sd= prior_sd)
  
  # See which hospital receives the highest weight in determining the outcome
  coeff <- results$Coefficients
  
  #Store predicted values in dataframe
  pred <- results$Predictions
  
  return(results)
  
}


evaluating_model<-function(organisation, data, switch_month, variable, prior_sd, model){
  
  mm<-  select_matches(organisation, data, switch_month, variable)
  
  #Prospective Pseudo Power Curves -will help you evaluate if your choice of test and control markets creates a sufficient model to measure a realistic lift from a future intervention. 
  power <- MarketMatching::test_fake_lift(matched_markets = mm, 
                                          test_market = organisation, 
                                          end_fake_post_period = (as.Date(switch_month) %m+% months(24)), 
                                          prior_level_sd = prior_sd, 
                                          steps=10,
                                          max_fake_lift=0.1)
  
  #Plot the actuals 
  a<-model$PlotActuals+
    su_theme()
  
  # Check out the DW and MAPE of the model
  b<-model$PlotPriorLevelSdAnalysis+
    su_theme()
  
  #And plot the graph- Ideally, a curve that starts at high probability on the left side, reaches its minimum at zero lift, and then rises again symmetrically. If the curve does not reach its minimum at zero there may be systemic model bias in the post period. 
  c<-power$ResultsGraph+
    su_theme()
  
  figure<-ggarrange(a,b,c,
                    ncol=1)
  
}

# Function to find the best matches FOR SITES
select_matches_sites<-function(organisation, site, data, switch_month, variable){
  
  matches<-tar_read(single_bedroom_matches)|>
    filter(organisation_code==organisation)
  
  dataset<-data|>
    filter(site_code==site | site_code %in% matches$site_code)|>
    left_join(matches[,c("site_code", "trust_name")], by=c("site_code"))|> # Add name of matched sites
    distinct()
  #  filter(month!=switch_month |site_code!=site) #remove month of switch
  
  # Find best matches
  mm <- MarketMatching::best_matches(data=dataset,
                                     id_variable="site_code",
                                     date_variable="month",
                                     matching_variable={{variable}},
                                     parallel=FALSE,
                                     warping_limit=1, 
                                     dtw_emphasis=1, 
                                     matches=5, 
                                     start_match_period=(as.Date(switch_month) %m-% months(120)),
                                     end_match_period=(as.Date(switch_month)))
  
  # start_match_period=(as.Date(min(data$month))),
  
  return(mm)
  
}



# Function to run the CIA FOR SITES   
cia_analysis_sites<-function(organisation, site, data, switch_month, variable, prior_sd){
  
  mm<-select_matches_sites(organisation, site, data, switch_month, variable)
  
  #View the best matches
  value<-mm$BestMatches|>
    filter(site_code==site)
  
  # Run the causal impact analysis
  results <- MarketMatching::inference(matched_markets = mm,
                                       analyze_betas=TRUE,
                                       test_market = site,
                                       end_post_period =(as.Date(switch_month) %m+% months(24)),
                                       alpha=0.05, 
                                       prior_level_sd= prior_sd)
  
  # See which hospital receives the highest weight in determining the outcome
  coeff <- results$Coefficients
  
  #Store predicted values in dataframe
  pred <- results$Predictions
  
  return(results)
  
}

# Evaluating the model FOR SITES
evaluating_model_sites<-function(organisation, site, data, switch_month, variable, prior_sd, model){
  
  mm<-  select_matches_sites(organisation, site, data, switch_month, variable)
  
  #Prospective Pseudo Power Curves -will help you evaluate if your choice of test and control markets creates a sufficient model to measure a realistic lift from a future intervention. 
  power <- MarketMatching::test_fake_lift(matched_markets = mm, 
                                          test_market = site, 
                                          end_fake_post_period = (as.Date(switch_month) %m+% months(24)), 
                                          prior_level_sd = prior_sd, 
                                          steps=10,
                                          max_fake_lift=0.1)
  
  #Plot the actuals 
  a<-model$PlotActuals+
    su_theme()
  
  # Check out the DW and MAPE of the model
  b<-model$PlotPriorLevelSdAnalysis+
    su_theme()
  
  #And plot the graph- Ideally, a curve that starts at high probability on the left side, reaches its minimum at zero lift, and then rises again symmetrically. If the curve does not reach its minimum at zero there may be systemic model bias in the post period. 
  c<-power$ResultsGraph+
    su_theme()
  
  figure<-ggarrange(a,b,c,
                    ncol=1)
  
}

# Function to plot out the CIA results

cia_summary_plots<-function(results, title, ylab, switch_date){
  
  # Plot out actual vs expected
  a<-results$PlotActualVersusExpected+
    su_theme()+
    labs(title=title, y=ylab)+
    theme(axis.text = element_text(size=10), 
          axis.title = element_text(size=11), 
          legend.title =element_blank(),
          legend.text=element_text(size=10))+
    annotate(geom="text", x=as.Date(switch_date),  y=-Inf, vjust=-0.3, hjust=1.04, label="SBR switch", color="#ec6555")+
    geom_vline(aes(xintercept =as.Date(switch_date)),  linetype="solid", linewidth=0.6, color="#ec6555")+
    scale_color_manual(values=c("#f9bf07","#2c2825" ))+
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
    guides(colour = guide_legend(reverse=T))
  
  # Plot pointwise effect
  b<-results$PlotPointEffect+
    su_theme()+
    theme(axis.text = element_text(size=10), axis.title = element_text(size=11))+
    geom_hline(aes(yintercept =0), linetype="dotted", color="#686f73", linewidth=0.4)+
    geom_vline(aes(xintercept =as.Date(switch_date)), linetype="solid", linewidth=0.6, color="#ec6555")+
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  
  # Plot out cumulative effect
  c<-results$PlotCumulativeEffect+
    su_theme()+
    theme(axis.text = element_text(size=10), axis.title = element_text(size=11))+
    geom_hline(aes(yintercept =0), linetype="dotted", color="#686f73", linewidth=0.4)+
    geom_vline(aes(xintercept =as.Date(switch_date)), linetype="solid", linewidth=0.6, color="#ec6555")+
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  
  figure<-ggarrange(a,b,c, ncol=1)
}

