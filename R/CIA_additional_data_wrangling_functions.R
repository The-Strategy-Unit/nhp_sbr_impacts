# Additional data wrangling functions for Causal Impact Analysis

# Merge together site information where site codes have changed over time and mergers occurred
merge_sites<-function(data){
  data|>
    mutate(site_code=ifelse((site_code=="REMRQ"|site_code=="RQ617"), "REMRQ", site_code))|> #merge historical Royal Liverpool codes
    mutate(site_code=ifelse((site_code=="RGM22"|site_code=="RGM21"), "RGM22", site_code))|> #merge Papworth site codes
    mutate(site_code=ifelse((site_code=="RVJ01"|site_code=="RVJ20"), "RVJ01", site_code))|> #merge Southmead site codes
    mutate(site_code=ifelse((site_code=="RGN80"|site_code=="RGN42"), "RGN80", site_code))|> #merge Peterborough site codes
    mutate(site_code=ifelse((site_code=="RWFTW"|site_code=="RWF01"|site_code=="RWF02"), "RWFTW", site_code)) #merge Tunbridge site codes
}


# Friends and Family test additional formatting
friends_and_family_cia_formatting<-function(formatted_data){
  
  formatted_data|>
  mutate(month=as.Date(month))|>
  merge_sites()|>
  group_by(month, site_code)|>
  summarise(percent=mean(percent, na.rm=TRUE))
}


# Staff turnover - Nurses
staff_turnover_cia_formatting<-function(data){
  
  data|>
filter(staff_group=="nurses_health_visitors")|>
  mutate(month=floor_date(effective_snapshot_date, "month"))|> #Format date to monthly
  mutate(organisation_code=ifelse((organisation_code=="REM"|organisation_code=="RQ6"), "REM", organisation_code))|> #merge historical Royal Liverpool codes
  group_by(month, organisation_code, type)|>
  summarise(turnover_fte=sum(turnover_fte))|>
  pivot_wider(names_from = type, values_from = turnover_fte)|>
  mutate(leaving_rate=(Leavers/Denoms)*100)

}


# Staff sickness additional formatting
staff_sickness_cia_formatting<-function(data){
  
  data|>
  mutate(month=as.Date(month))|>
  mutate(organisation_code=ifelse((organisation_code=="REM"|organisation_code=="RQ6"), "REM", organisation_code))|> #merge historical Royal Liverpool codes
  group_by(month, organisation_code)|>
  summarise(fte_days_sick=sum(fte_days_sick), fte_days_available=sum(fte_days_available) )|> #regroup following merging the organisation codes
  mutate(percent=(fte_days_sick/fte_days_available)*100)
}

# Healthcare acquired infections additional formatting
hcai_cia_formatting<-function(data){

  data|>
  left_join(beddays_by_prov[,c("organisation_code", "yearmon", "beddays")], by=c("organisation_code", "month"="yearmon"))|>
    mutate(organisation_code=ifelse((organisation_code=="REM"|organisation_code=="RQ6"), "REM", organisation_code))|> #merge historical Royal Liverpool codes 
  mutate(rate=(count_of_cases/beddays)*10000)|> #cases per 10,000 beddays
  group_by(month, organisation_code)|>
  summarise(combined_rate=sum(rate))|>#calculate combined rate for all infections
  filter(month>="2018-03-01") #Combined measure can't be calculated before this date as lack of data
}

#Falls and fractures additional formatting
falls_and_fractures_cia_formatting<-function(data){
  
  data|>
rename(site_code=der_provider_site_code)|>
  rename(month=yearmon)|>
  merge_sites()|>
  group_by(site_code, month)|>
  summarise(fall_fracs=sum(fall_fracs), beddays=sum(beddays))|> #recalculate rate following merging sites
  mutate(ff_rate=(fall_fracs/beddays)*1000) #rate per 1000 beddays
}

# Hospital death rate additional formatting
sus_deaths_cia_formatting<-function(data){
  
  data|>
rename(site_code=der_provider_site_code)|>
  rename(month=yearmon)|>
  merge_sites()|>
  group_by(site_code, month)|>
  summarise(deaths=sum(deaths), spells=sum(spells))|> #recalculate rate following merging sites
  mutate(death_rate=(deaths/spells)*1000) #deaths/1000 spells
}

#rtt waiting time additional formatting 

rtt_waiting_time_cia_formatting<-function(formatted_data){
  
formatted_data|>
mutate(month=as.Date(month))|>
  mutate(organisation_code=ifelse((organisation_code=="REM"|organisation_code=="RQ6"), "REM", organisation_code))|> #merge historical Royal Liverpool codes
  group_by(month, organisation_code)|>
  summarise(median_by_prov = median(rep(weeks,number_of_incomplete_pathways)), number_incomplete=sum(number_of_incomplete_pathways))|> #Median by month and provider 
  mutate(median_by_prov=ifelse(is.na(median_by_prov),0,median_by_prov))

}

# Bed occupancy additional formatting
bed_occupancy_cia_formatting<-function(data){

data|>
mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%Y-%m-%d"))|> #Format date 
  mutate(month=floor_date(effective_snapshot_date, "month"))|> #Format date to monthly
  filter(report_period_length=="Quarterly")|>
  mutate(organisation_code=ifelse((organisation_code=="REM"|organisation_code=="RQ6"), "REM", organisation_code))|> #merge historical Royal Liverpool codes
  group_by(month, organisation_code)|>
  summarise(available=sum(available), occupied=sum(occupied))|>
  mutate(bed_occupancy=occupied/available)
}
  
  # Length of Stay additional formatting
  length_of_stay_cia_formatting<-function(data){
  
    data|>
  rename(site_code=der_provider_site_code)|>
    rename(month=yearmon)|>
    merge_sites()|>
    group_by(site_code, month)|>
    summarise(los=sum(los), spells=sum(spells))|> #recalculate following merge of sites
    mutate(avg_los=los/spells) 
  
}
