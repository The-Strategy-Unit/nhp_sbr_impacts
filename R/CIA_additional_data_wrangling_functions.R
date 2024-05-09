# Additional data wrangling functions for Causal Impact Analysis

# Merge together site information where site codes have changed over time and
# mergers occurred
merge_sites <- function(data) {
  data |>
    mutate(site_code = ifelse((site_code == "REMRQ" |
                                 site_code == "RQ617"),
                              "REMRQ",
                              site_code
    )) |> #merge historical Royal Liverpool codes
    mutate(site_code = ifelse((site_code == "RGM22" |
                                 site_code == "RGM21"),
                              "RGM22",
                              site_code
    )) |> #merge Papworth site codes
    mutate(site_code = ifelse((site_code == "RVJ01" |
                                 site_code == "RVJ20"),
                              "RVJ01",
                              site_code
    )) |> #merge Southmead site codes
    mutate(site_code = ifelse((site_code == "RGN80" |
                                 site_code == "RGN42"),
                              "RGN80",
                              site_code
    )) |> #merge Peterborough site codes
    mutate(site_code = ifelse((site_code == "RWFTW" |
                                 site_code == "RWF01" |
                                 site_code == "RWF02"),
                              "RWFTW",
                              site_code
    )) |> #merge Tunbridge site codes
    mutate(site_code = ifelse((site_code == "RALC7" |
                                 site_code == "RVLC7"),
                              "RALC7",
                              site_code
    )) |> #merge Chase Farm site codes
    mutate(site_code = ifelse((site_code == "REN22" |
                                 site_code == "REN20"),
                              "REN22",
                              site_code
    )) #merge Clatterbridge site codes
}

# Beddays by provider
beddays_by_provider_function <- function(data) {
  beddays_by_prov <- data |>
    mutate(organisation_code = substr(der_provider_site_code,
                                      start = 1,
                                      stop = 3)) |>
    #get organisation code
    group_by(yearmon, organisation_code) |>
    summarise(beddays = sum(beddays))
  
  return(beddays_by_prov)
}

# Friends and Family test additional formatting
friends_and_family_cia_formatting <- function(data) {
  friends_and_family_cia_format <- data |>
    mutate(month = as.Date(month)) |>
    merge_sites() |>
    group_by(month, site_code) |>
    summarise(friends_and_family_percent = mean(percent, na.rm = TRUE)) |>
    filter(!is.na(friends_and_family_percent)) |>
    filter(site_code != "N6J7V" &
             site_code != "E0A3H") |># removing control sites with issues
    filter( site_code!="RXL01" &
             site_code!="R0A09")
  
  return(friends_and_family_cia_format)
}


# Staff turnover - Nurses
staff_turnover_cia_formatting <- function(data) {
  staff_turnover_cia_format <-  data |>
    filter(staff_group == "nurses_health_visitors") |>
    mutate(
      turnover_fte = ifelse(
        organisation_code == "RQ6" &
          effective_snapshot_date >= '2019-09-01' &
          effective_snapshot_date < '2020-09-01',
        0,
        turnover_fte
      )
    ) |>
    # REM and RQ6 trusts merged so remove all the RQ6 leavers who transferred to REM
    mutate(
      month = floor_date(effective_snapshot_date, "month"),
      #Format date to monthly
      organisation_code = ifelse((organisation_code == "REM" |
                                    organisation_code == "RQ6"),
                                 "REM",
                                 organisation_code
      ),
      #merge historical Royal Liverpool codes
      organisation_code = ifelse((organisation_code == "RAL" |
                                    organisation_code == "RVL"),
                                 "RAL",
                                 organisation_code
      )
    ) |> #merge historical Chase Farm codes
    summarise(
      turnover_fte = sum(turnover_fte),
      .by = c(month, organisation_code, type)
    ) |>
    pivot_wider(names_from = type, values_from = turnover_fte) |>
    mutate(leaving_rate = (Leavers / Denoms) * 100) |>
    filter(month >= '2018-08-01') |> #no leavers or joiners before this date
    mutate(leaving_rate = ifelse(is.na(leaving_rate), 0, leaving_rate)) |>
    filter(month > "2020-08-01" |
             organisation_code != "REM") |> # Liverpool data issues before this point
    filter(organisation_code != "RVY")
  
  return(staff_turnover_cia_format)
  
}


# Staff sickness additional formatting
staff_sickness_cia_formatting <- function(data) {
  staff_sickness_cia_format <- data |>
    mutate(
      month = as.Date(month),
      organisation_code = ifelse((organisation_code == "REM" |
                                    organisation_code == "RQ6"),
                                 "REM",
                                 organisation_code
      ),
      #merge historical Royal Liverpool codes
      organisation_code = ifelse((organisation_code == "RAL" |
                                    organisation_code == "RVL"),
                                 "RAL",
                                 organisation_code
      )
    ) |> #merge historical Chase Farm codes
    summarise(
      fte_days_sick = sum(fte_days_sick),
      fte_days_available = sum(fte_days_available),
      .by = c(month, organisation_code)
    ) |> #regroup following merging the organisation codes
    mutate(staff_sickness_percent = (fte_days_sick / fte_days_available) * 100)
  
  return(staff_sickness_cia_format)
  
}

# Healthcare acquired infections additional formatting
hcai_cia_formatting <- function(bedday_data, hcai_data) {
  beddays_by_prov <- beddays_by_provider_function(bedday_data)
  
  hcai_cia_format <- hcai_data |>
    left_join(beddays_by_prov[, c("organisation_code", "yearmon", "beddays")],
              by = c("organisation_code", "month" = "yearmon")) |>
    mutate(
      organisation_code = ifelse((organisation_code == "REM" |
                                    organisation_code == "RQ6"),
                                 "REM",
                                 organisation_code
      ),
      #merge historical Royal Liverpool codes
      organisation_code = ifelse((organisation_code == "RAL" |
                                    organisation_code == "RVL"),
                                 "RAL",
                                 organisation_code
      ) ,
      #merge historical Chase Farm codes
      rate = (count_of_cases / beddays) * 10000 #cases per 10,000 beddays
    ) |>
    summarise(combined_rate = sum(rate),
              .by = c(month, organisation_code)) |> #calculate combined rate for all infections
    filter(month >= "2018-03-01") #Combined measure can't be calculated before this date as lack of data
  
  return(hcai_cia_format)
  
}

#C.difficile only additional formatting
cdiff_cia_formatting <- function(bedday_data, hcai_data) {
  beddays_by_prov <- beddays_by_provider_function(bedday_data)

  cdiff_cia_format <- hcai_data|>
  filter(collection=="C.difficile")|>
  left_join(beddays_by_prov[, c("organisation_code", "yearmon", "beddays")],
            by = c("organisation_code", "month" = "yearmon")) |>
  mutate(
    organisation_code = ifelse((organisation_code == "REM" |
                                  organisation_code == "RQ6"),
                               "REM",
                               organisation_code
    ),
    #merge historical Royal Liverpool codes
    organisation_code = ifelse((organisation_code == "RAL" |
                                  organisation_code == "RVL"),
                               "RAL",
                               organisation_code
    ) 
    #merge historical Chase Farm codes
  ) |>
    summarise(count_of_cases = sum(count_of_cases),
              beddays=sum(beddays),
              .by = c(month, organisation_code)) |>
   mutate(rate = (count_of_cases / beddays) * 10000 ) |>#cases per 10,000 beddays
    filter(organisation_code != "RAL" |
             month >= '2016-03-01') |> #remove data pre-merger where there is considerable variability
    filter(organisation_code != "RNL" ) #remove as missing data
  
return(cdiff_cia_format)
    
}   
    

#Falls and fractures additional formatting
falls_and_fractures_cia_formatting <- function(data) {
  falls_and_fractures_cia_format <- data |>
    rename(site_code = der_provider_site_code) |>
    rename(month = yr_mth) |>
    mutate(month = paste0(month, "-01")) |>
    mutate(month = as.Date(month)) |> #Format date
    merge_sites() |>
    mutate(
      fall_spell_los = as.numeric(fall_spell_los),
      all_spell_los = as.numeric(all_spell_los)
    ) |>
    summarise(
      fall_spells = sum(fall_spells),
      all_spells = sum(all_spells),
      all_spell_los = sum(all_spell_los),
      fall_spell_los = sum(fall_spell_los),
      .by = c(site_code, month)
    ) |> #recalculate rate following merging sites
    mutate(ff_rate = (fall_spells / all_spells) * 100) |> # % of spells with a fall
    mutate(ff_los = (fall_spell_los / all_spell_los) * 100)  |> # % of los associated with fall
    mutate(ff_rate = ifelse(is.nan(ff_rate), 0, ff_rate)) |>
    filter(site_code != "RALC7" |
             month > '2014-03-01') |> #remove data pre-merger
    filter(site_code != "RVJ01" |
             (site_code == "RVJ01" &
                month <= '2015-10-01')) |> #Remove part of Southmead where rate jumps
    filter(site_code != "RQWG0" &
             site_code!="RC979") #remove control site with jumps in time series
  
  
  return(falls_and_fractures_cia_format)
  
}

# Hospital death rate additional formatting
sus_deaths_cia_formatting <- function(data) {
  sus_deaths_cia_format <- data |>
    rename(site_code = der_provider_site_code) |>
    rename(month = yr_mth) |>
    mutate(month = paste0(month, "-01")) |>
    mutate(month = as.Date(month)) |> #Format date
    merge_sites() |>
    summarise(
      discharges = sum(discharges),
      death_hosp = sum(death_hosp),
      death_30days = sum(death_30days),
      .by = c(site_code, month)
    ) |> #recalculate rate following merging sites
    mutate(hosp_rate_1000 = (death_hosp / discharges) * 1000) |> #in hospital deaths/1000 discharges
    mutate(all_rate_1000 = ((death_hosp + death_30days) / discharges) * 1000) |> #all deaths/1000 discharges
    filter(site_code!="REN22" | (site_code == "REN22" &
                month > '2012-12-01')) #Remove part before 2013 where Clatterbridge has -ive CIs.

  return(sus_deaths_cia_format)
  
}

#rtt waiting time additional formatting

rtt_waiting_time_cia_formatting <- function(formatted_data) {
  rtt_waiting_time_cia_format <- formatted_data |>
    mutate(
      month = as.Date(month),
      organisation_code = ifelse((organisation_code == "REM" |
                                    organisation_code == "RQ6"),
                                 "REM",
                                 organisation_code
      ),
      #merge historical Royal Liverpool codes
      organisation_code = ifelse((organisation_code == "RAL" |
                                    organisation_code == "RVL"),
                                 "RAL",
                                 organisation_code
      )
    ) |> #merge historical Chase Farm codes
    summarise(
      median_by_prov = median(rep(weeks, number_of_incomplete_pathways)),
      number_incomplete = sum(number_of_incomplete_pathways),
      median_by_prov_dta = median(rep(weeks, number_of_incomplete_pathways_with_dta)),
      number_incomplete_dta = sum(number_of_incomplete_pathways_with_dta),
      .by = c(month, organisation_code)
    ) |> #Median by month and provider
    mutate(median_by_prov = ifelse(is.na(median_by_prov), 0, median_by_prov)) |>
    mutate(median_by_prov_dta = ifelse(is.na(median_by_prov_dta), 0, median_by_prov_dta)) |>
    filter(
      organisation_code != "RF4" &
        organisation_code != "R1H" &
        organisation_code != "RJ2" &
        organisation_code != "RJE" &
        organisation_code != "RQW" &
        organisation_code != "RHW"
    )|> #Removing poss control due to data issues
  filter(organisation_code!="REN"| (organisation_code=="REN" &
           month<'2021-11-01') )
  
  
  return(rtt_waiting_time_cia_format)
  
}

# Bed occupancy additional formatting
bed_occupancy_cia_formatting <- function(data) {
  bed_occupancy_cia_format <- data |>
    mutate(
      month = effective_snapshot_date |>
        as.Date("%Y-%m-%d") |> #Format date to monthly
        floor_date("month"),
      organisation_code = ifelse((organisation_code == "REM" |
                                    organisation_code == "RQ6"),
                                 "REM",
                                 organisation_code
      ),
      #merge historical Royal Liverpool codes
      organisation_code = ifelse((organisation_code == "RAL" |
                                    organisation_code == "RVL"),
                                 "RAL",
                                 organisation_code
      )
    ) |> #merge historical Chase Farm codes
    filter(month >= as.Date("2010-03-01")) |>
    summarise(
      available = sum(available),
      occupied = sum(occupied),
      .by = c(month, organisation_code)
    ) |>
    mutate(bed_occupancy = occupied / available) |>
    mutate(bed_occupancy = bed_occupancy * 100) |>
    filter(organisation_code != "R0A" &
             organisation_code != "RNL") |> #remove controls for Chase Farm
    filter(organisation_code != "RAL" |
             (organisation_code == "RAL" &
                month <= '2020-03-01')) |> #Remove part of Chase farm outcome period affected by covid
    filter(organisation_code != "RGM" |
             (organisation_code == "RGM" &
                month <= '2020-03-01')) #Remove part of Papworth outcome period affected by covid

  return(bed_occupancy_cia_format)
}

# Length of Stay additional formatting
length_of_stay_cia_formatting <- function(data) {
  length_of_stay_cia_format <- data |>
    rename(site_code = der_provider_site_code) |>
    rename(month = yr_mth) |>
    mutate(month = paste0(month, "-01")) |> # Format date
    mutate(month = as.Date(month)) |>
    merge_sites() |>
    summarise(
      los = sum(total_los),
      spells = sum(spells),
      .by = c(site_code, month)
    ) |> #recalculate following merge of sites
    mutate(avg_los = los / spells) |>
    filter(site_code != "RVR05" &
             site_code != "RNLAY" &
             site_code != "RNLBX")
  
  return(length_of_stay_cia_format)
  
}

# Cleaning staff/costs
cleaning_staff_cia_formatting <- function(data) {
  cleaning_staff_cia_format <- data |>
    filter(month >= "2015-03-01") |>
    merge_sites() |>
    summarise(
      cleaning_staff_wte = sum(as.numeric(cleaning_staff_wte),
                               na.rm = TRUE),
      .by = c(site_code, month)
    )
  
  return(cleaning_staff_cia_format)
}

cleaning_costs_cia_formatting <- function(data) {
  
  floor_area<- get_floor_space_by_site(data)|>
    mutate(month=ymd(format(effective_snapshot_date, "%Y-%m-01")))|>
    select(-effective_snapshot_date, -organisation_code)|>
    merge_sites() |>
    summarise( occupied_floor_area_m2= sum(occupied_floor_area_m2,
                                           na.rm=TRUE),
               .by = c(site_code, month))|>
    rbind( c(NA, '2015-03-01', NA))|>
    rbind( c(NA, '2020-03-01', NA))|>
    rbind( c(NA, '2021-03-01', NA ))|>
    mutate(occupied_floor_area_m2=as.numeric(occupied_floor_area_m2))|>
    spread(key=site_code, value=occupied_floor_area_m2)|>
    within(REN22[month == '2020-03-01'] <- REN22[month == '2019-03-01'])|>
    within(REN22[month == '2021-03-01'] <- REN22[month == '2022-03-01'])|>
    within(RGM22[month == '2020-03-01'] <- RGM22[month == '2022-03-01'])|>
    within(RGM22[month == '2021-03-01'] <- RGM22[month == '2022-03-01'])|>
    na_interpolation()|>
    pivot_longer(!month,names_to="site_code", values_to="occupied_floor_area_m2")
  
  
  cleaning_costs_cia_format <- data|>
    select(site_code, month, cleaning_service_cost)|>
    merge_sites() |>
    summarise(
      cost = sum(as.numeric(cleaning_service_cost),
                 na.rm = TRUE),
      .by = c(site_code, month)
    )|>
    left_join(floor_area, by=c("month", "site_code"))|>
    filter(month >= "2015-03-01") |>
    mutate(cleaning_service_cost=(cost/occupied_floor_area_m2))|> #cost in £ per m2
    filter(site_code!="RNLAY" &
             site_code!="RNLBX")
  
  return(cleaning_costs_cia_format)
}


# Emergency readmissions
emergency_readmissions_cia_formatting <- function(data) {
  emergency_readmissions_cia_format <- data |>
    rename(site_code = der_provider_site_code) |>
    rename(month = yr_mth) |>
    mutate(month = paste0(month, "-01")) |>
    mutate(month = as.Date(month)) |> #Format date
    merge_sites() |>
    summarise(
      admits = sum(admits),
      readmits = sum(readmits),
      .by = c(site_code, month)
    ) |> #recalculate rate following merging sites
    mutate(perc = (readmits / admits) * 100) |> # % of readmissions
    filter(
      site_code != "RXPCP" &
        site_code != "RVY01" &
        site_code != "RDEEB" &
        site_code != "RPA02" &
        site_code != "RNLAY" &
        site_code != "RNLBX" &
        site_code!="RA710" &
        site_code!="RTGFA" &
        site_code!="RD130" &
        site_code!="RP5MM"
    )|> #removing control data with issues
  filter(site_code != "RALC7" | (site_code=="RALC7" &
           month < '2019-11-01')) |> #Remove part affected by COVID-19 for Chase farm
    filter(site_code != "RALC7" | (site_code=="RALC7" &
                                     month > '2015-01-01')) #Remove data from pre 2014 when it has emergency dept
    
  
  return(emergency_readmissions_cia_format)
}

#Staff survey
staff_survey_cia_formatting<-function(data) {
  
  staff_survey_cia_format<-data|>
  mutate(month=paste0(year,"-12-31"))|>
  mutate(month=as.Date(month))|>
  filter(!is.na(positive_responses))|>
    mutate(
      organisation_code = ifelse((organisation_code == "REM" |
                                    organisation_code == "RQ6"),
                                 "REM",
                                 organisation_code
      ),
      #merge historical Royal Liverpool codes
      organisation_code = ifelse((organisation_code == "RAL" |
                                    organisation_code == "RVL"),
                                 "RAL",
                                 organisation_code
      )
    ) |> #merge historical Chase Farm codes  
    summarise(positive_responses=mean(positive_responses, na.rm=TRUE), .by = c(month, organisation_code))|>
    filter(organisation_code != "RTK")
      
  
  return(staff_survey_cia_format)
}



# SBR percentages
sbr_percent_cia_formatting <- function(data) {
  sbr_percent_cia_format <-  data |>
    mutate(
      percentage_single_bedrooms = ifelse(
        organisation_code == "RQ6" &
          effective_snapshot_date >= '2019-09-01' &
          effective_snapshot_date < '2020-09-01',
        0,
        percentage_single_bedrooms
      )
    ) |>
    # REM and RQ6 trusts merged so remove all the RQ6 leavers who transferred to REM
    mutate(
      month = floor_date(effective_snapshot_date, "month"),
      #Format date to monthly
      organisation_code = ifelse((organisation_code == "REM" |
                                    organisation_code == "RQ6"),
                                 "REM",
                                 organisation_code
      ),
      #merge historical Royal Liverpool codes
      organisation_code = ifelse((organisation_code == "RAL" |
                                    organisation_code == "RVL"),
                                 "RAL",
                                 organisation_code
      )
    ) |> #merge historical Chase Farm codes
    summarise(
      percentage_single_bedrooms = sum(total_single_bedrooms) / sum(total_available) *
        100,
      .by = c(month, organisation_code)
    )
  
  return(sbr_percent_cia_format)
}
