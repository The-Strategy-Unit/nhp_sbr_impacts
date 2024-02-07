
library(tidyverse)
library(janitor)
library(stringr)
library(tsibble)


# Wrangling Waiting times data to calculate medians

rtt_data<-read.csv("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/rtt_data_sample.csv")|>
  clean_names()|>
  mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%d/%m/%y"))|> #Format date
  mutate(Month = yearmonth(effective_snapshot_date))|> #Format date to monthly
  mutate(weeks=gsub("^>", "", number_of_weeks_since_referral) )|>
  mutate(weeks=sub("\\-.*", "", weeks))|>
  mutate(weeks=sub("\\+.*", "", weeks))|>
  mutate(weeks=as.numeric(weeks))|>
  group_by(organisation_code)|>
  reframe(median_by_prov_treatment = median(rep(weeks,number_of_incomplete_pathways)))
  
  
  
  cbind(  group_by(organisation_code, Month)|>
  summarise(median_by_prov = median(rep(weeks,number_of_incomplete_pathways)))
  
  summarise(total_waiting=sum(number_of_incomplete_pathways), weeks)
  #as_tsibble(key=c(organisation_code, treatment_function_code), index = Month)
