
library(tidyverse)
library(janitor)
library(stringr)
library(tsibble)


# Waiting times
rtt_data<-read.csv("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/NHP SBR data/rtt_waiting_times.csv")|>
  clean_names()|>
  mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%Y-%m-%d"))|> #Format date
  mutate(month = yearmonth(effective_snapshot_date))|> #Format date to monthly
  mutate(weeks=gsub("^>", "", number_of_weeks_since_referral) )|>
  mutate(weeks=sub("\\-.*", "", weeks))|>
  mutate(weeks=sub("\\+.*", "", weeks))|>
  mutate(weeks=as.numeric(weeks))


# Plot of median wait time (in weeks)
rtt_data|>
  #group_by(organisation_code, month)|>
  #filter(organisation_code=="RJE")|>
  group_by(month)|>
summarise(median_by_prov = median(rep(weeks,number_of_incomplete_pathways)))|> #Median by month and provider 
  ggplot(aes(x = month, y =median_by_prov)) + 
  geom_line(color = "#2c2825", size=1) +
  labs( title = "",y = "median wait (wks)", x="")+
  scale_y_continuous(limits=c(0,NA))

# Plot of total number waiting
rtt_data|>
  #group_by(organisation_code, month)|>
  #filter(organisation_code=="RJE")|>
  group_by(month)|>
  summarise(total_waiting = sum(number_of_incomplete_pathways))|> #Median by month and provider 
  ggplot(aes(x = month, y =total_waiting)) + 
  geom_line(color = "#2c2825", size=1) +
  labs( title = "",y = "No. of patients on waiting list", x="")+
  scale_y_continuous(limits=c(0,NA))


#Friends and Family format data  
friends_and_family_scores_data<-read.csv("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/NHP SBR data/friends_and_family_inpatient_scores.csv")|>
  clean_names()|>
  mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%Y-%m-%d"))|> #Format date
  mutate(month = yearmonth(effective_snapshot_date))|> #Format date to monthly
  mutate(count=ifelse(count=="NULL", 0, count))|>
  mutate(count=as.numeric(count))|>
  mutate(positive_responses=ifelse(likely_to_recommend=="Very Good"|likely_to_recommend=="Good"|
                                     likely_to_recommend=="Likely"|likely_to_recommend=="Extremely Likely" , "yes", "no")) #flag those that are +ive responses


#Friends and Family calculate percent and plot data
friends_and_family_scores_data |>
  filter(grouped_by=="Site")|>
  group_by(month)|>
  mutate(percent=round(((count/sum(count))*100),1))|> #calculate %
  mutate(percent=ifelse(is.nan(percent), 0 , percent))|>
  filter(positive_responses=="yes")|>
  summarise(percent=sum(percent))|>
  ggplot(aes(x = month, y =percent)) + 
  geom_line(color = "#2c2825", size=1) +
  labs( title = "",y = "% positive responses", x="")+
  scale_y_continuous(limits=c(0,100))


#Health Care acquired infections (HCAI) format and merge data 
  
hai_cdiff_pre_2018_data<-read.csv("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/NHP SBR data/hai_cdiff_pre_2018.csv")|>
  clean_names()|>
  mutate(collection="C.difficile")|>
  rename(organisation_code=provider_code)|>
  mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%Y-%m-%d")) |>#Format date
  mutate(month = yearmonth(effective_snapshot_date))|> #Format date to monthly
  select(-count_of_cases_str, -effective_snapshot_date)

hai_cdiff_data<-read.csv("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/NHP SBR data/hai_cdiff.csv")|>
  mutate(Collection="C.difficile")|>
  mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%Y-%m-%d"))#Format date

hai_ecoli_data<-read.csv("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/NHP SBR data/hai_ecoli.csv")|>
  mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%d/%m/%Y")) #Format date

hai_klebsiella_data<-read.csv("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/NHP SBR data/hai_klebsiella.csv")|>
  mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%Y-%m-%d"))#Format date

hai_mssa_data<-read.csv("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/NHP SBR data/hai_mssa.csv")|>
  mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%d/%m/%Y")) #Format date

hai_mrsa_data<-read.csv("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/NHP SBR data/hai_mrsa.csv")|>
  mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%d/%m/%Y")) #Format date

hai_p_aeruginosa_data<-read.csv("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/NHP SBR data/hai_p_aeruginosa.csv")|>
  mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%d/%m/%Y")) #Format date


 All_HCAI<-rbind(hai_cdiff_data,  hai_klebsiella_data,hai_ecoli_data, hai_mssa_data,  hai_mrsa_data ,hai_p_aeruginosa_data )|> 
   clean_names()|>
   mutate(month = yearmonth(effective_snapshot_date))|> #Format date to monthly
   rename(count_of_cases=figure)|>
   mutate(count_of_cases=as.numeric(count_of_cases))|>
   filter(metric=="HOHA cases"|metric=="Hospital-onset"|metric=="Hospital-onset, healthcare associated")|> #Select only hospital acquired
   filter(organisation_type=="NHS Acute Trust"|organisation_type=="NHS acute trust")|>
   select(-effective_snapshot_date, -organisation_type, -metric)|>
     rbind(hai_cdiff_pre_2018_data)
     
 #Health Care acquired infections (HCAI) plot data
 All_HCAI |>
 # filter(organisation_code=="REM")|>
   group_by(month, collection)|>
   summarise(count_of_cases=sum(count_of_cases))|>
   ggplot(aes(x = month, y =count_of_cases, group=collection, color=collection) ) + 
   geom_line(size=1) +
   labs( title = "",y = "No. of cases", x="")+
   theme( legend.position= c(0.25, 0.8))+
   scale_y_continuous(limits=c(0, NA))

 
 #Staff sickness format data
 
sickness_absence_data<-read.csv("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/NHP SBR data/staff_sickness_absence.csv")|>
   clean_names()|>
  mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%d/%m/%Y"))|> #Format date
  mutate(month = yearmonth(effective_snapshot_date))|> #Format date to monthly
  select(-organisation_type,-effective_snapshot_date )|>
  mutate(fte_days_sick=as.numeric(fte_days_sick))|>
  mutate(fte_days_sick=ifelse(is.na(fte_days_sick),0, fte_days_sick))|>
  mutate(fte_days_available=as.numeric(fte_days_available))|>
  mutate(fte_days_available=ifelse(is.na(fte_days_available),0, fte_days_available))

#Staff sickness plot data
sickness_absence_data|>
 #filter(organisation_code=="REM")|>
  group_by(month)|>
  summarise(rate=(sum(fte_days_sick)/ sum(fte_days_available))*100)|>
  ggplot(aes(x = month, y =rate)) + 
  geom_line(color = "#2c2825", size=1) +
  labs( title = "",y = "Sickness absence rate (%)", x="")+
  scale_y_continuous(limits=c(0,NA))