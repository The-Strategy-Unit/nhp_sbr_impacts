
library(tidyverse)
library(janitor)
library(stringr)
library(tsibble)
library(readxl)
library(zoo)
library(lubridate)


# RTT waiting times 
rtt_data_formatting<-function(data_file){  


#Functions to pull xls files for RTT waiting times prior to April 2011

  read_rtt<-function(url, name){
  
  tmp = tempfile(fileext = "")
  
  download.file(url = url, destfile = tmp, mode="wb")
  df<-read_excel(tmp, sheet="Provider",range = cell_limits(c(14, 3), c(NA, NA)) )|>
    select(1, 3, 5:57)|>
    gather(key=number_of_weeks_since_referral, value=number_of_incomplete_pathways, -1,-2)|>
    mutate(month=name)|>
    clean_names()|>
    rename(organisation_code=org_code)
  
  assign(name, df, envir=.GlobalEnv)
  
}


read_rtt2<-function(url, name){
 
  tmp = tempfile(fileext = "")
  
  download.file(url = url, destfile = tmp, mode="wb")
  df<-read_excel(tmp, sheet="Providers",range = cell_limits(c(6, 2), c(NA, NA)) )|>
    select(1, 3, 5:57)|>
    gather(key=number_of_weeks_since_referral, value=number_of_incomplete_pathways, -1,-2)|>
    mutate(month=name)|>
    clean_names()|>
    rename(organisation_code=code)
  
  assign(name, df, envir=.GlobalEnv)
  
}

read_rtt3<-function(url, name){
  
  tmp = tempfile(fileext = "")
  
  download.file(url = url, destfile = tmp, mode="wb")
  df<-read_excel(tmp, sheet="Providers",range = cell_limits(c(7, 2), c(NA, NA)) )|>
    select(1, 3, 5:57)|>
    gather(key=number_of_weeks_since_referral, value=number_of_incomplete_pathways, -1,-2)|>
    mutate(month=name)|>
    clean_names()|>
    rename(organisation_code=code)
  
  assign(name, df, envir=.GlobalEnv)
  
}

read_rtt4<-function(url, name){
  
  tmp = tempfile(fileext = "")
  
  download.file(url = url, destfile = tmp, mode="wb")
  df<-read_excel(tmp, sheet="Providers",range = cell_limits(c(9, 2), c(NA, NA)) )|>
    select(1, 3, 5:57)|>
    gather(key=number_of_weeks_since_referral, value=number_of_incomplete_pathways, -1,-2)|>
    mutate(month=name)|>
    clean_names()|>
    rename(organisation_code=code)
  
  assign(name, df, envir=.GlobalEnv)
  
}


#2010-2011
read_rtt("https://webarchive.nationalarchives.gov.uk/ukgwa/20130104202122mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_126945.xls", "Mar 2011")
read_rtt("https://webarchive.nationalarchives.gov.uk/ukgwa/20130104202122mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_128319.xls", "Feb 2011")
read_rtt("https://webarchive.nationalarchives.gov.uk/ukgwa/20130104202122mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_128312.xls", "Jan 2011")
read_rtt("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_128301.xls", "Dec 2010")
read_rtt3("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_123623.xls", "Nov 2010")
read_rtt3("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_122781.xls", "Oct 2010")
read_rtt3("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_121819.xls", "Sep 2010")
read_rtt3("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_132303.xls", "Aug 2010")
read_rtt3("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_119386.xls", "Jul 2010")
read_rtt3("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_118704.xls", "Jun 2010")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_117449.xls", "May 2010")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_123542.xls", "Apr 2010")


#2009-2010
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_132297.xls", "Mar 2010")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_115407.xls", "Feb 2010")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020034mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_114103.xls", "Jan 2010")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_112615.xls", "Dec 2009")
read_rtt4("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_111339.xls", "Nov 2009")
read_rtt4("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_110153.xls", "Oct 2009")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_108744.xls", "Sep 2009")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_110194.xls", "Aug 2009")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_132304.xls", "Jul 2009")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_110211.xls", "Jun 2009")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_110222.xls", "May 2009")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_110218.xls", "Apr 2009")

#2008-2009
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_099876.xls", "Mar 2009")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_100042.xls", "Feb 2009")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020037mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_099989.xls", "Jan 2009")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020040mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_095414.xls", "Dec 2008")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020040mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_102155.xls", "Nov 2008")
read_rtt2("https://webarchive.nationalarchives.gov.uk/ukgwa/20130105020040mp_/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_102163.xls", "Oct 2008")

# merge pre 2011 files together
pre2011_rtt_data<-rbind(`Mar 2011`, `Feb 2011`, `Jan 2011`, `Dec 2010`, `Nov 2010`, `Oct 2010`, `Sep 2010`,`Aug 2010`,
                        `Jul 2010`,`Jun 2010`, `May 2010`, `Apr 2010`,`Mar 2010`, `Feb 2010`, `Jan 2010`,`Dec 2009`,
                        `Nov 2009`,`Oct 2009`, `Sep 2009`, `Aug 2009`,`Jul 2009`, `Jun 2009`, `May 2009`, `Apr 2009`,
                        `Mar 2009`, `Feb 2009`,`Jan 2009`, `Dec 2008`,`Nov 2008`,`Oct 2008`)

pre2011_rtt_data<- pre2011_rtt_data|>
  mutate(month=zoo::as.yearmon(month,format ="%b %Y"))|> #Format date
  mutate(month=as.Date(month,frac=0 )) |>#Format date to monthly
mutate(weeks=gsub("^>", "", number_of_weeks_since_referral) )|>
  mutate(weeks=sub("\\-.*", "", weeks))|>
  mutate(weeks=sub("\\plus.*", "", weeks))|>
  mutate(weeks=as.numeric(weeks))|>
  mutate(number_of_incomplete_pathways_with_dta=NA)|>
  mutate(treatment_function_code=gsub("^IP", "", treatment_function_code) )

assign("pre2011_rtt_data", pre2011_rtt_data, envir=.GlobalEnv)

# RTT Waiting times April 2011 to Oct 2023
rtt_data<-read.csv(data_file)|>
  clean_names()|>
  mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%Y-%m-%d"))|> #Format date 
  mutate(month=floor_date(effective_snapshot_date, "month"))|> #Format date to monthly
  mutate(weeks=gsub("^>", "", number_of_weeks_since_referral) )|>
  mutate(weeks=sub("\\-.*", "", weeks))|>
  mutate(weeks=sub("\\+.*", "", weeks))|>
  mutate(weeks=as.numeric(weeks))|>
  select(-effective_snapshot_date)

assign("rtt_data", rtt_data, envir=.GlobalEnv)


formatted_rtt_data<-rbind(rtt_data, pre2011_rtt_data)|>
  filter(treatment_function_code=="999") #999 is the total for each provider


write.csv(formatted_rtt_data, "Data/formatted_rtt_data.csv", row.names=FALSE)

}

rtt_data_formatting("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/Data/rtt_waiting_times.csv")




rtt_data<-read.csv("Data/formatted_rtt_data.csv")


# Plot of median wait time (in weeks)
rtt_plot_median<-function(data){
  data|>
  group_by(month)|>
summarise(median_by_prov = median(rep(weeks,number_of_incomplete_pathways)))|> #Median by month and provider 
    mutate(month = tsibble::yearmonth(month))|> #Format date to monthly
  ggplot(aes(x = month, y =median_by_prov)) + 
  geom_line(color = "#2c2825", size=1) +
  labs( title = "",y = "median wait (wks)", x="")+
  scale_y_continuous(limits=c(0,NA))
}
  
  rtt_plot_median(rtt_data)
  


# Plot of total number waiting
  rtt_plot_number_waiting<-function(data){
    data|>
  group_by(month)|>
  summarise(total_waiting = sum(number_of_incomplete_pathways))|> #Median by month and provider 
      mutate(month = tsibble::yearmonth(month))|> #Format date to monthly    
  ggplot(aes(x = month, y =total_waiting)) + 
  geom_line(color = "#2c2825", size=1) +
  labs( title = "",y = "No. of patients on waiting list", x="")+
  scale_y_continuous(limits=c(0,NA))
  }
  
  
  rtt_plot_number_waiting(rtt_data)

#Friends and Family format data 
  friends_and_family_scores_data_formatting<-function(data1, data2){
    
    #pre 2022-07 data
    friends_and_family_scores_data1<-read.csv(data1)|>
      clean_names()|>
      mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%Y-%m-%d"))|> #Format date
      mutate(month=floor_date(effective_snapshot_date, "month"))|> #Format date to monthly
      mutate(count=ifelse(count=="NULL", 0, count))|>
      mutate(count=as.numeric(count))|>
      mutate(positive_responses=ifelse(likely_to_recommend=="Very Good"|likely_to_recommend=="Good"|
                                         likely_to_recommend=="Likely"|likely_to_recommend=="Extremely Likely" , "yes", "no"))|> #flag those that are +ive responses
      filter(grouped_by=="Site")|>
      group_by(month, site_code)|>
      mutate(percent=round(((count/sum(count))*100),1))|> #calculate %
      filter(positive_responses=="yes")|>
      summarise(percent=sum(percent)) #calculate %
    
    #Post 2022-07 data  
    friends_and_family_scores_data2<-read.csv(data2)|>
      clean_names()|>
      mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%d/%m/%Y"))|> #Format date
      mutate(month=floor_date(effective_snapshot_date, "month"))|> #Format date to monthly
      filter(measure_category=="Percentage Positive")|>
      mutate(percent=as.numeric(measure_value)*100)|>
      select(-effective_snapshot_date, -measure_category, -measure_value, -measure_name)|>
      filter(month>='2022-08-01')
    
    friends_and_family_scores_data<-rbind(friends_and_family_scores_data1,friends_and_family_scores_data2)
    
    
    write.csv(friends_and_family_scores_data, "Data/formatted_friends_and_family_data.csv", row.names=FALSE) 
    

}
  
  friends_and_family_scores_data_formatting("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/Data/friends_and_family_inpatient_scores.csv",
                                            "Data/friend_and_family_inpatient_scores_post_Jul2022.csv")

    
  friends_and_family_scores_data<-read.csv("Data/formatted_friends_and_family_data.csv")
  
#Friends and Family calculate percent and plot data
 friends_family_scores_plot<-function(data){
   
   data |>
     group_by(month)|>
     summarise(percent=mean(percent))|>
  mutate(month = yearmonth(month))|> #Format date to monthly       
  ggplot(aes(x = month, y =percent)) + 
  geom_line(color = "#2c2825", size=1) +
  labs( title = "",y = "% positive responses", x="")+
  scale_y_continuous(limits=c(0,100))
}

 friends_family_scores_plot(friends_and_family_scores_data)
 
#Health Care acquired infections (HCAI) format and merge data 
 
hcai_formatting<-function(cdiff_pre_2018, cdiff, ecoli, kleb, mssa, mrsa, p_aeruginosa){
  
hai_cdiff_pre_2018_data<-read.csv(cdiff_pre_2018)|>
  clean_names()|>
  mutate(collection="C.difficile")|>
  rename(organisation_code=provider_code)|>
  mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%Y-%m-%d")) |>#Format date
  mutate(month = yearmonth(effective_snapshot_date))|> #Format date to monthly
  select(-count_of_cases_str, -effective_snapshot_date)

hai_cdiff_data<-read.csv(cdiff)|>
  mutate(Collection="C.difficile")|>
  mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%Y-%m-%d"))#Format date

hai_ecoli_data<-read.csv(ecoli)|>
  mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%d/%m/%Y")) #Format date

hai_klebsiella_data<-read.csv(kleb)|>
  mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%Y-%m-%d"))#Format date

hai_mssa_data<-read.csv(mssa)|>
  mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%d/%m/%Y")) #Format date

hai_mrsa_data<-read.csv(mrsa)|>
  mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%d/%m/%Y")) #Format date

hai_p_aeruginosa_data<-read.csv(p_aeruginosa)|>
  mutate(Effective_Snapshot_Date=as.Date(Effective_Snapshot_Date,"%d/%m/%Y")) #Format date


HCAI_data<-rbind(hai_cdiff_data,  hai_klebsiella_data,hai_ecoli_data, hai_mssa_data,  hai_mrsa_data ,hai_p_aeruginosa_data )|> 
   clean_names()|>
  mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%Y-%m-%d")) |>#Format date
  mutate(month=floor_date(effective_snapshot_date, "month"))|>
   rename(count_of_cases=figure)|>
   mutate(count_of_cases=as.numeric(count_of_cases))|>
   filter(metric=="HOHA cases"|metric=="Hospital-onset"|metric=="Hospital-onset, healthcare associated")|> #Select only hospital acquired
   filter(organisation_type=="NHS Acute Trust"|organisation_type=="NHS acute trust")|>
   select(-effective_snapshot_date, -organisation_type, -metric)|>
     rbind(hai_cdiff_pre_2018_data)
 
 write.csv(HCAI_data, "Data/formatted_HCAI_data.csv", row.names=FALSE)
}

hcai_formatting("Data/hai_cdiff_pre_2018.csv", 
                "Data/hai_cdiff.csv",
                "Data/hai_ecoli.csv",
                 "Data/hai_klebsiella.csv",
                 "Data/hai_mssa.csv",
                "Data/hai_mrsa.csv",
                "Data/hai_p_aeruginosa.csv")
                 
 #Health Care acquired infections (HCAI) plot data

HCAI_data<-read.csv("Data/formatted_HCAI_data.csv")

 hcai_plot<-function(data){
   
   data |>
     mutate(month = yearmonth(month))|> #Format date to monthly     
 # filter(organisation_code=="REM")|>
   group_by(month, collection)|>
   summarise(count_of_cases=sum(count_of_cases))|>
   ggplot(aes(x = month, y =count_of_cases, group=collection, color=collection) ) + 
   geom_line(size=1) +
   labs( title = "",y = "No. of cases", x="")+
   theme( legend.position= c(0.25, 0.8))+
   scale_y_continuous(limits=c(0, NA))
}
 
 hcai_plot(HCAI_data)
 
 #Staff sickness format data
 
staff_sickness_absence_formatting<-function(data){
  
  sickness_absence_data<-read.csv(data)|>
   clean_names()|>
    mutate(effective_snapshot_date=as.Date(effective_snapshot_date,"%d/%m/%Y"))|> #Format date
    mutate(month=floor_date(effective_snapshot_date, "month"))|> #Format date to monthly
  select(-organisation_type,-effective_snapshot_date )|>
  mutate(fte_days_sick=as.numeric(fte_days_sick))|>
  mutate(fte_days_sick=ifelse(is.na(fte_days_sick),0, fte_days_sick))|>
  mutate(fte_days_available=as.numeric(fte_days_available))|>
  mutate(fte_days_available=ifelse(is.na(fte_days_available),0, fte_days_available))|>
    mutate(percent=(fte_days_sick/fte_days_available)*100)
  
  write.csv(sickness_absence_data,"Data/formatted_staff_sickness_absence.csv", row.names=FALSE )
  
}
  
staff_sickness_absence_formatting ("Data/staff_sickness_absence.csv")
  
#Staff sickness plot data

sickess_absence_data<-read.csv("Data/formatted_staff_sickness_absence.csv")

staff_sickness_absence_plot<-function(data){
  
 data|>
 #filter(organisation_code=="REM")|>
  mutate(month = yearmonth(month))|> #Format date to monthly   
  group_by(month)|>
  summarise(rate=(sum(fte_days_sick)/ sum(fte_days_available))*100)|>
  ggplot(aes(x = month, y =rate)) + 
  geom_line(color = "#2c2825", size=1) +
  labs( title = "",y = "Sickness absence rate (%)", x="")+
  scale_y_continuous(limits=c(0,NA))
  
}

staff_sickness_absence_plot(sickess_absence_data)