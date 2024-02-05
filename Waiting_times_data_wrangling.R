library(RCurl)
library(dplyr)
library(tidyverse)
library(parsedate)
library(janitor)
library(readxl)


# Pulling and wrangling Waiting times data 

#Function
read_rtt<-function(url, name){
  
  tmp = tempfile(fileext = "")
  
  download.file(url = url, destfile = tmp, mode="wb")
  df<-read_excel(tmp, sheet="Provider",range = cell_limits(c(14, 2), c(NA, NA)) )|>
    clean_names()|>
    select(c(1:5,total_number_of_incomplete_pathways, average_median_waiting_time_in_weeks))|>
    mutate(date=name)
 
  assign(name, df, envir=.GlobalEnv)
  
}

#names(Attendance21)<-names(Attendance22)
#names(Attendance20)<-names(Attendance22)






