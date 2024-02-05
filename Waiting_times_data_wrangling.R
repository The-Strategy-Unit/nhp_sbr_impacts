
library(tidyverse)
library(janitor)
library(stringr)


# Wrangling Waiting times data 
pat <- "(\\d)+"

rrt_data<-read.csv("C:/Users/sarah.lucas/OneDrive - NHS/Documents/NHP Single Bed Accomodation/rtt_data_sample.csv")|>
  clean_names()|>
  mutate(weeks=gsub("^>", "", number_of_weeks_since_referral) )|>
  mutate(weeks=sub("\\-.*", "", weeks))|>
  mutate(weeks=sub("\\+.*", "", weeks))|>
  mutate(weeks=as.numeric(weeks))





