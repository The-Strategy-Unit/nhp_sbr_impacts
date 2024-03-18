

survey_data_acute <- read.csv("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff survey acute trusts.csv")
survey_data_specialist <- read.csv("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff survey specialist trust.csv")
survey_local_trends <- read.csv("Z:/Strategic Analytics/Projects 2024/1220 - NHP Single Bed Rooms/Data/staff survey local trends detail.csv")|>
  clean_names()|>
  mutate(effective_snapshot_date = as.Date(effective_snapshot_date, "%d/%m/%Y")) |> #Format date
  mutate(month = floor_date(effective_snapshot_date, "month")) |> #Format date to monthly
  mutate(value=as.numeric(value))|>
  rename(organisation_code=trust)|>
  filter((str_detect(month,"2023") & question=="q25c")| # need to add this one not in current dataset
           (str_detect(month,"2022") & question=="q23c")|
           (str_detect(month,"2021") & question=="q21c")|
           (str_detect(month,"2020") & question=="q18c")|
           (str_detect(month,"2019") & question=="q21c")|
           (str_detect(month,"2018") & question=="q21c")|
           (str_detect(month,"2017") & question=="q21c"))
#labels don't match up to the question numbers

#Labels are not the right responses for the Would you recommend your workplace question for 2018 and 2022 
#despite the question numbers being the correct ones according to the questionnaires for those particular years.


survey_data<-rbind(survey_data_acute, survey_data_specialist)|>
  clean_names()|>
  mutate(effective_snapshot_date = as.Date(effective_snapshot_date, "%Y-%m-%d")) |> #Format date
  mutate(month = floor_date(effective_snapshot_date, "month")) |> #Format date to monthly
  mutate(value=as.numeric(value))|>
  rename(organisation_code=trust)

survey_data|>
  filter(organisation_code=="REM"|
           organisation_code=="REN"|
           organisation_code=="RGM"|
           organisation_code=="RGN"|
           organisation_code=="RAL"| 
           organisation_code=="RVJ"|
           organisation_code=="RWF"  )|>
  group_by(month, organisation_code)|>
  summarise(value=mean(value))|>
  ggplot()+
  geom_line(aes(x=month, y=value, group=organisation_code, colour=organisation_code))+
  scale_y_continuous((limits=c(0,NA)))