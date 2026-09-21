

survey_data_2014 <- read.csv("Data/staff_survey_2014.csv")
survey_data_2015 <- read.csv("Data/staff_survey_2015.csv")
survey_data_2016 <- read.csv("Data/staff_survey_2016.csv")
survey_data_2017 <- read.csv("Data/staff_survey_2017.csv")
survey_data_2018 <- read.csv("Data/staff_survey_2018.csv")
survey_data_2019 <- read.csv("Data/staff_survey_2019.csv")
survey_data_2020 <- read.csv("Data/staff_survey_2020.csv")
survey_data_2021 <- read.csv("Data/staff_survey_2021.csv")
survey_data_2022 <- read.csv("Data/staff_survey_2022.csv")


survey_data<-survey_data_2014|>
full_join(survey_data_2015, by=c("organisation_code") )|>
  full_join(survey_data_2016, by=c("organisation_code") )|>
  full_join(survey_data_2017, by=c("organisation_code") )|>
  full_join(survey_data_2018, by=c("organisation_code") )|>
  full_join(survey_data_2019, by=c("organisation_code") )|>
  full_join(survey_data_2020, by=c("organisation_code") )|>
  full_join(survey_data_2021, by=c("organisation_code") )|>
  full_join(survey_data_2022, by=c("organisation_code") )|>
  gather(key="year", value="positive_responses", -organisation_code)|>
  mutate(year=str_sub(year,-4,-1))


