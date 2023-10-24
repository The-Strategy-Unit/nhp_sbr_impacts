library(NHSRplotthedots)

######################

library(NHSRdatasets)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

data("ae_attendances")

ae_attendances %>% 
  filter(org_code == "RRK", type == 1) %>% 
  ggplot(aes(x = period, y = breaches)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("4-hour target breaches", labels = comma) +
  scale_x_date("Date") +
  labs(title = "Example plot of A&E breaches for organsiation: 'RRK'") +
  theme_minimal()



######################




df <- read.csv("single_room_extract.csv") |> 
  clean_names() |>
  mutate(wk_2 = ifelse(wk<10, paste0(0,wk), wk)) |> 
  mutate(yyyy_wk = paste0(yr,"-",wk_2)) |> 
  mutate(avg_los = round(spell_los / spells,2))

# add table to get unique period number for year-wk
periods <- df |>
  group_by(yr,wk) |> 
  distinct(yyyy_wk) |>
  ungroup() |> 
  arrange(yr, wk) |> 
  mutate(n_period = row_number())

df <- df |> 
  left_join(periods |> select(yyyy_wk, n_period), by = "yyyy_wk") |> 
  mutate(yyyy_wk = fct_reorder(yyyy_wk, n_period))

rm(periods)

df_pre <- df |> 
  filter(between(n_period,72 ,136))

df_post <- df |> 
  filter(n_period > 136)

df |>
  mutate(admit_month_start = as.Date(paste0((as.character(yr),as.character(mth),"01")))
as.Date(paste0("2022","09","01"))


df %>% 
  filter(der_provider_site_code == "REMRQ",adm_meth_desc != 'other', between(n_period, 72, 181)) %>%
  ggplot(aes(x=fct_reorder(yyyy_wk, n_period), y=avg_los, group = 1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("LoS in days") +
  #scale_x_date("Date") +
  labs(title = "Example plot for organsiation: 'REMRQ'") +
  theme_minimal()



stable_set <- df %>% 
  filter(der_provider_site_code == "REMRQ",
         adm_meth_desc != 'other', between(n_period, 72, 181)) %>% 
  mutate(date1=ymd(case_when(mth<=9 ~ as.character(paste0(as.character(yr),"0",as.character(mth),"01")),
                         mth>9 ~ as.character(paste0(as.character(yr),as.character(mth),"01")))
  )
  ) %>% 
  group_by(der_provider_site_code, yr, wk, yyyy_wk, n_period,date1) %>%  
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) %>%  
  ungroup() %>% 
  mutate(avg_los = round(spell_los / spells,2)) 



ptd_spc(stable_set, value_field = avg_los, date_field = date1, improvement_direction = "decrease")