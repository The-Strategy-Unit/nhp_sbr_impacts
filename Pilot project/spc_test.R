library(NHSRplotthedots)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(janitor)
library(tidyverse)


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
  mutate(n_period = row_number()
         ,pseudo_date = as.Date("2020-04-04") + (n_period*7) - 7)

df <- df |> 
  left_join(periods |> select(yyyy_wk, n_period, pseudo_date), by = "yyyy_wk") |> 
  mutate(yyyy_wk = fct_reorder(yyyy_wk, n_period))

rm(periods)


df_pre <- df |> 
  filter(between(n_period,71 ,133))

df_post <- df |> 
  filter(n_period > 134)




df %>% 
  filter(der_provider_site_code == "REMRQ",adm_meth_desc != 'other', between(n_period, 71, 181)) %>%
  group_by(der_provider_site_code, yr, wk, yyyy_wk, n_period,pseudo_date) %>%  
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) %>%  
  ungroup() %>% 
  mutate(avg_los = round(spell_los / spells,2)) %>% 
  ggplot(aes(x=pseudo_date, y=avg_los, group = 1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("LoS in days") +
  scale_x_date("pseudo_date") +
  labs(title = "Example plot for organsiation: 'REMRQ'") +
  theme_minimal()


stable_set <- df %>% 
  filter(der_provider_site_code == "REMRQ",adm_meth_desc != 'other', between(n_period, 71, 181)) %>%
  group_by(der_provider_site_code, yr, wk, yyyy_wk, n_period,pseudo_date) %>%  
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) %>%  
  ungroup() %>% 
  mutate(avg_los = round(spell_los / spells,2))

#ptd_spc(stable_set, value_field = avg_los, date_field = pseudo_date, improvement_direction = "decrease")

change_set <- stable_set 

ptd_spc(change_set,
        value_field = avg_los,
        date_field = pseudo_date,
        improvement_direction = "decrease",
        rebase = ptd_rebase(as.Date("2022-10-22")))

#elective
stable_set <- df %>% 
  filter(der_provider_site_code == "REMRQ",adm_meth_desc != 'other', between(n_period, 71, 181),adm_meth_desc=="elective") %>%
  group_by(der_provider_site_code, yr, wk, yyyy_wk, n_period,pseudo_date) %>%  
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) %>%  
  ungroup() %>% 
  mutate(avg_los = round(spell_los / spells,2))

#ptd_spc(stable_set, value_field = avg_los, date_field = pseudo_date, improvement_direction = "decrease")

change_set <- stable_set 

ptd_spc(change_set,
        value_field = avg_los,
        date_field = pseudo_date,
        improvement_direction = "decrease",
        rebase = ptd_rebase(as.Date("2022-10-22")))

#non-elective
stable_set <- df %>% 
  filter(der_provider_site_code == "REMRQ",adm_meth_desc != 'other', between(n_period, 71, 181),adm_meth_desc=="non_elective") %>%
  group_by(der_provider_site_code, yr, wk, yyyy_wk, n_period,pseudo_date) %>%  
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) %>%  
  ungroup() %>% 
  mutate(avg_los = round(spell_los / spells,2))

#ptd_spc(stable_set, value_field = avg_los, date_field = pseudo_date, improvement_direction = "decrease")

change_set <- stable_set 

ptd_spc(change_set,
        value_field = avg_los,
        date_field = pseudo_date,
        improvement_direction = "decrease",
        rebase = ptd_rebase(as.Date("2022-10-22")))




