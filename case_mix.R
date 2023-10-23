library(tidyverse)
library(janitor)

df <- read.csv("single_room_extract.csv") |> 
  clean_names() |>
  mutate(yyyy_wk = paste0(yr,"-",wk)) |> 
  mutate(avg_los = round(spell_los / spells,2))

# add table to get unique period number for year-wk
periods <- df |>
  group_by(yr,wk) |> 
  distinct(yyyy_wk) |>
  ungroup() |> 
  arrange(yr, wk) |> 
  mutate(n_period = row_number())

df <- df |> 
  left_join(periods |> select(yyyy_wk, n_period), by = "yyyy_wk")

rm(periods)

df |> 
  filter(der_provider_site_code == "REMRQ", adm_meth_desc != 'other') |> 
  group_by(der_provider_site_code, yr, wk, yyyy_wk, n_period) |> 
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) |> 
  ungroup() |> 
  mutate(avg_los = round(spell_los / spells,2)) |>
  ggplot(aes(x=fct_reorder(yyyy_wk, n_period), y=avg_los, group = 1)) +
  geom_line(colour = "#f9bf07") +
  geom_point(colour = "#686F73") +
  geom_vline(xintercept = "2022-43", colour = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  labs(title = "Average length of stay (days) by week"
       ,subtitle = "Royal Liverpool Hospital, Aug 2021 to Aug 2023"
       ,caption = "blue line indicates week of site switch")


df |> 
  filter(der_provider_site_code == "REMRQ", adm_meth_desc != 'other') |> 
  group_by(der_provider_site_code, yr, wk, yyyy_wk, n_period) |> 
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) |> 
  ungroup() |> 
  mutate(avg_los = round(spell_los / spells,2)) |>
  ggplot(aes(x=fct_reorder(yyyy_wk, n_period), y=avg_los, group = 1)) +
  geom_line(colour = "#f9bf07") +
  geom_point(colour = "#686F73") +
  geom_vline(xintercept = "2022-43", colour = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  labs(title = "Average length of stay (days) by week"
       ,subtitle = "Royal Liverpool Hospital, Aug 2021 to Aug 2023"
       ,caption = "blue line indicates week of site switch")

df |> 
  filter(der_provider_site_code == "REMRQ", adm_meth_desc != 'other') |> 
  group_by(der_provider_site_code, adm_meth_desc, yr, wk, yyyy_wk, n_period) |> 
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) |> 
  ungroup() |> 
  mutate(avg_los = round(spell_los / spells,2)) |>
  ggplot(aes(x=fct_reorder(yyyy_wk, n_period), y=avg_los, group = 1)) +
  geom_line(colour = "#f9bf07") +
  geom_point(colour = "#686F73") +
  geom_vline(xintercept = "2022-43", colour = "blue") +
  facet_grid(vars(adm_meth_desc), scales = "free") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  labs(title = "Average length of stay (days) by week"
       ,subtitle = "Royal Liverpool Hospital, Aug 2021 to Aug 2023"
       ,caption = "blue line indicates week of site switch") 

df_top_10_elective <- df |>
  filter(der_provider_site_code == "REMRQ") |> 
  filter(adm_meth_desc == "elective") |>
  group_by(main_specialty_code) |>
  summarise(spells_total = sum(spells))|>
  ungroup() |>
slice_max(spells_total, n = 10)

df_top_10_nonelective <- df |>
  filter(der_provider_site_code == "REMRQ") |> 
  filter(adm_meth_desc == "non_elective") |>
  group_by(main_specialty_code) |>
  summarise(spells_total = sum(spells))|>
  ungroup() |>
  slice_max(spells_total, n = 10)

top_10_elective <- df_top_10_elective$main_specialty_code
top_10_nonelective <- df_top_10_nonelective$main_specialty_code



df |> 
  filter(der_provider_site_code == "REMRQ", adm_meth_desc != 'other') |>
  filter(adm_meth_desc == "elective") |>
  filter(main_specialty_code %in% top_10_elective)|>
  group_by(der_provider_site_code, main_specialty_code, yr, wk, yyyy_wk, n_period) |> 
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) |> 
  ungroup() |> 
  mutate(avg_los = round(spell_los / spells,2)) |>
  ggplot(aes(x=fct_reorder(yyyy_wk, n_period), y=avg_los, group = 1)) +
  geom_line(colour = "#f9bf07") +
  geom_point(colour = "#686F73") +
  geom_vline(xintercept = "2022-43", colour = "blue") +
  facet_wrap(vars(main_specialty_code), scales = "free") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  labs(title = "Average length of stay (days) by week and top 10 elective main specialties"
       ,subtitle = "Royal Liverpool Hospital, Aug 2021 to Aug 2023"
       ,caption = "blue line indicates week of site switch") 


df |> 
  filter(der_provider_site_code == "REMRQ", adm_meth_desc != 'other') |>
  filter(adm_meth_desc == "non_elective") |>
  filter(main_specialty_code %in% top_10_nonelective)|>
  group_by(der_provider_site_code, main_specialty_code, yr, wk, yyyy_wk, n_period) |> 
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) |> 
  ungroup() |> 
  mutate(avg_los = round(spell_los / spells,2)) |>
  ggplot(aes(x=fct_reorder(yyyy_wk, n_period), y=avg_los, group = 1)) +
  geom_line(colour = "#f9bf07") +
  geom_point(colour = "#686F73") +
  geom_vline(xintercept = "2022-43", colour = "blue") +
  facet_wrap(vars(main_specialty_code), scales = "free") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  labs(title = "Average length of stay (days) by week and top 10 non-elective main specialties"
       ,subtitle = "Royal Liverpool Hospital, Aug 2021 to Aug 2023"
       ,caption = "blue line indicates week of site switch") 



df |> 
  filter(der_provider_site_code == "REMRQ", adm_meth_desc != 'other') |> 
  group_by(der_provider_site_code, adm_meth_desc, yr, wk, yyyy_wk, n_period) |> 
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) |> 
  ungroup() |> 
  mutate(avg_los = round(spell_los / spells,2)) |>
  ggplot(aes(x=fct_reorder(yyyy_wk, n_period), y=avg_los, group = 1)) +
  geom_line(colour = "#f9bf07") +
  geom_point(colour = "#686F73") +
  geom_vline(xintercept = "2022-43", colour = "blue") +
  facet_grid(vars(adm_meth_desc), scales = "free") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  labs(title = "Average length of stay (days) by week"
       ,subtitle = "Royal Liverpool Hospital, Aug 2021 to Aug 2023"
       ,caption = "blue line indicates week of site switch") 

df_top_10_elective <- df |>
  filter(der_provider_site_code == "REMRQ") |> 
  filter(adm_meth_desc == "elective") |>
  group_by(main_specialty_code) |>
  summarise(spells_total = sum(spells))|>
  ungroup() |>
  slice_max(spells_total, n = 10)

df_top_10_nonelective <- df |>
  filter(der_provider_site_code == "REMRQ") |> 
  filter(adm_meth_desc == "non_elective") |>
  group_by(main_specialty_code) |>
  summarise(spells_total = sum(spells))|>
  ungroup() |>
  slice_max(spells_total, n = 10)

top_10_elective <- df_top_10_elective$main_specialty_code
top_10_nonelective <- df_top_10_nonelective$main_specialty_code


df |> 
  filter(der_provider_site_code == "REMRQ", adm_meth_desc != 'other') |>
  filter(adm_meth_desc == "elective") |>
  filter(main_specialty_code %in% top_10_elective)|>
  group_by(der_provider_site_code, main_specialty_code, yr, wk, yyyy_wk, n_period) |> 
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) |> 
  ungroup() |> 
  mutate(avg_los = round(spell_los / spells,2)) |>
  ggplot(aes(x=fct_reorder(yyyy_wk, n_period), y=avg_los, group = 1)) +
  geom_line(colour = "#f9bf07") +
  geom_point(colour = "#686F73") +
  geom_vline(xintercept = "2022-43", colour = "blue") +
  facet_wrap(vars(main_specialty_code), scales = "free") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  labs(title = "Average length of stay (days) by week and top 10 elective main specialties"
       ,subtitle = "Royal Liverpool Hospital, Aug 2021 to Aug 2023"
       ,caption = "blue line indicates week of site switch") 

df |> 
  filter(der_provider_site_code == "REMRQ", adm_meth_desc != 'other') |>
  filter(adm_meth_desc == "non_elective") |>
  filter(main_specialty_code %in% top_10_nonelective)|>
  group_by(der_provider_site_code, main_specialty_code, yr, wk, yyyy_wk, n_period) |> 
  summarise(spells = sum(spells)
            ,spell_los = sum(spell_los)) |> 
  ungroup() |> 
  mutate(avg_los = round(spell_los / spells,2)) |>
  ggplot(aes(x=fct_reorder(yyyy_wk, n_period), y=avg_los, group = 1)) +
  geom_line(colour = "#f9bf07") +
  geom_point(colour = "#686F73") +
  geom_vline(xintercept = "2022-43", colour = "blue") +
  facet_wrap(vars(main_specialty_code), scales = "free") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  labs(title = "Average length of stay (days) by week and top 10 elective main specialties"
       ,subtitle = "Royal Liverpool Hospital, Aug 2021 to Aug 2023"
       ,caption = "blue line indicates week of site switch") 

