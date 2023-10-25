library(tidyverse)
library(janitor)

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
  left_join(periods |> select(yyyy_wk, n_period), by = "yyyy_wk") |> 
  mutate(yyyy_wk = fct_reorder(yyyy_wk, n_period))

rm(periods)

df <- df |>
  #add a row
  rbind
c("","","")