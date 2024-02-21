library(tidyverse)
library(janitor)

options(scipen = 999)

##load the beddays file

fun_load_beddays <- function(filepth){
  
  data <- read.csv(filepth) |> 
  clean_names() |> 
  mutate(yearmon = lubridate::ym(period))
  
  return(data)

}

beddays <- fun_load_beddays("Data/nhp_hospsite_beddays_mth.csv")

##generate bed days plot
fun_bedday_plot <- function(site){
  
beddays |> 
    filter(der_provider_site_code == site) |>
    ggplot(aes(x=yearmon, y = beddays, group = 1)) +
    annotate("rect", xmin = ymd("2020-03-01"), xmax = ymd("2021-07-01"), ymin = -Inf, ymax = Inf
             , fill = "blue", alpha = .2) +
    geom_line() +
    scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
    expand_limits(y = 0) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6)) +
    labs(x = "year-month",
         y = "total bed days in period",
         title = "Total inpatient bed days by calendar month, Apr 2008 to Nov 2023",
         subtitle = paste0("Provider Site Code: ",site),
         caption = "Blue shaded area = period of national and local lockdowns")

}

bd_plot <- fun_bedday_plot("RWFTW")
bd_plot

##load the inpatient activity file

fun_load_sus_apcs <- function(filepth){
  
    data <- read.csv(filepth) |> 
    clean_names() |>
    mutate(los = as.integer(str_replace(los, "NULL", "0"))
           ,mon2 = case_when(mth < 10 ~ paste0("0",as.factor(mth))
                  ,TRUE ~ as.factor(mth))
           ,yearmon = lubridate::ym(paste0(as.factor(yr),mon2))) |> 
    select(-mon2) |> 
    left_join(beddays |> select(der_provider_site_code, yearmon, beddays), by = c("der_provider_site_code", "yearmon")) |> 
    mutate(beddays = if_else(beddays < 0, 0, beddays)
           ,avg_los = los/spells
           ,ff_rate = fall_fracs/beddays*1000
           ,death_rate = deaths/spells*100)
    
    return(data)
}

apcs <- fun_load_sus_apcs("Data/nhp_hospsite_apcs_mth.csv")

##load the cost file yearly

fun_load_sus_cost_yr <- function(filepth){
  
  read.csv(filepth) |> 
    clean_names()
}

cost_yr <- fun_load_sus_cost_yr("Data/nhp_hospsite_costs_yr.csv")

##load the cost file monthly

fun_load_sus_cost_mth <- function(filepth){
  
  read.csv(filepth) |> 
    clean_names()
}

cost_mth <- fun_load_sus_cost_mth("Data/nhp_hospsite_costs_mth.csv")



