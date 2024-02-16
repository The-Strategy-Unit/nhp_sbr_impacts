# Workforce

# This script combines the workforce data from UDAL and NHS-Digitial to get a 
  # longer timeseries of Headcount and FTE for NHS organisations.

#### Setup ####
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

# Monthly data from Jul 2019 and yearly Sept data from 2009 to 2018 are from
  # UDAL:
workforce_udal <- read.csv("data/sql_workforce.csv")

# Monthly data from Jan 2016 to Jul 2019 can be found at NHS-Digital: 
workforce_links <- read_excel("data/links_workforce.xlsx")

#### Wrangling ####
# This script uses functions from the data_wrangling_functions.R file.
workforce <- workforce_udal |>
  mutate(effective_snapshot_date = as.Date(effective_snapshot_date, format = "%d/%m/%Y"),
         staff_group = str_to_lower(staff_group) |> 
           str_replace_all(c(" [[:punct:]]" = "",
                             "&" = " ",
                             "," = "",
                             " " = "_",
                             "managers" = "manager"
                             )
                           )
         ) |>
  bind_rows(get_workforce("2016-01-31"),
            get_workforce("2016-02-29"),
            get_workforce("2016-03-31"),
            get_workforce("2016-04-30"),
            get_workforce("2016-05-31"),
            get_workforce("2016-06-30"),
            get_workforce("2016-07-31"),
            get_workforce("2016-08-31"),
            # get_workforce("2016-09-30"), # Sept already available
            get_workforce("2016-10-31"),
            get_workforce("2016-11-30"),
            get_workforce("2016-12-31"),
            
            get_workforce("2017-01-31"),
            get_workforce("2017-02-28"),
            
            get_workforce("2017-03-31"),
            get_workforce("2017-04-30"),
            get_workforce("2017-05-31"),
            get_workforce("2017-06-30"),
            get_workforce("2017-07-31"),
            get_workforce("2017-08-31"),
            # get_workforce("2017-09-30"), # Sept already available
            get_workforce("2017-10-31"),
            get_workforce("2017-11-30"),
            get_workforce("2017-12-31"),
            
            get_workforce("2018-01-31"),
            get_workforce("2018-02-28"),
            get_workforce("2018-03-31"),
            get_workforce("2018-04-30"),
            get_workforce("2018-05-31"),
            get_workforce("2018-06-30"),
            get_workforce("2018-07-31"),
            get_workforce("2018-08-31"),
            #  get_workforce("2018-09-30"), # Sept already available
            get_workforce("2018-10-31"),
            get_workforce("2018-11-30"),
            get_workforce("2018-12-31"),
            
            get_workforce("2019-01-31"),
            get_workforce("2019-02-28"),
            get_workforce("2019-03-31"),
            get_workforce("2019-04-30"),
            get_workforce("2019-05-31"),
            get_workforce("2019-06-30")
            ) |>
  pivot_wider(names_from = data_type, values_from = total)

#### Checks ####
summary(workforce)

# Quick plot to check what months we have and data quality:
library(plotly)
library(ggplot2)

ggplotly(
  workforce |>
   # filter(org_code == "RAE") |> # can look at specific organisation
    summarise(FTE = sum(FTE, na.rm = TRUE), 
              .by = c(effective_snapshot_date)
              ) |>
    ggplot(aes(effective_snapshot_date, FTE)) +
    geom_line() +
    geom_point() +
    theme_bw() 
  )

#### Saving output ####
write.csv(workforce, "Data/formatted_workforce.csv", row.names = FALSE)



