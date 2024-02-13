# ERIC data

#### functions ####
# uses functions from data_wrangling_functions.R file

#### reading site data in ####
eric_22_23_site <- scrape_csv("https://files.digital.nhs.uk/41/5787C9/ERIC%20-%202022_23%20-%20Site%20data.csv")
eric_21_22_site <- scrape_csv("https://files.digital.nhs.uk/EE/7E330D/ERIC%20-%20202122%20-%20Site%20Data%20v3.csv")
eric_20_21_site <- scrape_csv("https://files.digital.nhs.uk/0F/46F719/ERIC%20-%20202021%20-%20Site%20data%20v2.csv")
eric_19_20_site <- scrape_csv("https://files.digital.nhs.uk/11/BC1043/ERIC%20-%20201920%20-%20SiteData%20-%20v2.csv")
eric_18_19_site <- scrape_csv("https://files.digital.nhs.uk/63/ADBFFF/ERIC%20-%20201819%20-%20SiteData%20v4.csv")

eric_17_18_site <- scrape_csv("https://files.digital.nhs.uk/A8/188D99/ERIC-201718-SiteData.csv")
eric_16_17_site <- scrape_csv("https://files.digital.nhs.uk/publication/q/3/eric-201617-site-data.csv")

eric_15_16_site <- scrape_csv("https://files.digital.nhs.uk/publicationimport/pub21xxx/pub21992/est-ret-info-col-2015-2016-site-data.csv") |> 
  janitor::row_to_names(1) |>
  janitor::clean_names() 

eric_14_15_site <- scrape_csv("https://files.digital.nhs.uk/publicationimport/pub18xxx/pub18726/est-ret-info-col-2014-2015-dat.csv") |> 
  janitor::row_to_names(1) |>
  janitor::clean_names() 

eric_13_14_site <- scrape_xls("https://files.digital.nhs.uk/CF/698629/ERIC-201314-Data.XLS",
                         "Site Data"
                         )

eric_12_13_site <- scrape_xls("https://files.digital.nhs.uk/98/1EB9FA/ERIC-2012-13-Data.XLS",
                         "Site Data"
                         )

eric_11_12_site <- scrape_xls("https://files.digital.nhs.uk/41/0BEE05/ERIC-201112-Data.XLS",
                         "Site Data"
                         )

eric_10_11_site <- scrape_xls("https://files.digital.nhs.uk/AE/D02AB9/ERIC-201011-Data.xls",
                         "Site Data"
                         )

eric_09_10_site <- scrape_xls("https://files.digital.nhs.uk/C7/22BBC6/ERIC-200910-Data.xls",
                         "Site Data"
                         )

eric_08_09_site <- scrape_xls("https://files.digital.nhs.uk/7B/DBAA4A/ERIC-200809-Data.xls",
                         "Site Data"
                         )

#### reading trust data in ####
eric_13_14_trust <- scrape_xls("https://files.digital.nhs.uk/CF/698629/ERIC-201314-Data.XLS",
                               "Trust Data"
)

eric_12_13_trust <- scrape_xls("https://files.digital.nhs.uk/98/1EB9FA/ERIC-2012-13-Data.XLS",
                               "Trust Data"
)

eric_11_12_trust <- scrape_xls("https://files.digital.nhs.uk/41/0BEE05/ERIC-201112-Data.XLS",
                               "Trust Data"
)

eric_10_11_trust <- scrape_xls("https://files.digital.nhs.uk/AE/D02AB9/ERIC-201011-Data.xls",
                               "Trust Data"
)

eric_09_10_trust <- scrape_xls("https://files.digital.nhs.uk/C7/22BBC6/ERIC-200910-Data.xls",
                               "Trust Data"
)

eric_08_09_trust <- scrape_xls("https://files.digital.nhs.uk/7B/DBAA4A/ERIC-200809-Data.xls",
                               "Trust Data"
)

#### wrangling data ####
library(dplyr)
library(stringr)

eric_data_site <- bind_rows(wrangle_eric_site(eric_08_09_site),
                            wrangle_eric_site(eric_09_10_site),
                            wrangle_eric_site(eric_10_11_site),
                            wrangle_eric_site(eric_11_12_site),
                            wrangle_eric_site(eric_12_13_site),
                            wrangle_eric_site(eric_13_14_site),
                            wrangle_eric_site(eric_14_15_site),
                            wrangle_eric_site(eric_15_16_site),
                            wrangle_eric_site(eric_16_17_site),
                            wrangle_eric_site(eric_17_18_site),
                            wrangle_eric_site(eric_18_19_site),
                            wrangle_eric_site(eric_19_20_site),
                            wrangle_eric_site(eric_20_21_site),
                            wrangle_eric_site(eric_21_22_site),
                            wrangle_eric_site(eric_22_23_site)
                            ) 

eric_data_trust <- bind_rows(wrangle_eric_trust(eric_08_09_trust),
                             wrangle_eric_trust(eric_09_10_trust),
                             wrangle_eric_trust(eric_10_11_trust),
                             wrangle_eric_trust(eric_11_12_trust),
                             wrangle_eric_trust(eric_12_13_trust),
                             wrangle_eric_trust(eric_13_14_trust)
                             )