# ERIC data

library(dplyr)

# uses functions from data_wrangling_functions.R file

eric_22_23 <- scrape_csv("https://files.digital.nhs.uk/41/5787C9/ERIC%20-%202022_23%20-%20Site%20data.csv")
eric_21_22 <- scrape_csv("https://files.digital.nhs.uk/EE/7E330D/ERIC%20-%20202122%20-%20Site%20Data%20v3.csv")
eric_20_21 <- scrape_csv("https://files.digital.nhs.uk/0F/46F719/ERIC%20-%20202021%20-%20Site%20data%20v2.csv")
eric_19_20 <- scrape_csv("https://files.digital.nhs.uk/11/BC1043/ERIC%20-%20201920%20-%20SiteData%20-%20v2.csv")
eric_18_19 <- scrape_csv("https://files.digital.nhs.uk/63/ADBFFF/ERIC%20-%20201819%20-%20SiteData%20v4.csv")

eric_17_18 <- scrape_csv("https://files.digital.nhs.uk/A8/188D99/ERIC-201718-SiteData.csv")
eric_16_17 <- scrape_csv("https://files.digital.nhs.uk/publication/q/3/eric-201617-site-data.csv")
eric_15_16 <- scrape_csv("https://files.digital.nhs.uk/publicationimport/pub21xxx/pub21992/est-ret-info-col-2015-2016-site-data.csv")
eric_14_15 <- scrape_csv("https://files.digital.nhs.uk/publicationimport/pub18xxx/pub18726/est-ret-info-col-2014-2015-dat.csv")

eric_13_14 <- scrape_xls("https://files.digital.nhs.uk/CF/698629/ERIC-201314-Data.XLS",
                         "Site Data"
                         )

eric_12_13 <- scrape_xls("https://files.digital.nhs.uk/98/1EB9FA/ERIC-2012-13-Data.XLS",
                         "Site Data"
                         )

eric_11_12 <- scrape_xls("https://files.digital.nhs.uk/41/0BEE05/ERIC-201112-Data.XLS",
                         "Site Data"
                         )

eric_10_11 <- scrape_xls("https://files.digital.nhs.uk/AE/D02AB9/ERIC-201011-Data.xls",
                         "Site Data"
                         )

eric_09_10 <- scrape_xls("https://files.digital.nhs.uk/C7/22BBC6/ERIC-200910-Data.xls",
                         "Site Data"
                         )

eric_08_09 <- scrape_xls("https://files.digital.nhs.uk/7B/DBAA4A/ERIC-200809-Data.xls",
                         "Site Data"
                         )

# lots different colnames

# 08_09 and 09_10 - needs cleaning is only at trust? diff single beds reporting
eric_09_10 <- eric_09_10 |> 
  select(trust_code = organisation_code, 
         trust_name = organisation_name,
         commissioning_region,
         site_code,
         site_name,
         site_type,
         gross_internal_floor_area_m = gross_internal_site_floor_area_m2,
         available_beds_no,
         percentage_of_single_bedrooms_for_patients_percent#,
         # cleaning_services_cost,
         #  cleaning_staff_wte
  )

# works for 10_11 to 13_14 - needs cleaning is only at trust?
eric_13_14 <- eric_13_14 |> 
  select(trust_code = organisation_code, # not 15-16
         trust_name = organisation_name,
         commissioning_region,
         site_code,
         site_name,
         site_type,
         gross_internal_floor_area_m = gross_internal_site_floor_area_m2,
         single_bedrooms_for_patients_with_en_suite_facilities_no,
         single_bedrooms_for_patients_without_en_suite_facilities_no#,
        # cleaning_service_cost,
       #  cleaning_staff_wte
  )

# for 14-15 - need to skip first row
eric_14_15 <- eric_14_15 |> 
  janitor::row_to_names(1) |>
  janitor::clean_names() |>
  select(trust_code = organisation_code, 
         trust_name = organisation_name,
         commissioning_region,
         site_code,
         site_name,
         site_type,
         gross_internal_floor_area_m = gross_internal_site_floor_area_m2,
         single_bedrooms_for_patients_with_en_suite_facilities_no,
         single_bedrooms_for_patients_without_en_suite_facilities_no,
         cleaning_service_cost,
         cleaning_staff_wte
  )

# for 15_16 - need to skip first row
eric_15_16 <- eric_15_16 |> 
  janitor::row_to_names(1) |>
  janitor::clean_names() |>
  select(trust_code = organisation_code, 
         trust_name = organisation_name,
         commissioning_region,
         site_code,
         site_name,
         site_type,
         gross_internal_floor_area_m = gross_internal_floor_area_m2,
         single_bedrooms_for_patients_with_en_suite_facilities_no,
         single_bedrooms_for_patients_without_en_suite_facilities_no,
         cleaning_service_cost,
         cleaning_staff_wte
  )


# for 16-17 to 22_23
eric_22_23 <- eric_22_23|>
  select(trust_code,
         trust_name,
         commissioning_region,
         site_code,
         site_name,
         site_type,
         gross_internal_floor_area_m,
         single_bedrooms_for_patients_with_en_suite_facilities_no,
         single_bedrooms_for_patients_without_en_suite_facilities_no,
         cleaning_service_cost,
         cleaning_staff_wte
         )


tog <- rbind(eric_22_23,
      eric_15_16,
      eric_14_15 
      )


      