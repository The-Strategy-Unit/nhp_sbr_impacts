# ERIC data

scrape_csv <- function(url) {
  download <- RCurl::getURL(url)
  
  data <- read.csv(text = download) |>
    janitor::clean_names()
  
  return(data)
}

eric_22_23 <- scrape_csv("https://files.digital.nhs.uk/41/5787C9/ERIC%20-%202022_23%20-%20Site%20data.csv")
eric_21_22 <- scrape_csv("https://files.digital.nhs.uk/EE/7E330D/ERIC%20-%20202122%20-%20Site%20Data%20v3.csv")
eric_20_21 <- scrape_csv("https://files.digital.nhs.uk/0F/46F719/ERIC%20-%20202021%20-%20Site%20data%20v2.csv")
eric_19_20 <- scrape_csv("https://files.digital.nhs.uk/11/BC1043/ERIC%20-%20201920%20-%20SiteData%20-%20v2.csv")
eric_18_19 <- scrape_csv("https://files.digital.nhs.uk/63/ADBFFF/ERIC%20-%20201819%20-%20SiteData%20v4.csv")

eric_17_18 <- scrape_csv("https://files.digital.nhs.uk/A8/188D99/ERIC-201718-SiteData.csv")
eric_16_17 <- scrape_csv("https://files.digital.nhs.uk/publication/q/3/eric-201617-site-data.csv")
eric_15_16 <- scrape_csv("https://files.digital.nhs.uk/publicationimport/pub21xxx/pub21992/est-ret-info-col-2015-2016-site-data.csv")
eric_14_15 <- scrape_csv("https://files.digital.nhs.uk/publicationimport/pub18xxx/pub18726/est-ret-info-col-2014-2015-dat.csv")


# lots different colnames




scrape_xls <- function(url, sheet) {
  download <- RCurl::getURL(url)
  
  data <- readxl::read_excel(path = download,
                             sheet = sheet
                             ) |>
    janitor::clean_names()
  
  return(data)
}


tmp = tempfile(fileext = "")
download.file(url = "https://files.digital.nhs.uk/98/1EB9FA/ERIC-2012-13-Data.XLS", destfile = tmp, mode="wb")
df<-readxl::read_excel(tmp)






download <- RCurl::getURL("https://files.digital.nhs.uk/CF/698629/ERIC-201314-xls.XLS")
readxl::read.xl(download)


eric_13_14 <- openxlsx::read.xls("https://files.digital.nhs.uk/CF/698629/ERIC-201314-xls.XLS",
                         "Site")
eric_12_13 <- scrape_xls("https://files.digital.nhs.uk/98/1EB9FA/ERIC-2012-13-Data.XLS")
eric_11_12 <- scrape_xls("https://files.digital.nhs.uk/41/0BEE05/ERIC-201112-Data.XLS")
eric_10_11 <- scrape_xls("https://files.digital.nhs.uk/AE/D02AB9/ERIC-201011-Data.xls")
eric_09_10 <- scrape_xls("https://files.digital.nhs.uk/C7/22BBC6/ERIC-200910-Data.xls")
eric_08_09 <- scrape_xls("https://files.digital.nhs.uk/7B/DBAA4A/ERIC-200809-Data.xls")

