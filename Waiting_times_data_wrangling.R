library(RCurl)
library(dplyr)
library(tidyverse)
library(parsedate)
library(janitor)
library(readxl)


# Pulling and wrangling Waiting times data 

#Function
read_rtt<-function(url, name){
  
  tmp = tempfile(fileext = "")
  
  download.file(url = url, destfile = tmp, mode="wb")
  df<-read_excel(tmp, sheet="Provider",range = cell_limits(c(14, 2), c(NA, NA)) )|>
    clean_names()|>
    select(c(1:5,total_number_of_incomplete_pathways, average_median_waiting_time_in_weeks))|>
    mutate(date=name)
 
  assign(name, df, envir=.GlobalEnv)
  
}

#2023/2024
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/12/Incomplete-Provider-Oct23-XLSX-8664K-39245.xlsx", "Oct23")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/Incomplete-Provider-Sep23-XLSX-8652K-revised.xlsx", "Sept23")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/Incomplete-Provider-Aug23-XLSX-8635K-revised.xlsx", "Aug23")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/Incomplete-Provider-Jul23-XLSX-8577K-revisions.xlsx", "Jul23")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Incomplete-Provider-Jun23-XLSX-8564K-64970.xlsx", "June23")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/Incomplete-Provider-May23-XLSX-8587K-revised.xlsx", "May23")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/06/Incomplete-Provider-Apr23-XLS-18344K-36960.xls", "Apr23")

rtt2023_24<-rbind(Apr23, May23, June23, Jul23, Aug23, Sept23, Oct23)

rm(Apr23, May23, June23, Jul23, Aug23, Sept23, Oct23)


#2022/2023
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Incomplete-Provider-Mar23-revised-XLSX-8540K.xlsx", "Mar23")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Incomplete-Provider-Feb23-revised-XLSX-8539K.xlsx", "Feb23")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Incomplete-Provider-Jan23-revised-XLSX-8526K.xlsx", "Jan23")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Incomplete-Provider-Dec22-revised-XLSX-8487K.xlsx", "Dec22")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Incomplete-Provider-Nov22-revised-XLSX-8442K.xlsx", "Nov22")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/08/Incomplete-Provider-Oct22-revised-XLSX-8471K.xlsx", "Oct22")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Incomplete-Provider-Sep22-revised-XLS-17733K.xls", "Sept22")
 
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Incomplete-Provider-Aug22-revised-XLS-17734K.xls", "Aug22")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Incomplete-Provider-Jul22-revised-XLS-17809K.xls", "Jul22")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Incomplete-Provider-Jun22-revised-XLS-17436K.xls", "June22")
 
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Incomplete-Provider-May22-revised-XLS-17211K.xls", "May22")
 
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Incomplete-Provider-Apr22-revised-XLS-17506K.xls", "Apr22")

rtt2022_23<-rbind(Apr22, May22, June22, Jul22, Aug22, Sept22, Oct22, Nov22, Dec22, Jan23, Feb23, Mar23)

rm(Apr22, May22, June22, Jul22, Aug22, Sept22, Oct22, Nov22, Dec22, Jan23, Feb23, Mar23)

#2021/2022
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Incomplete-Provider-Mar22-revised-XLS-17116K.xls", "Mar22")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Incomplete-Provider-Feb22-revised-XLS-17115K.xls", "Feb22")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Incomplete-Provider-Jan22-revised-XLS-17155K.xls", "Jan22")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Incomplete-Provider-Dec21-revised-XLS-17055K.xls", "Dec21")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Incomplete-Provider-Nov21-revised-XLS-17094K.xls", "Nov21")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/03/Incomplete-Provider-Oct21-revised-XLS-16977K.xls", "Oct21")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Incomplete-Provider-Sep21-revised-XLS-16928K.xls", "Sept21")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Incomplete-Provider-Aug21-revised-XLS-16854K.xls", "Aug21")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Incomplete-Provider-Jul21-revised-XLS-16928K.xls", "Jul21")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Incomplete-Provider-Jun21-revised-v2-XLS-16812K.xls", "June21")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Incomplete-Provider-May21-revised-XLS-16772K.xls", "May21")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Incomplete-Provider-Apr21-revised-XLS-16850K.xls", "Apr21")

rtt2021_22<-rbind(Apr21, May21, June21, Jul21, Aug21, Sept21, Oct21, Nov21, Dec21, Jan22, Feb22, Mar22)

rm(Apr21, May21, June21, Jul21, Aug21, Sept21, Oct21, Nov21, Dec21, Jan22, Feb22, Mar22)

#2020/2021
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Incomplete-Provider-Mar21-XLS-8420K-76325.xls", "Mar21")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/Incomplete-Provider-Feb21-XLS-8343K-25692.xls", "Feb21")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/Incomplete-Provider-Jan21-XLS-8402K-v2.xls", "Jan21")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/Incomplete-Provider-Dec20-XLS-8346K-v2.xls", "Dec20")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/Incomplete-Provider-Nov20-XLS-8287K-26885.xls", "Nov20")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/Incomplete-Provider-Oct20-revised-XLS-8266K.xls", "Oct20")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Incomplete-Provider-Sep20-XLS-8295K-v2.xls", "Sept20")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/Incomplete-Provider-Aug20-XLS-8230K-v2.xls", "Aug20")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/09/Incomplete-Provider-Jul20-XLS-8164K-59000.xls", "Jul20")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/Incomplete-Provider-Jun20-revised-XLS-8101K.xls", "June20")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/Incomplete-Provider-May20-revised-XLS-8140K.xls", "May20")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/Incomplete-Provider-Apr20-revised-XLS-8171K.xls", "Apr20")

rtt2020_21<-rbind(Apr20, May20, June20, Jul20, Aug20, Sept20, Oct20, Nov20, Dec20, Jan21, Feb21, Mar21)

rm(Apr20, May20, June20, Jul20, Aug20, Sept20, Oct20, Nov20, Dec20, Jan21, Feb21, Mar21)

#2019/2020
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/05/Incomplete-Provider-Mar20-XLS-8346K-73640.xls", "Mar20")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Incomplete-Provider-Feb20-revised-XLS-8402K.xls", "Feb20")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Incomplete-Provider-Jan20-revised-XLS-8401K.xls", "Jan20")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Incomplete-Provider-Dec19-revised-XLS-8402K.xls", "Dec19")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Incomplete-Provider-Nov19-revised-XLS-8390K.xls", "Nov19")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Incomplete-Provider-Oct19-revised-XLS-8410K.xls", "Oct19")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/11/Incomplete-Provider-Sep19-XLS-8357K-62303.xls", "Sept19")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Incomplete-Provider-Aug19-XLS-8341K-revised.xls", "Aug19")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Incomplete-Provider-Jul19-XLS-8321K-revised.xls", "Jul19")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Incomplete-Provider-Jun19-XLS-8359K-revised.xls", "June19")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Incomplete-Provider-May19-XLS-8362K-revised.xls", "May19")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Incomplete-Provider-Apr19-revised-XLS-8362K.xls", "Apr19")

rtt2019_20<-rbind(Apr19, May19, June19, Jul19, Aug19, Sept19, Oct19, Nov19, Dec19, Jan20, Feb20, Mar20)

rm(Apr19, May19, June19, Jul19, Aug19, Sept19, Oct19, Nov19, Dec19, Jan20, Feb20, Mar20)

#2018/2019
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/07/Incomplete-Provider-Mar19-revised-XLS-8332K.xls", "Mar19")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/07/Incomplete-Provider-Feb19-revised-XLS-8293K.xls", "Feb19")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/07/Incomplete-Provider-Jan19-revised-XLS-8308K.xls", "Jan19")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/02/Incomplete-Provider-Dec18-XLS-8223K-89381.xls", "Dec18")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/07/Incomplete-Provider-Nov18-revised-XLS-8215K.xls", "Nov18")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/07/Incomplete-Provider-Oct18-revised-XLS-8197K.xls", "Oct18")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Incomplete-Provider-Sep18-revised-XLS-8138K.xls", "Sept18")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Incomplete-Provider-Aug18-revised-XLS-8153K.xls", "Aug18")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Incomplete-Provider-Jul18-revised-XLS-8157K.xls", "Jul18")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Incomplete-Provider-Jun18-revised-XLS-8177K.xls", "June18")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Incomplete-Provider-May18-revised-XLS-8178K.xls", "May18")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Incomplete-Provider-Apr18-revised-XLS-8187K.xls", "Apr18")

rtt2018_19<-rbind(Apr18, May18, June18, Jul18, Aug18, Sept18, Oct18, Nov18, Dec18, Jan19, Feb19, Mar19)

rm(Apr18, May18, June18, Jul18, Aug18, Sept18, Oct18, Nov18, Dec18, Jan19, Feb19, Mar19)

#2017/2018
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Incomplete-Provider-Mar18-revised-XLS-8007K.xls", "Mar18")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Incomplete-Provider-Feb18-revised-XLS-8063K.xls", "Feb18")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Incomplete-Provider-Jan18-revised-XLS-8061K.xls", "Jan18")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Incomplete-Provider-Dec17-revised-XLS-8042K.xls", "Dec17")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Incomplete-Provider-Nov17-revised-XLS-7791K.xls", "Nov17")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Incomplete-Provider-Oct17-revised-XLS-7811K.xls", "Oct17")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2017/06/Incomplete-Provider-Sep17-XLS-7757K-98673.xls", "Sept17")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/01/Incomplete-Provider-Aug17-revised-XLS-7778K.xls", "Aug17")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/01/Incomplete-Provider-Jul17-revised-XLS-7853K.xls", "Jul17")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/01/Incomplete-Provider-Jun17-revised-XLS-7814K.xls", "June17")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/01/Incomplete-Provider-May17-revised-XLS-7872K.xls", "May17")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/01/Incomplete-Provider-Apr17-revised-XLS-7814K.xls", "Apr17")

rtt2017_18<-rbind(Apr17, May17, June17, Jul17, Aug17, Sept17, Oct17, Nov17, Dec17, Jan18, Feb18, Mar18)

rm(Apr17, May17, June17, Jul17, Aug17, Sept17, Oct17, Nov17, Dec17, Jan18, Feb18, Mar18)

#2016/2017
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/Incomplete-Provider-Mar17-revised-v2-XLS-4181K-resaved.xlsx", "Mar17")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/Incomplete-Provider-Feb17-revised-v2-XLS-4171K-resaved.xlsx", "Feb17")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/Incomplete-Provider-Jan17-revised-v2-XLS-4165K-resaved.xlsx", "Jan17")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/Incomplete-Provider-Dec16-revised-v2-XLS-4145K-resaved.xlsx", "Dec16")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/Incomplete-Provider-Nov16-revised-v2-XLS-4153K-resaved.xlsx", "Nov16")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/Incomplete-Provider-Oct16-revised-v2-XLS-4159K-resaved.xlsx", "Oct16")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/Incomplete-Provider-Sep16-revised-v2-XLS-4117K-resaved.xlsx", "Sept16")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/Incomplete-Provider-Aug16-revised-v2-XLS-4096K-resaved.xlsx", "Aug16")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/Incomplete-Provider-Jul16-revised-v2-XLS-4099K-resaved.xlsx", "Jul16")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/Incomplete-Provider-Jun16-revised-v2-XLS-4077K-resaved.xlsx", "June16")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/Incomplete-Provider-May16-revised-XLS-7669K.xls", "May16")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/Incomplete-Provider-Apr16-revised-XLS-7651K.xls", "Apr16")

rtt2016_17<-rbind(Apr16, May16, June16, Jul16, Aug16, Sept16, Oct16, Nov16, Dec16, Jan17, Feb17, Mar17)

rm(Apr16, May16, June16, Jul16, Aug16, Sept16, Oct16, Nov16, Dec16, Jan17, Feb17, Mar17)

#2015/2016
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/06/Incomplete-Provider-Mar16-revised-XLS-3709K.xls", "Mar16")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/06/Incomplete-Provider-Feb16-revised-XLS-3690K.xls", "Feb16")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/06/Incomplete-Provider-Jan16-revised-XLS-3680K.xls", "Jan16")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/06/Incomplete-Provider-Dec15-revised-XLS-3690K.xls", "Dec15")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/06/Incomplete-Provider-Nov15-revised-XLS-3700K.xls", "Nov15")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/06/Incomplete-Provider-Oct15-revised-XLS-3709K.xls", "Oct15")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/06/Incomplete-Provider-Sep15-revised-v2-XLS-3620K.xls", "Sept15")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/06/Incomplete-Provider-Aug15-revised-XLS-3692K.xls", "Aug15")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/06/Incomplete-Provider-Jul15-revised-XLS-3692K.xls", "Jul15")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/06/Incomplete-Provider-Jun15-revised-XLS-3709K.xls", "June15")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/06/Incomplete-Provider-May15-revised-XLS-3699K.xls", "May15")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/06/Incomplete-Provider-Apr15-revised-XLS-3690K.xls", "Apr15")

rtt2015_16<-rbind(Apr15, May15, June15, Jul15, Aug15, Sept15, Oct15, Nov15, Dec15, Jan16, Feb16, Mar16)

rm(Apr15, May15, June15, Jul15, Aug15, Sept15, Oct15, Nov15, Dec15, Jan16, Feb16, Mar16)

#2014/2015
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/06/Incomplete-Provider-Mar15-revised-XLS-3691K.xls", "Mar15")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/06/Incomplete-Provider-Feb15-revised-XLS-3681K.xls", "Feb15")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/06/Incomplete-Provider-Jan15-revised-XLS-3708K.xls", "Jan15")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/06/Incomplete-Provider-Dec14-revised-XLS-3690K.xls", "Dec14")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/06/Incomplete-Provider-Nov14-revised-XLS-3690K.xls", "Nov14")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/06/Incomplete-Provider-Oct14-XLS-3690K-03680.xls", "Oct14")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/06/Incomplete-Provider-Sep14-revised-XLS-3746K.xls", "Sept14")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/06/Incomplete-Provider-Aug14-revised-XLS-3719K.xls", "Aug14")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/06/Incomplete-Provider-Jul14-revised-2-XLS-3728K.xls", "Jul14")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/06/Incomplete-Provider-Jun14-revised-XLS-3746K.xls", "June14")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/06/Incomplete-Provider-May14-revised-XLS-3756K.xls", "May14")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/06/Incomplete-Provider-Apr14-revised-2-XLS-3737K.xls", "Apr14")

rtt2014_15<-rbind(Apr14, May14, June14, Jul14, Aug14, Sept14, Oct14, Nov14, Dec14, Jan15, Feb15, Mar15)

rm(Apr14, May14, June14, Jul14, Aug14, Sept14, Oct14, Nov14, Dec14, Jan15, Feb15, Mar15)

#2013/2014
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Mar14-XLS-3717K-73803.xls", "Mar14")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Feb14-revised-v2-XLS-3718K.xls", "Feb14")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Jan14-revised-v2-XLS-3708K.xls", "Jan14")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Dec13-revised-v2-XLS-3727K1.xls", "Dec13")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Nov13-revised-v2-XLS-3699K.xls", "Nov13")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Oct13-revised-XLS-3699K.xls", "Oct13")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Sep13-revised-XLS-3736K.xls", "Sept13")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Aug13-revised-XLS-3746K.xls", "Aug13")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Jul13-revised-XLS-3764K.xls", "Jul13")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Jun13-revised-XLS-3745K.xls", "June13")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-May13-revised-XLS-3744K.xls", "May13")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Apr13-revised-2-XLS-3754K.xls", "Apr13")

rtt2013_14<-rbind(Apr13, May13, June13, Jul13, Aug13, Sept13, Oct13, Nov13, Dec13, Jan14, Feb14, Mar14)

rm(Apr13, May13, June13, Jul13, Aug13, Sept13, Oct13, Nov13, Dec13, Jan14, Feb14, Mar14)

#2012/2013
read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Mar14-XLS-3717K-73803.xls", "Mar13")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Feb14-revised-v2-XLS-3718K.xls", "Feb13")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Jan14-revised-v2-XLS-3708K.xls", "Jan13")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Dec13-revised-v2-XLS-3727K1.xls", "Dec12")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Nov13-revised-v2-XLS-3699K.xls", "Nov12")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Oct13-revised-XLS-3699K.xls", "Oct12")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Sep13-revised-XLS-3736K.xls", "Sept12")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Aug13-revised-XLS-3746K.xls", "Aug12")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Jul13-revised-XLS-3764K.xls", "Jul12")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Jun13-revised-XLS-3745K.xls", "June12")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-May13-revised-XLS-3744K.xls", "May12")

read_rtt("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/06/Incomplete-Provider-Apr13-revised-2-XLS-3754K.xls", "Apr12")

rtt2012_13<-rbind(Apr12, May12, June12, Jul12, Aug12, Sept12, Oct12, Nov12, Dec12, Jan13, Feb13, Mar13)

rm(Apr12, May12, June12, Jul12, Aug12, Sept12, Oct12, Nov12, Dec12, Jan13, Feb13, Mar13)



#names(Attendance21)<-names(Attendance22)
#names(Attendance20)<-names(Attendance22)






