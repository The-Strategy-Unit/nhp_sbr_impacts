
declare @period1 varchar(10);
set @period1 = '2020-04'
declare @start_date1 date;
set @start_date1 = (select [start_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period1)
declare @end_date1 date;
set @end_date1 = (select [end_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period1)

declare @period2 varchar(10);
set @period2 = '2020-05'
declare @start_date2 date;
set @start_date2 = (select [start_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period2)
declare @end_date2 date;
set @end_date2 = (select [end_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period2)

declare @period3 varchar(10);
set @period3 = '2020-06'
declare @start_date3 date;
set @start_date3 = (select [start_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period3)
declare @end_date3 date;
set @end_date3 = (select [end_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period3)

declare @period4 varchar(10);
set @period4 = '2020-07'
declare @start_date4 date;
set @start_date4 = (select [start_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period4)
declare @end_date4 date;
set @end_date4 = (select [end_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period4)

declare @period5 varchar(10);
set @period5 = '2020-08'
declare @start_date5 date;
set @start_date5 = (select [start_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period5)
declare @end_date5 date;
set @end_date5 = (select [end_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period5)

declare @period6 varchar(10);
set @period6 = '2020-09'
declare @start_date6 date;
set @start_date6 = (select [start_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period6)
declare @end_date6 date;
set @end_date6 = (select [end_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period6)

declare @period7 varchar(10);
set @period7 = '2020-10'
declare @start_date7 date;
set @start_date7 = (select [start_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period7)
declare @end_date7 date;
set @end_date7 = (select [end_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period7)

declare @period8 varchar(10);
set @period8 = '2020-11'
declare @start_date8 date;
set @start_date8 = (select [start_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period8)
declare @end_date8 date;
set @end_date8 = (select [end_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period8)

declare @period9 varchar(10);
set @period9 = '2020-12'
declare @start_date9 date;
set @start_date9 = (select [start_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period9)
declare @end_date9 date;
set @end_date9 = (select [end_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period9)

declare @period10 varchar(10);
set @period10 = '2021-01'
declare @start_date10 date;
set @start_date10 = (select [start_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period10)
declare @end_date10 date;
set @end_date10 = (select [end_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period10)

declare @period11 varchar(10);
set @period11 = '2021-02'
declare @start_date11 date;
set @start_date11 = (select [start_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period11)
declare @end_date11 date;
set @end_date11 = (select [end_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period11)

declare @period12 varchar(10);
set @period12 = '2021-03'
declare @start_date12 date;
set @start_date12 = (select [start_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period12)
declare @end_date12 date;
set @end_date12 = (select [end_date] from NHSE_Sandbox_StrategyUnit.dbo.months_dates where [period] = @period12)

----## Now execute the queries on the above variables and save to temp file for future collation

Select Der_Provider_Site_Code, @period1 as [period]
, sum( case when admission_date <= @start_date1 AND (discharge_date >= @end_date1 or discharge_date is NULL) then datediff(dd,@start_date1,@end_date1)
			when admission_date <= @start_date1 AND discharge_date between @start_date1 AND @end_date1 then datediff(dd,@start_date1,discharge_date)
			when admission_date between @start_date1 AND @end_date1 AND (discharge_date >= @end_date1 or discharge_date is NULL) then datediff(dd,admission_date,@end_date1)
			when admission_date between @start_date1 AND @end_date1 AND discharge_date between @start_date1 AND @end_date1 then datediff(dd,admission_date,discharge_date)
			else 0 end ) as beddays
into NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_2021
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-01-01' and '2023-11-30'
and left(Der_Provider_Site_Code,1) = 'R'
group by Der_Provider_Site_Code

union all

Select Der_Provider_Site_Code, @period2 as [period]
, sum( case when admission_date <= @start_date2 AND (discharge_date >= @end_date2 or discharge_date is NULL) then datediff(dd,@start_date2,@end_date2)
			when admission_date <= @start_date2 AND discharge_date between @start_date2 AND @end_date2 then datediff(dd,@start_date2,discharge_date)
			when admission_date between @start_date2 AND @end_date2 AND (discharge_date >= @end_date2 or discharge_date is NULL) then datediff(dd,admission_date,@end_date2)
			when admission_date between @start_date2 AND @end_date2 AND discharge_date between @start_date2 AND @end_date2 then datediff(dd,admission_date,discharge_date)
			else 0 end ) as beddays
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-01-01' and '2023-11-30'
and left(Der_Provider_Site_Code,1) = 'R'
group by Der_Provider_Site_Code

union all

Select Der_Provider_Site_Code, @period3 as [period]
, sum( case when admission_date <= @start_date3 AND (discharge_date >= @end_date3 or discharge_date is NULL) then datediff(dd,@start_date3,@end_date3)
			when admission_date <= @start_date3 AND discharge_date between @start_date3 AND @end_date3 then datediff(dd,@start_date3,discharge_date)
			when admission_date between @start_date3 AND @end_date3 AND (discharge_date >= @end_date3 or discharge_date is NULL) then datediff(dd,admission_date,@end_date3)
			when admission_date between @start_date3 AND @end_date3 AND discharge_date between @start_date3 AND @end_date3 then datediff(dd,admission_date,discharge_date)
			else 0 end ) as beddays
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-01-01' and '2023-11-30'
and left(Der_Provider_Site_Code,1) = 'R'
group by Der_Provider_Site_Code

union all

Select Der_Provider_Site_Code, @period4 as [period]
, sum( case when admission_date <= @start_date4 AND (discharge_date >= @end_date4 or discharge_date is NULL) then datediff(dd,@start_date4,@end_date4)
			when admission_date <= @start_date4 AND discharge_date between @start_date4 AND @end_date4 then datediff(dd,@start_date4,discharge_date)
			when admission_date between @start_date4 AND @end_date4 AND (discharge_date >= @end_date4 or discharge_date is NULL) then datediff(dd,admission_date,@end_date4)
			when admission_date between @start_date4 AND @end_date4 AND discharge_date between @start_date4 AND @end_date4 then datediff(dd,admission_date,discharge_date)
			else 0 end ) as beddays
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-01-01' and '2023-11-30'
and left(Der_Provider_Site_Code,1) = 'R'
group by Der_Provider_Site_Code

union all

Select Der_Provider_Site_Code, @period5 as [period]
, sum( case when admission_date <= @start_date5 AND (discharge_date >= @end_date5 or discharge_date is NULL) then datediff(dd,@start_date5,@end_date5)
			when admission_date <= @start_date5 AND discharge_date between @start_date5 AND @end_date5 then datediff(dd,@start_date5,discharge_date)
			when admission_date between @start_date5 AND @end_date5 AND (discharge_date >= @end_date5 or discharge_date is NULL) then datediff(dd,admission_date,@end_date5)
			when admission_date between @start_date5 AND @end_date5 AND discharge_date between @start_date5 AND @end_date5 then datediff(dd,admission_date,discharge_date)
			else 0 end ) as beddays
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-01-01' and '2023-11-30'
and left(Der_Provider_Site_Code,1) = 'R'
group by Der_Provider_Site_Code

union all

Select Der_Provider_Site_Code, @period6 as [period]
, sum( case when admission_date <= @start_date6 AND (discharge_date >= @end_date6 or discharge_date is NULL) then datediff(dd,@start_date6,@end_date6)
			when admission_date <= @start_date6 AND discharge_date between @start_date6 AND @end_date6 then datediff(dd,@start_date6,discharge_date)
			when admission_date between @start_date6 AND @end_date6 AND (discharge_date >= @end_date6 or discharge_date is NULL) then datediff(dd,admission_date,@end_date6)
			when admission_date between @start_date6 AND @end_date6 AND discharge_date between @start_date6 AND @end_date6 then datediff(dd,admission_date,discharge_date)
			else 0 end ) as beddays
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-01-01' and '2023-11-30'
and left(Der_Provider_Site_Code,1) = 'R'
group by Der_Provider_Site_Code

union all

Select Der_Provider_Site_Code, @period7 as [period]
, sum( case when admission_date <= @start_date7 AND (discharge_date >= @end_date7 or discharge_date is NULL) then datediff(dd,@start_date7,@end_date7)
			when admission_date <= @start_date7 AND discharge_date between @start_date7 AND @end_date7 then datediff(dd,@start_date7,discharge_date)
			when admission_date between @start_date7 AND @end_date7 AND (discharge_date >= @end_date7 or discharge_date is NULL) then datediff(dd,admission_date,@end_date7)
			when admission_date between @start_date7 AND @end_date7 AND discharge_date between @start_date7 AND @end_date7 then datediff(dd,admission_date,discharge_date)
			else 0 end ) as beddays
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-01-01' and '2023-11-30'
and left(Der_Provider_Site_Code,1) = 'R'
group by Der_Provider_Site_Code

union all

Select Der_Provider_Site_Code, @period8 as [period]
, sum( case when admission_date <= @start_date8 AND (discharge_date >= @end_date8 or discharge_date is NULL) then datediff(dd,@start_date8,@end_date8)
			when admission_date <= @start_date8 AND discharge_date between @start_date8 AND @end_date8 then datediff(dd,@start_date8,discharge_date)
			when admission_date between @start_date8 AND @end_date8 AND (discharge_date >= @end_date8 or discharge_date is NULL) then datediff(dd,admission_date,@end_date8)
			when admission_date between @start_date8 AND @end_date8 AND discharge_date between @start_date8 AND @end_date8 then datediff(dd,admission_date,discharge_date)
			else 0 end ) as beddays
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-01-01' and '2023-11-30'
and left(Der_Provider_Site_Code,1) = 'R'
group by Der_Provider_Site_Code

union all

Select Der_Provider_Site_Code, @period9 as [period]
, sum( case when admission_date <= @start_date9 AND (discharge_date >= @end_date9 or discharge_date is NULL) then datediff(dd,@start_date9,@end_date9)
			when admission_date <= @start_date9 AND discharge_date between @start_date9 AND @end_date9 then datediff(dd,@start_date9,discharge_date)
			when admission_date between @start_date9 AND @end_date9 AND (discharge_date >= @end_date9 or discharge_date is NULL) then datediff(dd,admission_date,@end_date9)
			when admission_date between @start_date9 AND @end_date9 AND discharge_date between @start_date9 AND @end_date9 then datediff(dd,admission_date,discharge_date)
			else 0 end ) as beddays
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-01-01' and '2023-11-30'
and left(Der_Provider_Site_Code,1) = 'R'
group by Der_Provider_Site_Code

union all

Select Der_Provider_Site_Code, @period10 as [period]
, sum( case when admission_date <= @start_date10 AND (discharge_date >= @end_date10 or discharge_date is NULL) then datediff(dd,@start_date10,@end_date10)
			when admission_date <= @start_date10 AND discharge_date between @start_date10 AND @end_date10 then datediff(dd,@start_date10,discharge_date)
			when admission_date between @start_date10 AND @end_date10 AND (discharge_date >= @end_date10 or discharge_date is NULL) then datediff(dd,admission_date,@end_date10)
			when admission_date between @start_date10 AND @end_date10 AND discharge_date between @start_date10 AND @end_date10 then datediff(dd,admission_date,discharge_date)
			else 0 end ) as beddays
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-01-01' and '2023-11-30'
and left(Der_Provider_Site_Code,1) = 'R'
group by Der_Provider_Site_Code

union all

Select Der_Provider_Site_Code, @period11 as [period]
, sum( case when admission_date <= @start_date11 AND (discharge_date >= @end_date11 or discharge_date is NULL) then datediff(dd,@start_date11,@end_date11)
			when admission_date <= @start_date11 AND discharge_date between @start_date11 AND @end_date11 then datediff(dd,@start_date11,discharge_date)
			when admission_date between @start_date11 AND @end_date11 AND (discharge_date >= @end_date11 or discharge_date is NULL) then datediff(dd,admission_date,@end_date11)
			when admission_date between @start_date11 AND @end_date11 AND discharge_date between @start_date11 AND @end_date11 then datediff(dd,admission_date,discharge_date)
			else 0 end ) as beddays
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-01-01' and '2023-11-30'
and left(Der_Provider_Site_Code,1) = 'R'
group by Der_Provider_Site_Code

union all

Select Der_Provider_Site_Code, @period12 as [period]
, sum( case when admission_date <= @start_date12 AND (discharge_date >= @end_date12 or discharge_date is NULL) then datediff(dd,@start_date12,@end_date12)
			when admission_date <= @start_date12 AND discharge_date between @start_date12 AND @end_date12 then datediff(dd,@start_date12,discharge_date)
			when admission_date between @start_date12 AND @end_date12 AND (discharge_date >= @end_date12 or discharge_date is NULL) then datediff(dd,admission_date,@end_date12)
			when admission_date between @start_date12 AND @end_date12 AND discharge_date between @start_date12 AND @end_date12 then datediff(dd,admission_date,discharge_date)
			else 0 end ) as beddays
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-01-01' and '2023-11-30'
and left(Der_Provider_Site_Code,1) = 'R'
group by Der_Provider_Site_Code

order by Der_Provider_Site_Code, [period]

----## checking the data looks OK
select *
from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_2021
where beddays > 0
order by Der_Provider_Site_Code, [period]

