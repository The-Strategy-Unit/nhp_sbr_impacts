DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_deaths]

Select Der_Provider_Code, Der_Provider_Site_Code
, cast(datepart(yyyy, Discharge_Date) as varchar) + '-' + (case when datepart(mm, Discharge_Date) < 10 then '0'+ cast(datepart(mm, Discharge_Date) as varchar) else cast(datepart(mm, Discharge_Date) as varchar) end) as yr_mth
, count(distinct apcs_ident) as discharges
, sum(case when Discharge_Method = '4' OR
				(b.POD_NHS_ESTABLISHMENT = '1' and b.POD_ESTABLISHMENT_TYPE in ('1','3','01','03','18','99')) OR
				(b.POD_NHS_ESTABLISHMENT = '2' and b.POD_ESTABLISHMENT_TYPE in ('1','07','18','19')) then 1 else 0 end) as death_hosp
, sum(case when b.DER_PSEUDO_NHS_NUMBER is not NULL and b.POD_ESTABLISHMENT_TYPE not in ('1','3','01','03','7','07','18','19','99') then 1 else 0 end) as death_30days
into [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_deaths]
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS a
left outer join [NHSE_Mortality].[mort].[Mortality] b

on a.Der_Pseudo_NHS_Number = b.Der_Pseudo_NHS_Number
and datediff(dd,a.discharge_date, b.REG_DATE_OF_DEATH) between 0 and 29
where Patient_Classification = '1' ---ordinary admissions only
and Discharge_Method != '5' ---exclude stillbirths
and Discharge_Date between '2008-04-01' AND '2023-11-30' ---full study period apr 08 to nov 23
and Discharge_Date is not NULL ---completed spells only
and left(der_provider_code, 1) = 'R' ---NHS trusts only

group by Der_Provider_Code, Der_Provider_Site_Code
, cast(datepart(yyyy, Discharge_Date) as varchar) + '-' + (case when datepart(mm, Discharge_Date) < 10 then '0'+ cast(datepart(mm, Discharge_Date) as varchar) else cast(datepart(mm, Discharge_Date) as varchar) end)

----## for extract to R
Select *
from [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_deaths]
order by Der_Provider_Code, Der_Provider_Site_Code, yr_mth