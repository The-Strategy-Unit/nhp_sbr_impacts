DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_los]

Select Der_Provider_Code, Der_Provider_Site_Code
, cast(datepart(yyyy, [Admission_Date]) as varchar) + '-' + (case when datepart(mm, [Admission_Date]) < 10 then '0'+ cast(datepart(mm, [Admission_Date]) as varchar) else cast(datepart(mm, [Admission_Date]) as varchar) end) as yr_mth
, count(distinct apcs_ident) as spells
, sum(Der_Spell_LoS) as total_los
, sum(cast(Der_Spell_LoS as float)) / count(distinct apcs_ident) *1.0 as avg_los
into [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_los]
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Patient_Classification = '1' ---ordinary admissions only
and Discharge_Method != '4' ---exclude spells ending in death
and Admission_Date between '2008-04-01' AND '2023-11-30' ---full study period
and Discharge_Date is not NULL ---completed spells only
and left(der_provider_code, 1) = 'R' ---NHS trusts only

group by Der_Provider_Code, Der_Provider_Site_Code
, cast(datepart(yyyy, [Admission_Date]) as varchar) + '-' + (case when datepart(mm, [Admission_Date]) < 10 then '0'+ cast(datepart(mm, [Admission_Date]) as varchar) else cast(datepart(mm, [Admission_Date]) as varchar) end)

----## for extract to R
Select *
from [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_los]
order by Der_Provider_Code, Der_Provider_Site_Code, yr_mth