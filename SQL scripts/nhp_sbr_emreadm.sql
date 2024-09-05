
DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_emreadm]

USE NHSE_SUSPlus_Live
GO

Select a.Der_Pseudo_NHS_Number, APCS_Ident, Admission_Date, discharge_date, Der_Provider_Site_Code
, cast(datepart(yyyy, discharge_date) as varchar) + '-' + (case when datepart(mm, discharge_date) < 10 then '0'+ cast(datepart(mm, discharge_date) as varchar) else cast(datepart(mm, discharge_date) as varchar) end) as yr_mth
, b.APCS_Ident2, b.Admission_Date2, b.Discharge_Date2
, case when b.Der_Pseudo_NHS_Number is not NULL then 1 else 0 end as em_readm_30_day

into [NHSE_Sandbox_StrategyUnit].dbo.nhp_sbr_emreadm
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS a
left outer join (Select Der_Pseudo_NHS_Number, APCS_Ident as APCS_Ident2, Admission_Date as Admission_Date2, Discharge_Date as Discharge_Date2
			from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
			where Admission_Date between '2008-04-01' AND '2023-12-31'
			and Patient_Classification = '1'
			--and Der_Admit_Treatment_Function_Code not in ('656','700','710','711','712','713','715','720','721','722','723','724','725','726','727','730')
			and left(Admission_Method,1) = '2') b
			on a.Der_Pseudo_NHS_Number = b.Der_Pseudo_NHS_Number
			and datediff(dd,a.discharge_date, b.Admission_Date2) between 0 and 29

where a.discharge_date between '2008-04-01' AND '2023-11-30'
and Discharge_Method in ('1','3')
and Patient_Classification = '1'
--and Der_Admit_Treatment_Function_Code not in ('656','700','710','711','712','713','715','720','721','722','723','724','725','726','727','730')


----## Now aggregating and making binary indicator for readmission
Select Der_Provider_Site_Code, yr_mth, APCS_Ident
into #1
from [NHSE_Sandbox_StrategyUnit].dbo.nhp_sbr_emreadm
group by Der_Provider_Site_Code, yr_mth, APCS_Ident

Select Der_Provider_Site_Code, yr_mth, APCS_Ident
, sum(case when apcs_ident2 is NULL then 0 else 1 end) as readmits
into #2
from [NHSE_Sandbox_StrategyUnit].dbo.nhp_sbr_emreadm
group by Der_Provider_Site_Code, yr_mth, APCS_Ident

select a.*
, case when b.readmits > 0 then 1 else 0 end as readmits
into #3
from #1 a
left outer join #2 b
on a.Der_Provider_Site_Code = b.Der_Provider_Site_Code
and a.yr_mth = b.yr_mth
and a.apcs_ident = b.apcs_ident

select Der_Provider_Site_Code, yr_mth
, count(distinct apcs_ident) as admits
, sum(readmits) as readmits
into [NHSE_Sandbox_StrategyUnit].dbo.nhp_sbr_emreadm_agg
from #3
group by Der_Provider_Site_Code, yr_mth
order by Der_Provider_Site_Code, yr_mth

----## clean up
drop table #1
drop table #2
drop table #3


----## full extract to take to R
select *
from [NHSE_Sandbox_StrategyUnit].dbo.nhp_sbr_emreadm_agg
where left(Der_Provider_Site_Code,1) = 'R'
order by Der_Provider_Site_Code, yr_mth

----## testing the (national) time series for scale and consistency
select yr_mth, sum(admits) as admits, sum(readmits) as readmits
, sum(cast(readmits as float))/sum(cast(admits as float)) *1.0
from [NHSE_Sandbox_StrategyUnit].dbo.nhp_sbr_emreadm_agg
where left(Der_Provider_Site_Code,1) = 'R'
group by yr_mth
order by yr_mth
