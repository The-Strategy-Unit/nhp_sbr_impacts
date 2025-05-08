DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_bedmix]

Select Der_Provider_Code, Der_Provider_Site_Code
, Der_Financial_Year
, sum(case when left(admission_method,1) = '1' then Der_Spell_LoS else 0 end) as elec_los
, sum(case when left(admission_method,1) = '2' then Der_Spell_LoS else 0 end) as emrg_los
into #1
from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Patient_Classification = '1' ---ordinary admissions only
and left(Admission_Method,1) in ('1','2') ---elective and emergency only
and Admission_Date between '2008-04-01' AND '2025-03-31' ---full study period
and Discharge_Date is not NULL ---completed spells only
and (left(Der_Provider_Site_Code,1) = 'R' OR Der_Provider_Site_Code = 'I3W1A') ---NHS trusts only + Midlands Met

group by Der_Provider_Code, Der_Provider_Site_Code
, Der_Financial_Year

select *
, case when emrg_los = 0 then round(elec_los / cast(1 as float) * 1.0,2) else round(elec_los / cast(emrg_los as float) * 1.0,2) end as elec_emrg_ratio
into [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_bedmix]
from #1

--drop table #1

----## for extract to R
Select *
from [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_bedmix]
order by Der_Provider_Site_Code, Der_Financial_Year