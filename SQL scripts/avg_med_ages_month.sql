DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_ages]

----## record level query
select Der_Provider_Site_Code, apcs_ident, Age_At_Start_of_Spell_SUS
, cast(datepart(yyyy, [Admission_Date]) as varchar) + '-' + (case when datepart(mm, [Admission_Date]) < 10 then '0'+ cast(datepart(mm, [Admission_Date]) as varchar) else cast(datepart(mm, [Admission_Date]) as varchar) end) as yr_mth

into #1

from NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS
where Admission_Date between '2008-04-01' and '2025-03-31'
and Age_At_Start_of_Spell_SUS between 0 and 115

----## grouped provider and period with count and mean age
select Der_Provider_Site_Code, yr_mth
, count(distinct apcs_ident) as spells
, round(avg(cast(Age_At_Start_of_Spell_SUS as float)),2) as age_avg
into #2
FROM  #1
group by Der_Provider_Site_Code, yr_mth
order by Der_Provider_Site_Code, yr_mth

----## grouped provider and period with median age
select Der_Provider_Site_Code, yr_mth
, PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY Age_At_Start_of_Spell_SUS) OVER(PARTITION BY Der_Provider_Site_Code, yr_mth) as age_med
into #3
FROM  #1

select Der_Provider_Site_Code, yr_mth, age_med
into #4
from #3
group by Der_Provider_Site_Code, yr_mth, age_med

----## join to one table
Select a.*, b.age_med
into [NHSE_Sandbox_StrategyUnit].dbo.nhp_sbr_ages
from #2 a
left outer join #4 b
on a.Der_Provider_Site_Code = b.Der_Provider_Site_Code
and a.yr_mth = b.yr_mth

----## clean up environment/session
drop table #1
drop table #2
drop table #3
drop table #4

----## table for extract to file
Select * from [NHSE_Sandbox_StrategyUnit].dbo.nhp_sbr_ages
where (left(Der_Provider_Site_Code,1) = 'R' OR Der_Provider_Site_Code = 'I3W1A')
order by der_provider_site_code, yr_mth
