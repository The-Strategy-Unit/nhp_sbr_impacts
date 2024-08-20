
--DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_falls]

--  select a.Der_Provider_Code, a.Der_Provider_Site_Code
--  , datepart(yyyy, a.admission_date) as yr, datepart(mm, a.admission_date) as mth--, datepart(wk, admission_date) as wk
--  , case when (APCS_First_Ep_Ind != 0 AND (a.Der_Diagnosis_All like '%W01%' OR
--				a.Der_Diagnosis_All like '%W03%' OR
--				a.Der_Diagnosis_All like '%W04%' OR
--				a.Der_Diagnosis_All like '%W05%' OR
--				a.Der_Diagnosis_All like '%W06%' OR
--				a.Der_Diagnosis_All like '%W07%' OR
--				a.Der_Diagnosis_All like '%W08%' OR
--				a.Der_Diagnosis_All like '%W18%' OR
--				a.Der_Diagnosis_All like '%W19%')) then 1 else 0 end as fall_after
--  , case when (APCS_First_Ep_Ind = 1 AND (a.Der_Diagnosis_All like '%W01%' OR  
--				a.Der_Diagnosis_All like '%W03%' OR
--				a.Der_Diagnosis_All like '%W04%' OR
--				a.Der_Diagnosis_All like '%W05%' OR
--				a.Der_Diagnosis_All like '%W06%' OR
--				a.Der_Diagnosis_All like '%W07%' OR
--				a.Der_Diagnosis_All like '%W08%' OR
--				a.Der_Diagnosis_All like '%W18%' OR
--				a.Der_Diagnosis_All like '%W19%')) then 1 else 0 end as [fall_first]
--  , case when (APCS_First_Ep_Ind != 1 AND (a.Der_Diagnosis_All like '%S0%' OR
--				a.Der_Diagnosis_All like '%S1%' OR
--				a.Der_Diagnosis_All like '%S2%' OR
--				a.Der_Diagnosis_All like '%S3%' OR
--				a.Der_Diagnosis_All like '%S4%' OR
--				a.Der_Diagnosis_All like '%S5%' OR
--				a.Der_Diagnosis_All like '%S6%' OR
--				a.Der_Diagnosis_All like '%S7%' OR
--				a.Der_Diagnosis_All like '%S8%' OR
--				a.Der_Diagnosis_All like '%S9%' OR
--				a.Der_Diagnosis_All like '%T0%' OR
--				a.Der_Diagnosis_All like '%T10%' OR
--				a.Der_Diagnosis_All like '%T11%' OR
--				a.Der_Diagnosis_All like '%T12%' OR
--				a.Der_Diagnosis_All like '%T13%' OR
--				a.Der_Diagnosis_All like '%T14%')) then 1 else 0 end as injure_after
--, case when (APCS_First_Ep_Ind = 1 AND (a.Der_Diagnosis_All like '%S0%' OR
--				a.Der_Diagnosis_All like '%S1%' OR
--				a.Der_Diagnosis_All like '%S2%' OR
--				a.Der_Diagnosis_All like '%S3%' OR
--				a.Der_Diagnosis_All like '%S4%' OR
--				a.Der_Diagnosis_All like '%S5%' OR
--				a.Der_Diagnosis_All like '%S6%' OR
--				a.Der_Diagnosis_All like '%S7%' OR
--				a.Der_Diagnosis_All like '%S8%' OR
--				a.Der_Diagnosis_All like '%S9%' OR
--				a.Der_Diagnosis_All like '%T0%' OR
--				a.Der_Diagnosis_All like '%T10%' OR
--				a.Der_Diagnosis_All like '%T11%' OR
--				a.Der_Diagnosis_All like '%T12%' OR
--				a.Der_Diagnosis_All like '%T13%' OR
--				a.Der_Diagnosis_All like '%T14%')) then 1 else 0 end as [injure_first]
--  , count(distinct a.apcs_ident) as spells
--  , sum(b.Der_Spell_LoS/Der_Episode_Count) as spell_los
 
-- into [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_falls]

--  FROM [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCE] a

--  left outer join NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS b
--  on a.APCS_Ident = b.APCS_Ident
--  and a.Der_Financial_Year = b.Der_Financial_Year

--  where a.Admission_Date between '2008-04-01' AND '2023-11-30' -- total study period is apr 2008 to nov 2023
--  and left(a.Der_Provider_Code,1) = 'R' -- NHS trusts only
--  and a.Discharge_Date is not NULL -- exclude patients still in hospital
--  and a.Patient_Classification = '1' --exclude day case and regular

--  group by a.Der_Provider_Code, a.Der_Provider_Site_Code
--  , datepart(yyyy, a.admission_date), datepart(mm, a.admission_date)
--  , case when (APCS_First_Ep_Ind != 0 AND (a.Der_Diagnosis_All like '%W01%' OR
--				a.Der_Diagnosis_All like '%W03%' OR
--				a.Der_Diagnosis_All like '%W04%' OR
--				a.Der_Diagnosis_All like '%W05%' OR
--				a.Der_Diagnosis_All like '%W06%' OR
--				a.Der_Diagnosis_All like '%W07%' OR
--				a.Der_Diagnosis_All like '%W08%' OR
--				a.Der_Diagnosis_All like '%W18%' OR
--				a.Der_Diagnosis_All like '%W19%')) then 1 else 0 end
--  , case when (APCS_First_Ep_Ind = 1 AND (a.Der_Diagnosis_All like '%W01%' OR  
--				a.Der_Diagnosis_All like '%W03%' OR
--				a.Der_Diagnosis_All like '%W04%' OR
--				a.Der_Diagnosis_All like '%W05%' OR
--				a.Der_Diagnosis_All like '%W06%' OR
--				a.Der_Diagnosis_All like '%W07%' OR
--				a.Der_Diagnosis_All like '%W08%' OR
--				a.Der_Diagnosis_All like '%W18%' OR
--				a.Der_Diagnosis_All like '%W19%')) then 1 else 0 end
--  , case when (APCS_First_Ep_Ind != 1 AND (a.Der_Diagnosis_All like '%S0%' OR
--				a.Der_Diagnosis_All like '%S1%' OR
--				a.Der_Diagnosis_All like '%S2%' OR
--				a.Der_Diagnosis_All like '%S3%' OR
--				a.Der_Diagnosis_All like '%S4%' OR
--				a.Der_Diagnosis_All like '%S5%' OR
--				a.Der_Diagnosis_All like '%S6%' OR
--				a.Der_Diagnosis_All like '%S7%' OR
--				a.Der_Diagnosis_All like '%S8%' OR
--				a.Der_Diagnosis_All like '%S9%' OR
--				a.Der_Diagnosis_All like '%T0%' OR
--				a.Der_Diagnosis_All like '%T10%' OR
--				a.Der_Diagnosis_All like '%T11%' OR
--				a.Der_Diagnosis_All like '%T12%' OR
--				a.Der_Diagnosis_All like '%T13%' OR
--				a.Der_Diagnosis_All like '%T14%')) then 1 else 0 end
--, case when (APCS_First_Ep_Ind = 1 AND (a.Der_Diagnosis_All like '%S0%' OR
--				a.Der_Diagnosis_All like '%S1%' OR
--				a.Der_Diagnosis_All like '%S2%' OR
--				a.Der_Diagnosis_All like '%S3%' OR
--				a.Der_Diagnosis_All like '%S4%' OR
--				a.Der_Diagnosis_All like '%S5%' OR
--				a.Der_Diagnosis_All like '%S6%' OR
--				a.Der_Diagnosis_All like '%S7%' OR
--				a.Der_Diagnosis_All like '%S8%' OR
--				a.Der_Diagnosis_All like '%S9%' OR
--				a.Der_Diagnosis_All like '%T0%' OR
--				a.Der_Diagnosis_All like '%T10%' OR
--				a.Der_Diagnosis_All like '%T11%' OR
--				a.Der_Diagnosis_All like '%T12%' OR
--				a.Der_Diagnosis_All like '%T13%' OR
--				a.Der_Diagnosis_All like '%T14%')) then 1 else 0 end

----## summary counts by prov by month - for R
Select *
, case when mth < 10 then cast(yr as varchar) + '-0' + cast(mth as varchar) else cast(yr as varchar) + '-' + cast(mth as varchar) end as yr_mth
, case when (fall_first = 0 AND injure_first = 0) AND (fall_after = 1 OR injure_after = 1) then 1 else 0 end as [fall_inj_event]
into #1
from [NHSE_Sandbox_StrategyUnit].dbo.[nhp_sbr_falls]
where left(Der_Provider_Site_Code,1) = 'R'
order by Der_Provider_Site_Code, yr, mth

Select der_provider_code, Der_Provider_Site_Code, yr_mth
, sum(spells) as all_spells
, sum(case when fall_inj_event = 1 then spells else 0 end) as fall_spells
, sum(spell_los) as all_spell_los
, sum(case when fall_inj_event = 1 then spell_los else 0 end) as fall_spell_los
from #1
group by der_provider_code, Der_Provider_Site_Code, yr_mth
order by der_provider_code, Der_Provider_Site_Code, yr_mth