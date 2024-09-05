
DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].dbo.[sbr_apce_temp]

  select a.Der_Provider_Code, a.Der_Provider_Site_Code
  , f.Main_Specialty_Group ----currently used as high level grouping of main specialty (too many rows!)
  , case when left(a.Admission_Method,1) = '1' then 'elective'
		when left(a.Admission_Method,1) = '2' then 'non_elective'
		else 'other' end as adm_meth_desc
  , datepart(yyyy, a.admission_date) as yr, datepart(mm, a.admission_date) as mth--, datepart(wk, admission_date) as wk
  , case when (APCS_First_Ep_Ind != 0 AND (a.Der_Diagnosis_All like '%W01%' OR
				a.Der_Diagnosis_All like '%W03%' OR
				a.Der_Diagnosis_All like '%W04%' OR
				a.Der_Diagnosis_All like '%W05%' OR
				a.Der_Diagnosis_All like '%W06%' OR
				a.Der_Diagnosis_All like '%W07%' OR
				a.Der_Diagnosis_All like '%W08%' OR
				a.Der_Diagnosis_All like '%W18%' OR
				a.Der_Diagnosis_All like '%W19%')) then 1 else 0 end as fall_after
  , case when (APCS_First_Ep_Ind = 1 AND (a.Der_Diagnosis_All like '%W01%' OR  
				a.Der_Diagnosis_All like '%W03%' OR
				a.Der_Diagnosis_All like '%W04%' OR
				a.Der_Diagnosis_All like '%W05%' OR
				a.Der_Diagnosis_All like '%W06%' OR
				a.Der_Diagnosis_All like '%W07%' OR
				a.Der_Diagnosis_All like '%W08%' OR
				a.Der_Diagnosis_All like '%W18%' OR
				a.Der_Diagnosis_All like '%W19%')) then 1 else 0 end as [fall_first]
  , case when (APCS_First_Ep_Ind != 1 AND (a.Der_Diagnosis_All like '%S02%' OR
				a.Der_Diagnosis_All like '%S12%' OR
				a.Der_Diagnosis_All like '%S22%' OR
				a.Der_Diagnosis_All like '%S32%' OR
				a.Der_Diagnosis_All like '%S42%' OR
				a.Der_Diagnosis_All like '%S52%' OR
				a.Der_Diagnosis_All like '%S62%' OR
				a.Der_Diagnosis_All like '%S72%' OR
				a.Der_Diagnosis_All like '%S82%' OR
				a.Der_Diagnosis_All like '%S92%' OR
				a.Der_Diagnosis_All like '%T02%' OR
				a.Der_Diagnosis_All like '%T08%')) then 1 else 0 end as fracture_after
, case when (APCS_First_Ep_Ind = 1 AND (a.Der_Diagnosis_All like '%S02%' OR
				a.Der_Diagnosis_All like '%S12%' OR
				a.Der_Diagnosis_All like '%S22%' OR
				a.Der_Diagnosis_All like '%S32%' OR
				a.Der_Diagnosis_All like '%S42%' OR
				a.Der_Diagnosis_All like '%S52%' OR
				a.Der_Diagnosis_All like '%S62%' OR
				a.Der_Diagnosis_All like '%S72%' OR
				a.Der_Diagnosis_All like '%S82%' OR
				a.Der_Diagnosis_All like '%S92%' OR
				a.Der_Diagnosis_All like '%T02%' OR
				a.Der_Diagnosis_All like '%T08%')) then 1 else 0 end as [fracture_first]
   , case when a.Discharge_Method = '5' OR a.Discharge_Destination = '79' then 1 else 0 end as [death_spell]
  , case when APCS_First_Ep_Ind = 1 and a.Der_Financial_Year >= '2015/16' and (c.Grand_Total_Payment_MFF is NULL or c.Grand_Total_Payment_MFF = 0) then 1
			when APCS_First_Ep_Ind = 1 and a.Der_Financial_Year = '2014/15' and (d.Grand_Total_Payment is NULL or d.Grand_Total_Payment = 0) then 1
			when APCS_First_Ep_Ind = 1 and a.Der_Financial_Year = '2013/14' and (e.Grand_Total_Payment is NULL or e.Grand_Total_Payment = 0) then 1
			else 0 end as not_costed
  , count(distinct a.apcs_ident) as spells
  , sum(case when APCS_First_Ep_Ind = 1 then b.Der_Spell_LoS else 0 end) as spell_los
  
  , sum(case when APCS_First_Ep_Ind = 1 and a.Der_Financial_Year >= '2015/16' then c.Grand_Total_Payment_MFF
			when APCS_First_Ep_Ind = 1 and a.Der_Financial_Year = '2014/15' then d.Grand_Total_Payment
			when APCS_First_Ep_Ind = 1 and a.Der_Financial_Year = '2013/14' then e.Grand_Total_Payment
			else 0 end) as spell_cost
 
 into [NHSE_Sandbox_StrategyUnit].dbo.[sbr_apce_temp]

  FROM [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCE] a

  left outer join NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS b
  on a.APCS_Ident = b.APCS_Ident
  and a.Der_Financial_Year = b.Der_Financial_Year

  left outer join NHSE_SUSPlus_Live.dbo.tbl_Data_SEM_APCS_2324_Cost c
  on a.APCS_Ident = c.APCS_Ident
  and a.Der_Financial_Year = c.Der_Financial_Year

  left outer join NHSE_SUSPlus_Live.[dbo].[tbl_zArchive_Data_SEM_APCS_1415_Cost] d
  on a.APCS_Ident = d.APCS_Ident
  and a.Der_Financial_Year = d.Der_Financial_Year

  left outer join NHSE_SUSPlus_Live.[dbo].[tbl_zArchive_Data_SEM_APCS_1314_Cost] e
  on a.APCS_Ident = e.APCS_Ident
  and a.Der_Financial_Year = e.Der_Financial_Year

  left outer join NHSE_Reference.[dbo].[tbl_Ref_DataDic_ZZZ_MainSpecialty] f
  on a.Main_Specialty_Code = f.Main_Specialty_Code

  where a.Admission_Date between '2008-04-01' AND '2023-11-30' -- total study period is apr 2008 to nov 2023
  and left(a.Der_Provider_Code,1) = 'R' -- NHS trusts only
  and a.Discharge_Date is not NULL -- exclude patients still in hospital
  and a.Patient_Classification = '1' --exclude day case and regular

  group by a.Der_Provider_Code, a.Der_Provider_Site_Code
  , f.Main_Specialty_Group
  , case when left(a.Admission_Method,1) = '1' then 'elective'
		when left(a.Admission_Method,1) = '2' then 'non_elective'
		else 'other' end
  , datepart(yyyy, a.admission_date), datepart(mm, a.admission_date)
  , case when (APCS_First_Ep_Ind != 0 AND (a.Der_Diagnosis_All like '%W01%' OR
				a.Der_Diagnosis_All like '%W03%' OR
				a.Der_Diagnosis_All like '%W04%' OR
				a.Der_Diagnosis_All like '%W05%' OR
				a.Der_Diagnosis_All like '%W06%' OR
				a.Der_Diagnosis_All like '%W07%' OR
				a.Der_Diagnosis_All like '%W08%' OR
				a.Der_Diagnosis_All like '%W18%' OR
				a.Der_Diagnosis_All like '%W19%')) then 1 else 0 end
  , case when (APCS_First_Ep_Ind = 1 AND (a.Der_Diagnosis_All like '%W01%' OR  
				a.Der_Diagnosis_All like '%W03%' OR
				a.Der_Diagnosis_All like '%W04%' OR
				a.Der_Diagnosis_All like '%W05%' OR
				a.Der_Diagnosis_All like '%W06%' OR
				a.Der_Diagnosis_All like '%W07%' OR
				a.Der_Diagnosis_All like '%W08%' OR
				a.Der_Diagnosis_All like '%W18%' OR
				a.Der_Diagnosis_All like '%W19%')) then 1 else 0 end
  , case when (APCS_First_Ep_Ind != 1 AND (a.Der_Diagnosis_All like '%S02%' OR
				a.Der_Diagnosis_All like '%S12%' OR
				a.Der_Diagnosis_All like '%S22%' OR
				a.Der_Diagnosis_All like '%S32%' OR
				a.Der_Diagnosis_All like '%S42%' OR
				a.Der_Diagnosis_All like '%S52%' OR
				a.Der_Diagnosis_All like '%S62%' OR
				a.Der_Diagnosis_All like '%S72%' OR
				a.Der_Diagnosis_All like '%S82%' OR
				a.Der_Diagnosis_All like '%S92%' OR
				a.Der_Diagnosis_All like '%T02%' OR
				a.Der_Diagnosis_All like '%T08%')) then 1 else 0 end
, case when (APCS_First_Ep_Ind = 1 AND (a.Der_Diagnosis_All like '%S02%' OR
				a.Der_Diagnosis_All like '%S12%' OR
				a.Der_Diagnosis_All like '%S22%' OR
				a.Der_Diagnosis_All like '%S32%' OR
				a.Der_Diagnosis_All like '%S42%' OR
				a.Der_Diagnosis_All like '%S52%' OR
				a.Der_Diagnosis_All like '%S62%' OR
				a.Der_Diagnosis_All like '%S72%' OR
				a.Der_Diagnosis_All like '%S82%' OR
				a.Der_Diagnosis_All like '%S92%' OR
				a.Der_Diagnosis_All like '%T02%' OR
				a.Der_Diagnosis_All like '%T08%')) then 1 else 0 end
   , case when a.Discharge_Method = '5' OR a.Discharge_Destination = '79' then 1 else 0 end
   , case when APCS_First_Ep_Ind = 1 and a.Der_Financial_Year >= '2015/16' and (c.Grand_Total_Payment_MFF is NULL or c.Grand_Total_Payment_MFF = 0) then 1
			when APCS_First_Ep_Ind = 1 and a.Der_Financial_Year = '2014/15' and (d.Grand_Total_Payment is NULL or d.Grand_Total_Payment = 0) then 1
			when APCS_First_Ep_Ind = 1 and a.Der_Financial_Year = '2013/14' and (e.Grand_Total_Payment is NULL or e.Grand_Total_Payment = 0) then 1
			else 0 end