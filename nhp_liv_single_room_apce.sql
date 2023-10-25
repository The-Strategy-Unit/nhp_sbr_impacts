
  select Der_Provider_Code, Der_Provider_Site_Code
  , case when Der_Provider_Site_Code = 'REMRQ' then 'Royal Liverpool'
		when Der_Provider_Site_Code = 'REM21' then 'Aintree University'
		when Der_Provider_Site_Code = 'RBN01' then 'Whiston'
		when Der_Provider_Site_Code = 'REMBG' then 'Warrington'
		when Der_Provider_Site_Code = 'RVY01' then 'Southport & Formby'
		when Der_Provider_Site_Code = 'RVY02' then 'Ormskirk & District'
		when Der_Provider_Site_Code = 'RJR05' then 'Countess of Chester'
		else NULL end as [site_name]
  , Main_Specialty_Code
  , Admission_Method --elective/non-elective split
  , case when left(Admission_Method,1) = '1' then 'elective'
		when left(Admission_Method,1) = '2' then 'non_elective'
		else 'other' end as adm_meth_desc
  , case when Der_Provider_Site_Code = 'REMRQ' AND Admission_Date >= '2022-10-20' then 1 else 0 end as 'new_site'
  , case when Der_Provider_Site_Code = 'REMRQ' AND Admission_Date < '2022-10-20' and Discharge_Date >= '2022-10-20' then 1 else 0 end as 'moved_site'
  , case when Der_Provider_Site_Code = 'REMRQ' AND Discharge_Date < '2022-10-20' then 1 else 0 end as 'old_site'
  , datepart(yyyy, admission_date) as yr, datepart(wk, admission_date) as wk
  , case when Discharge_Date is NULL then 1 else 0 end as still_admitted
  , count(distinct apcs_ident) as spells, sum(Der_Episode_LoS) as spell_los
  FROM [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCE]
  where Der_Provider_Site_Code in ('REMRQ','REM21','RBN01','REMBG','RVY01', 'RVY02','RJR05')
  and Admission_Date between '2020-04-01' AND '2023-08-31'
  --and Discharge_Date is not NULL
  and Patient_Classification = '1' --exclude day case and regular
  group by Der_Provider_Code, Der_Provider_Site_Code
  , case when Der_Provider_Site_Code = 'REMRQ' then 'Royal Liverpool'
		when Der_Provider_Site_Code = 'REM21' then 'Aintree University'
		when Der_Provider_Site_Code = 'RBN01' then 'Whiston'
		when Der_Provider_Site_Code = 'REMBG' then 'Warrington'
		when Der_Provider_Site_Code = 'RVY01' then 'Southport & Formby'
		when Der_Provider_Site_Code = 'RVY02' then 'Ormskirk & District'
		when Der_Provider_Site_Code = 'RJR05' then 'Countess of Chester'
		else NULL end
  , Main_Specialty_Code
  , Admission_Method --elective/non-elective split
  , case when left(Admission_Method,1) = '1' then 'elective'
		when left(Admission_Method,1) = '2' OR Admission_Method = '81' then 'non_elective'
		else 'other' end
  , case when Der_Provider_Site_Code = 'REMRQ' AND Admission_Date >= '2022-10-20' then 1 else 0 end
  , case when Der_Provider_Site_Code = 'REMRQ' AND Admission_Date < '2022-10-20' and Discharge_Date >= '2022-10-20' then 1 else 0 end
  , case when Der_Provider_Site_Code = 'REMRQ' AND Discharge_Date < '2022-10-20' then 1 else 0 end
  , datepart(yyyy, admission_date), datepart(wk, admission_date)
  , case when Discharge_Date is NULL then 1 else 0 end