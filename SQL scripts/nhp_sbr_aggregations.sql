----## testing the hospital codes over time...
select Der_Provider_Site_Code
, yr, mth
, sum(spells) as spells
from [NHSE_Sandbox_StrategyUnit].dbo.[sbr_apce_temp]
where Der_Provider_Site_Code in ('RWF01','RWF02','RWFTW')
group by Der_Provider_Site_Code, yr, mth
order by Der_Provider_Site_Code, yr, mth

select case when Der_Provider_Site_Code in ('RWF01','RWF02','RWFTW') then 'royal liv' end as prov_site
, yr, mth
, sum(spells) as spells
from [NHSE_Sandbox_StrategyUnit].dbo.[sbr_apce_temp]
where Der_Provider_Site_Code in ('RWF01','RWF02','RWFTW')
group by case when Der_Provider_Site_Code in ('RWF01','RWF02','RWFTW') then 'royal liv' end, yr, mth
order by case when Der_Provider_Site_Code in ('RWF01','RWF02','RWFTW') then 'royal liv' end, yr, mth

----## testing occurrence of falls and fractures
   Select fall_first, fall_after, fracture_first, fracture_after , sum(spells) as spells
   from [NHSE_Sandbox_StrategyUnit].dbo.[sbr_apce_temp]
   where Der_Provider_Site_Code = 'RWFTW'
   group by fall_first, fall_after, fracture_first, fracture_after
   order by fall_first, fall_after, fracture_first, fracture_after

----## general time series for spells, events, los
Select Der_Provider_Site_Code, yr, mth
, sum(spells) as spells
, sum(case when (fall_first = 0 AND fracture_first = 0) AND (fall_after = 1 OR fracture_after = 1) then 1 else 0 end) as fall_fracs
, sum(death_spell) as deaths
, sum(spell_los) as los
   from [NHSE_Sandbox_StrategyUnit].dbo.[sbr_apce_temp]
   where left(Der_Provider_Site_Code, 1) = 'R'
   group by Der_Provider_Site_Code, yr, mth
   order by Der_Provider_Site_Code, yr, mth

----## costs by site by fin_year and admission method
select der_provider_site_code, adm_meth_desc
, case when mth < 4 then cast((yr-1) as varchar)+'/'+cast(right(yr,2) as varchar) else cast(yr as varchar)+'/'+cast(right((yr+1),2) as varchar) end as der_financial_year
, yr, mth
, sum(spells) as spells, sum(spell_cost) as costs
into #1
from [NHSE_Sandbox_StrategyUnit].dbo.[sbr_apce_temp]
   where left(Der_Provider_Site_Code, 1) = 'R'
   and yr >= 2013
   and adm_meth_desc != 'other'
   and not_costed = 0
   and spell_cost > 0
   group by Der_Provider_Site_Code, adm_meth_desc
   , case when mth < 4 then cast((yr-1) as varchar)+'/'+cast(right(yr,2) as varchar) else cast(yr as varchar)+'/'+cast(right((yr+1),2) as varchar) end
   , yr, mth
   order by Der_Provider_Site_Code, der_financial_year, adm_meth_desc

select *
, costs/spells as unit_cost
from #1
order by Der_Provider_Site_Code, adm_meth_desc, der_financial_year, yr, mth

----## all data if just want a grande extract
select *
from [NHSE_Sandbox_StrategyUnit].dbo.[sbr_apce_temp]
where left(Der_Provider_Site_Code, 1) = 'R'
order by Der_Provider_Site_Code, yr, mth


