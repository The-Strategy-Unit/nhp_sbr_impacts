
----## union of all the yearly files then clean up sandbox
select *
into NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_all
from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_0809

union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_0910
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1011
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1112
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1213
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1314
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1415
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1516
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1617
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1718
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1819
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1920
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_2021
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_2122
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_2223
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_2324
union all
select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_2425

order by Der_Provider_Site_Code, [period]

select * from NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_all
--where beddays > 0
order by Der_Provider_Site_Code, [period]

--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_0809
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_0910
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1011
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1112
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1213
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1314
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1415
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1516
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1617
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1718
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1819
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_1920
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_2021
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_2122
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_2223
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_2324
--drop table NHSE_Sandbox_StrategyUnit.dbo.nhp_sbr_bd_2425