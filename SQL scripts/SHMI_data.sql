-- SHMI from Apr 2010 to Aug 2023
-- annual indicator
-- site and trust level from Jan 2018, before Jan 2018 trust level only

SELECT 
	SHMI_Trust.Effective_Snapshot_Date,
	SHMI_Trust.Provider AS Provider_Code,
	'All' AS Site_Code,
	SHMI_Trust.SHMI_Value,
	SHMI_Trust.Observed,
	SHMI_Trust.Expected,
	SHMI_Trust.Denominator,
	'Trust' AS Level

FROM [UKHF_Mortality].[SHMI_Indicator1_1] AS SHMI_Trust

WHERE  Effective_Snapshot_Date BETWEEN '2008-10-01' AND '2023-10-31'

UNION

SELECT 
	SHMI_Site.Effective_Snapshot_Date,
	SHMI_Site.Provider_Code,
	SHMI_Site.Site_Code,
	SHMI_Site.SHMI_Value,
	SHMI_Site.Observed,
	SHMI_Site.Expected,
	SHMI_Site.Spells,
	'Site' AS Level

FROM [UKHF_Mortality].[SHMI_Indicator_Site_Level1_1] AS SHMI_Site

WHERE  Effective_Snapshot_Date BETWEEN '2008-10-01' AND '2023-10-31'