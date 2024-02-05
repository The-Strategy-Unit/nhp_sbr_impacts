-- SHMI from Apr 2010 to Aug 2023
-- annual indicator
-- site and trust level from Jan 2018, before Jan 2018 trust level only

IF OBJECT_ID(N'tempdb..#SHMI_All') IS NOT NULL
	DROP TABLE #SHMI_All;
	
SELECT 
	SHMI_Trust.Reporting_Period_Start,
	SHMI_Trust.Reporting_Period_End,
	SHMI_Trust.SHMI_Value,
	Providers.Organisation_Code AS Provider_Code,
	Providers.Organisation_Name AS Provider,
	'All' AS Site_Code,
	'All' AS Site,
	ROW_NUMBER() OVER(PARTITION BY Providers.Organisation_Code, Reporting_Period_Start ORDER BY Providers.Effective_To) AS Row,
	'Trust' AS Level

INTO #SHMI_All

FROM [UKHF_Mortality].[SHMI_Indicator1_1] AS SHMI_Trust

LEFT JOIN  [Internal_Reference].[Provider_1] AS Providers
	ON Providers.Organisation_Code = SHMI_Trust.Provider
	AND CAST(SHMI_Trust.Reporting_Period_End AS DATE) <= ISNULL(Providers.Effective_To, '2300-01-01')

UNION

SELECT 
	SHMI_Site.Reporting_Period_Start,
	SHMI_Site.Reporting_Period_End,
	SHMI_Site.SHMI_Value,
	SHMI_Site.Provider_Code,
	Providers.Organisation_Name AS Provider,
	SHMI_Site.Site_Code,
	Sites.Organisation_Name AS Site,
	ROW_NUMBER() OVER(PARTITION BY Sites.Organisation_Code, Reporting_Period_Start ORDER BY Sites.Effective_To) AS Row,
	'Site' AS Level

FROM [UKHF_Mortality].[SHMI_Indicator_Site_Level1_1] AS SHMI_Site

LEFT JOIN  [Internal_Reference].[Provider_1] AS Providers
	ON Providers.Organisation_Code = SHMI_Site.Provider_Code
	AND CAST(SHMI_Site.Reporting_Period_End AS DATE) <= ISNULL(Providers.Effective_To, '2300-01-01')

LEFT JOIN  [Internal_Reference].[Site_1] AS Sites
	ON Sites.Organisation_Code = SHMI_Site.Site_Code
	AND CAST(SHMI_Site.Reporting_Period_End AS DATE) <= ISNULL(Sites.Effective_To, '2300-01-01')

SELECT 
	Reporting_Period_Start,
	Reporting_Period_End,
	Provider_Code,
	Provider,
	Site_Code,
	Site,
	SHMI_Value,
	Level

FROM #SHMI_All

WHERE Row = 1

ORDER BY 1 