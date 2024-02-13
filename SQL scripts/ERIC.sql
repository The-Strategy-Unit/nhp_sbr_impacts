-- ERIC from March 2009 to March 2023
-- annual indicator
-- site level

SELECT 
	effective_snapshot_date,
	organisation_code,
	site_code,
	report_period_length,
	organisation_type,
	site_type,
	measure,
	measure_value,
	'Site' AS level

FROM [UKHF_Estates_Returns_Information_Collection].[Site_Data1_1] 

WHERE effective_snapshot_date > '2009-02-01'
	AND Measure IN (
		'Age profile - pre 1948 (%)'
		,'Age profile - 1948 to 1954 (%)'
		,'Age Profile - 1955 to 1964 (%)'
		,'Age profile - 1965 to 1974 (%)'
		,'Age profile - 1975 to 1984 (%)'
		,'Age Profile - 1985 to 1994 (%)'
		,'Age Profile - 1995 to 2004 (%)'
		,'Age Profile - 2005 to 2014 (%)'
		,'Age Profile - 2005 to Present (%)'
		,'Age Profile - 2015 to 2024 (%)'
		,'Average fee charged per hour for staff parking (£)'
		,'Occupied floor area (m²)'
		,'Site heated volume (m³)'
		,'Water volume (including borehole) (m³)'
		,'Cleaning service cost (£)'
		,'Cleaning staff (WTE)'
		,'Single bedrooms for patients with en-suite facilities (No.)'
		,'Single bedrooms for patients without en-suite facilities (No.)'
		,'Average fee charged per hour for patient/visitor parking (£)'
		,'Cost to eradicate high risk backlog (£)'
		,'Cost to eradicate Low Risk Backlog (£)'
		,'Cost to eradicate moderate risk backlog (£)'
		,'Cost to eradicate Significant Risk Backlog (£)'
		)

UNION

SELECT  
	effective_snapshot_date,
	organisation_code,
	'All' AS site_code,
	report_period_length,	
	organisation_type,
	'NA' AS site_type,
	CASE WHEN measure = 'Cleaning services costs (£)' THEN 'Cleaning service cost (£)'
		WHEN measure = 'Number of cleaning staff (WTE)' THEN 'Cleaning staff (WTE)'
		END AS measure,
	measure_value,
	'Trust' AS level

FROM [UKHF_Estates_Returns_Information_Collection].[Trust_Data1_1] 

WHERE effective_snapshot_date > '2009-02-01'
	AND Measure IN ('Number of cleaning staff (WTE)', 'Cleaning services costs (£)')
