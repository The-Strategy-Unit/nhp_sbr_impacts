-- Occupancy by sector by Trust
-- is annual from Apr08 to Apr10, then quarterly from Apr10 
SELECT 
	Available.Effective_Snapshot_Date,
	Available.Organisation_Code,
	Available.Sector,
	Available.Report_Period_Length,
	Available.Number_Of_Beds AS Available,
	Occupied.Number_Of_Beds AS Occupied,
	CASE WHEN Available.Number_Of_Beds = 0 AND Occupied.Number_Of_Beds = 0 THEN NULL
		ELSE Occupied.Number_Of_Beds / Available.Number_Of_Beds 
		END AS Bed_Occupancy

FROM [UKHF_Bed_Availability].[Provider_By_Sector_Available_Overnight_Beds1_1] AS Available

LEFT JOIN [UKHF_Bed_Availability].[Provider_By_Sector_Occupied_Overnight_Beds1_1] AS Occupied
	ON Occupied.Effective_Snapshot_Date = Available.Effective_Snapshot_Date
	AND Occupied.Organisation_Code = Available.Organisation_Code
	AND Occupied.Sector = Available.Sector

WHERE Available.Effective_Snapshot_Date BETWEEN '2008-10-01' AND '2023-10-31'