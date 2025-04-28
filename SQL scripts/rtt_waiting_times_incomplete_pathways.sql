SELECT [Organisation_Code]
      ,[Treatment_Function_Code]
      ,[Number_Of_Weeks_Since_Referral]
      ,[Number_Of_Incomplete_Pathways]
      ,[Number_Of_Incomplete_Pathways_with_DTA]
      ,[Effective_Snapshot_Date]
  FROM [UKHF_RTT].[Incomplete_Pathways_Provider1_1]
  where Treatment_Function_Code= '999' AND
 Effective_Snapshot_Date  between '2008-10-01' and '2025-03-31'