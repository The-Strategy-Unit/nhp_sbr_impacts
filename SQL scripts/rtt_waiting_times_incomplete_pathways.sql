SELECT [Organisation_Code]
      ,[Treatment_Function_Code]
      ,[Number_Of_Weeks_Since_Referral]
      ,[Number_Of_Incomplete_Pathways]
      ,[Number_Of_Incomplete_Pathways_with_DTA]
      ,[Effective_Snapshot_Date]
  FROM [UKHF_RTT].[Incomplete_Pathways_Provider1_1]
  where Effective_Snapshot_Date between '2008-10-01' and '2023-10-31'
  and ((Number_Of_Weeks_Since_Referral = '>0-1') or (Number_Of_Incomplete_Pathways != '0'))
