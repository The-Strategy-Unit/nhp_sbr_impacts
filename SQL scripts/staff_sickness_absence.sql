SELECT [Organisation_Code]
      ,[Organisation_Type]
      ,[FTE_Days_Sick]
      ,[FTE_Days_Available]
      ,[Effective_Snapshot_Date]
  FROM [UKHF_NHS_Workforce].[Sickness_Absence1_1]
    where Effective_Snapshot_Date between '2008-10-01' and '2023-10-31'
