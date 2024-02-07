SELECT [Collection]
      ,[Organisation_Type]
      ,[Organisation_Code]
      ,[Metric]
      ,[Figure]
      ,[Effective_Snapshot_Date]
  FROM [UKHF_HAI].[Klebsiella_Species_Bacteraemia1_1]
   where Effective_Snapshot_Date between '2008-10-01' and '2023-10-31'