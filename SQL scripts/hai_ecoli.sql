SELECT [Collection]
      ,[Organisation_Type]
      ,[Organisation_Code]
      ,[Metric]
      ,[Figure]
      ,[Effective_Snapshot_Date]
  FROM [UKHF_HAI].[E_Coli1_1]
   where Effective_Snapshot_Date between '2008-10-01' and '2023-10-31'