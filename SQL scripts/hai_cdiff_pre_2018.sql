SELECT [Provider_Code]
      ,[Count_Of_Cases]
      ,[Count_Of_Cases_Str]
      ,[Effective_Snapshot_Date]
  FROM [UKHF_HAI].[CDiff_By_Provider1_1]
where Effective_Snapshot_Date between '2008-10-01' and '2023-10-31'