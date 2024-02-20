SELECT [Site_Code]
      ,[Measure_Category]
      ,[Measure_Name]
      ,[Measure_Value]
      ,[Effective_Snapshot_Date]
  FROM [UKHF_FriendsAndFamilyTest].[Inpatients_Sites1_1]
    where Effective_Snapshot_Date between '2008-10-01' and '2023-10-31'