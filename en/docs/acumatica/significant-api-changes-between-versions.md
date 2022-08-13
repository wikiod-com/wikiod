---
title: "Significant API Changes Between Versions"
slug: "significant-api-changes-between-versions"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## PXSelectGroupBy and Bit Values in Acumatica 5.1 and 5.2+
The method of SQL generation from BQL `PXSelectGroupBy<>` data views has been changed in Acumatica Framework 5.2.

The sections below illustrate the differences using the example of `PXSelectGroupBy<FinYear, Aggregate<GroupBy<FinYear.finPeriods>>>.Select(graph)`:

# Acumatica Framework 5.2 and Later

    SELECT Max([finyear].[year]), 
           Max([finyear].[startdate]), 
           Max([finyear].[enddate]), 
           [finyear].[finperiods], 
           -- Attention!
           CONVERT (BIT, Max([finyear].[customperiods] + 0)),
           --
           Max([finyear].[begfinyearhist]), 
           Max([finyear].[periodsstartdatehist]), 
           Max([finyear].[noteid]), 
           ( NULL ), 
           ( NULL ), 
           ( NULL ), 
           Max([finyear].[tstamp]), 
           Max([finyear].[createdbyid]), 
           Max([finyear].[createdbyscreenid]), 
           Max([finyear].[createddatetime]), 
           Max([finyear].[lastmodifiedbyid]), 
           Max([finyear].[lastmodifiedbyscreenid]), 
           Max([finyear].[lastmodifieddatetime]) 
    FROM   finyear FinYear 
    WHERE  ( finyear.companyid = 2 ) 
    GROUP  BY [finyear].[finperiods] 
    ORDER  BY Max([finyear].[year])

# Acumatica Framework 5.1 and Earlier

    SELECT Max([finyear].[year]), 
           Max([finyear].[startdate]), 
           Max([finyear].[enddate]), 
           [finyear].[finperiods], 
           -- Attention!
           ( NULL ), 
           --
           Max([finyear].[begfinyearhist]), 
           Max([finyear].[periodsstartdatehist]), 
           ( NULL ), 
           ( NULL ), 
           ( NULL ), 
           Max([finyear].[tstamp]), 
           ( NULL ), 
           Max([finyear].[createdbyscreenid]), 
           Max([finyear].[createddatetime]), 
           ( NULL ), 
           Max([finyear].[lastmodifiedbyscreenid]), 
           Max([finyear].[lastmodifieddatetime]) 
    FROM   finyear FinYear 
    WHERE  ( finyear.companyid = 2 ) 
    GROUP  BY [finyear].[finperiods] 
    ORDER  BY Max([finyear].[year]) 

# Explanation

By default, the `Max()` aggregate is applied to all fields not explicitly mentioned in a BQL statement.

However, in Acumatica 5.1 and earlier, it excludes the `CreatedByID`, `LastModifiedByID`, and `bool` fields. When translated into SQL, these fields will always be `null` unless you explicitly grouped by. 

Starting from version 5.2, `Max()` will be applied by default for them, too.

