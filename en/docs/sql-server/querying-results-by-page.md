---
title: "Querying results by page"
slug: "querying-results-by-page"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Row_Number()
    SELECT Row_Number() OVER(ORDER BY UserName) As RowID, UserFirstName, UserLastName
    FROM Users

From which it will yield a result set with a RowID field which you can use to page between.

    SELECT * 
    FROM 
        ( SELECT Row_Number() OVER(ORDER BY UserName) As RowID, UserFirstName, UserLastName
          FROM Users 
        ) As RowResults
    WHERE RowID Between 5 AND 10

