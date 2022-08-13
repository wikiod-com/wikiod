---
title: "Hierarchical Retrieval With Oracle Database 12C"
slug: "hierarchical-retrieval-with-oracle-database-12c"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

You can use hierarchical queries to retrieve data based on a natural hierarchical relationship between rows in a table

## Using the CONNECT BY Caluse
    SELECT E.EMPLOYEE_ID,E.LAST_NAME,E.MANAGER_ID FROM HR.EMPLOYEES E
    CONNECT BY PRIOR E.EMPLOYEE_ID = E.MANAGER_ID;

The `CONNECT BY` clause to define the relationship between employees and managers.

## Specifying the Direction of the Query From the Top Down
    SELECT E.LAST_NAME|| ' reports to ' ||
    PRIOR E.LAST_NAME "Walk Top Down"  
    FROM HR.EMPLOYEES E
    START WITH E.MANAGER_ID IS NULL
    CONNECT BY PRIOR E.EMPLOYEE_ID = E.MANAGER_ID;

