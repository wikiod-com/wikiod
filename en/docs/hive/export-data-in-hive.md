---
title: "Export Data in Hive"
slug: "export-data-in-hive"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Export feature in hive
**Exporting data from employees table to /tmp/ca_employees**

 

INSERT OVERWRITE LOCAL DIRECTORY '/tmp/ca_employees' SELECT name, salary, address FROM employees WHERE se.state = 'CA'; 

**Exporting data from employees table to multiple local directories based on specific condition**

The below query shows how a single construct can be used to export data to multiple directories based on specific criteria

    FROM employees se INSERT OVERWRITE DIRECTORY '/tmp/or_employees' SELECT * WHERE se.cty = 'US' and se.st = 'OR'
    INSERT OVERWRITE DIRECTORY '/tmp/ca_employees' SELECT * WHERE se.cty = 'US' and se.st = 'CA'
    INSERT OVERWRITE DIRECTORY '/tmp/il_employees' SELECT * WHERE se.cty = 'US' and se.st = 'IL';

