---
title: "Error logging"
slug: "error-logging"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Error logging when writing to database
Create Oracle error log table ERR$_EXAMPLE for existing EXAMPLE table:

    EXECUTE DBMS_ERRLOG.CREATE_ERROR_LOG('EXAMPLE', NULL, NULL, NULL, TRUE);

Make writing operation with SQL:

    insert into EXAMPLE (COL1) values ('example') 
    LOG ERRORS INTO ERR$_EXAMPLE reject limit unlimited;

