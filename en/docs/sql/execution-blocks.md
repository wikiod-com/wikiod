---
title: "Execution blocks"
slug: "execution-blocks"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Using BEGIN ... END
    BEGIN
      UPDATE Employees SET PhoneNumber = '5551234567' WHERE Id = 1;
      UPDATE Employees SET Salary = 650 WHERE Id = 3;
    END

