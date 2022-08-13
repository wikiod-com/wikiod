---
title: "Anonymous PLSQL Block"
slug: "anonymous-plsql-block"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

Since they are unnamed, anonymous blocks cannot be referenced by other program units.

## An example of an anonymous block
    DECLARE
        -- declare a variable
        message varchar2(20);
    BEGIN
      -- assign value to variable
      message := 'HELLO WORLD';

      -- print message to screen
      DBMS_OUTPUT.PUT_LINE(message);
    END;
    /

