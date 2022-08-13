---
title: "Creating a Context"
slug: "creating-a-context"
draft: false
images: []
weight: 9875
type: docs
toc: true
---

## Syntax
 - CREATE [OR REPLACE] CONTEXT namespace USING [schema.]package;
 - CREATE [OR REPLACE] CONTEXT namespace USING [schema.]package INITIALIZED EXTERNALLY;
 - CREATE [OR REPLACE] CONTEXT namespace USING [schema.]package INITIALIZED GLOBALLY;
 - CREATE [OR REPLACE] CONTEXT namespace USING [schema.]package ACCESSED GLOBALLY;

## Parameters
| Parameter | Details |
| ------ | ------ |
| `OR REPLACE` | Redefine an existing context namespace |
| namespace | Name of the context - this is the namespace for calls to `SYS_CONTEXT` |
| schema | Owner of the package |
| package | Database package that sets or resets the context attributes. Note: the database package doesn't have to exist in order to create the context. |
| `INITIALIZED` | Specify an entity other than Oracle Database that can set the context. |
| `EXTERNALLY` | Allow the OCI interface to initialize the context. |
| `GLOBALLY` | Allow the LDAP directory to initialize the context when establishing the session. |
| `ACCESSED GLOBALLY` | Allow the context to be accessible throughout the entire instance - multiple sessions can share the attribute values as long as they have the same Client ID. |

Oracle documentation (12cR1): http://docs.oracle.com/database/121/SQLRF/statements_5003.htm

## Create a Context
    CREATE CONTEXT my_ctx USING my_pkg;

This creates a context that can only be set by routines in the database package `my_pkg`, e.g.:

    CREATE PACKAGE my_pkg AS
      PROCEDURE set_ctx;
    END my_pkg;

    CREATE PACKAGE BODY my_pkg AS
      PROCEDURE set_ctx IS
      BEGIN
        DBMS_SESSION.set_context('MY_CTX','THE KEY','Value');
        DBMS_SESSION.set_context('MY_CTX','ANOTHER','Bla');
      END set_ctx;
    END my_pkg;

Now, if a session does this:

    my_pkg.set_ctx;

It can now retrieve the value for the key thus:

    SELECT SYS_CONTEXT('MY_CTX','THE KEY') FROM dual;

    Value

