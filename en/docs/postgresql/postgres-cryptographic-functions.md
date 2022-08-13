---
title: "Postgres cryptographic functions"
slug: "postgres-cryptographic-functions"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

In Postgres, cryptographic functions can be unlocked by using pgcrypto module. 
CREATE EXTENSION pgcrypto;

## digest
`DIGEST()` functions generate a binary hash of the given data. This **can** be used to create a random hash.

Usage: `digest(data text, type text) returns bytea`

Or:    `digest(data bytea, type text) returns bytea`

Examples:

- `SELECT DIGEST('1', 'sha1')`

- `SELECT DIGEST(CONCAT(CAST(current_timestamp AS TEXT), RANDOM()::TEXT), 'sha1')`


