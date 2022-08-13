---
title: "MongoDB Authorization Model"
slug: "mongodb-authorization-model"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Authorization is the basically  verifies user privileges. MongoDB support different kind of authorization models.

1. **Role base access control** <br>

Role are group of privileges, actions over resources. That are  gain to users over a given namespace (Database).

Actions are performs on resources.
Resources are any object that hold state in database. 

## Build-in Roles
Built-in database user roles and database administration roles roles exist in each database.

**Database User Roles**
1. `read`
2. `readwrite`

