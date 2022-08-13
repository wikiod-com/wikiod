---
title: "Role Strategy Plugin"
slug: "role-strategy-plugin"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Configuration
## Manage Roles
**Global Roles**- Create roles with selected set of Jenkins features
e.g. Usually for a development project, 2 roles can be created.

 1. Developer- Global role can be set to only **Overall** : Read
 2. ProjectOwner- Global role can be set to **Overall** : Read

This restricts developer and project owner to read access to all Jenkins features.

[![enter image description here][1]][1]

**Project Roles**- Create roles by restricting user access respective jenkins job and credential features using regular expressions. 

E.g. for a development project 'MyProjectA'; project owners needs to have  full permissions to Jobs and developers need Build access to Jenkins jobs. So we create below roles:

 - **ProjectA_admin**- check all options under Job viz. *Build, Cancel, Configure, Create, Delete, Discover, Move, Read, Workspace*
 - **ProjectA_dev** - check options Build, Cancel, Read, Workspace under Job

[![enter image description here][2]][2]

> To restrict above projects to respective project owners and
> developers, all jobs must follow a pre-defined pattern.

 Assume 'MyProjectA' needs 3 jenkins build jobs: *MyProjectA_Dev_Build, MyProjectA_QA_Build, MyProjectA_Nightly_Sonar_Analysis*

To restrict project owner and developers of project 'MyProjectA' to above build jobs, provide '*Pattern*' as **MyProjectA.***.

##  Assign Roles 

Helps to assign users or project groups to respective Global or Project roles.
E.g. to assign a developer 'Gautam' to Developer global role, provide the user name 'Gautam', click *Add* and select the check box next to 'Gautam' and below Developer global role. 

[![enter image description here][3]][3]

Similarly add the user under project roles and select respective project roles to assign required project roles.

If you notice below screenshots you can see user 'gautam' has access only to projects starting with MyProjectA. Also, user's access is restricted to build and configure is missing.

[![enter image description here][4]][4]

[![enter image description here][5]][5]


  [1]: http://i.stack.imgur.com/sXAfc.png
  [2]: http://i.stack.imgur.com/HwDpt.png
  [3]: http://i.stack.imgur.com/ShAIM.png
  [4]: http://i.stack.imgur.com/rqyzr.png
  [5]: http://i.stack.imgur.com/RYVRn.png

