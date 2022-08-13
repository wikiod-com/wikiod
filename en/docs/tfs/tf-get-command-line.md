---
title: "TF GET (command-line)"
slug: "tf-get-command-line"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Parameters
| Parameter| Details  |
| ------ | ------ |
|/version|The version to retrieve. View **remarks**. |
|/all|Forces all files to be retrieved, not just those that are out-of-date.|
|/overwrite|Overwrites writable files that are not checked out|
|/force| Combines _/all_ and _/overwrite_|
|/preview|Displays what would occur, without actually performing the Get operation.|
|/recursive|Recursively retrieves all items that match your itemspec.|
|/noprompt|Suppresses any dialog boxes that would otherwise be displayed during this operation.|
|/remap|Updates local mapping on the server and re-downloads the file faster if you already have downloaded the same file from another branch.|
|/login |Specifies the user name and password to authenticate the user with Team Foundation Server.|



 1. The parameter **/version** assumes **Latest** if no parameter provided and accepts 
  
    a.Changesets ("C<_ChangeSetID_>")

    b.Date/Time ("D<_DateTime_UTC_>")

    c.Label ("L<_Label_>")

    d.Latest Version ("T")

    e.Workspace Version ("W<_workspacename;owner_>").
 2. Parameters as of **TFS 2010**
 3. Retrieved and edited from https://msdn.microsoft.com/en-us/library/fx7sdeyf(v=vs.100).aspx at 2016-07-22 14:48UTC




## Getting Files
    tf get /all /recursive $/
Gets _and_ replaces all files and directories from the last version available

## Get files, even if TFS believes the workspace is upto date
    tf.exe get /all /recursive /force /overwrite $/


Gets all files and directories, and will replace any files that are not currently checked out in the workspace (including files that have been edited).

