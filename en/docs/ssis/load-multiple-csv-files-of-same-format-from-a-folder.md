---
title: "Load multiple CSV files of same format from a folder"
slug: "load-multiple-csv-files-of-same-format-from-a-folder"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

In this guide you can find out steps to load multiple CSV/TXT files from a folder to the database table.

## Parameters
| Parameter/Vaiable | Details |
| ------ | ------ |
| SourceFolder | It is a read only project parameter available and configurable at the deployment. Example of project parameter are Connection Strings, passwords, port no, users etc |
| CompleteSourceFilePath | It is read write user variable available only inside the package like local variables in programming languages |

## Steps to load data
To achieve this objective what we need is

 1. **Foreach Loop Container:** To Iterate over a directory to pick files.
 2. **Data Flow Task:** To load data from the CSV (Flat File Source) to the
        database table (OLE DB Destination).
3. **Flat File Source:** For text or csv files.
4. **OLE DB Destination:** To select the destination table which we want to populate with.

**Steps**

1. First Drag and drop a *Foreach Loop Container* from the **container** section of the SSIS toolbox.
2. Now double click on the **Project.params** in the solution Explorer and create a variable *SourceFolder* as string. In the value field type the path from which you want to pick the files. We are creating this path as a project parameter so that it can be configured after deployment.
3. Create a user variable by clicking on the **Variables** icon on the right and create *CompleteSourceFilePath* variable of type string. This variable will hold the value returned from the *Foreach loop container*.
4. Now Double click on the *Foreach loop Container* and select Collection on left hand side. On the right hand side select **Foreach File Enumerator**. Now for Expression click on the three dots on the right which will open a property editor select **Directory** in the property section and select *@[$Project:SourceFolder]* as its value. Click Ok.
[![ForEach property Expression Editor][1]][1]    
5. In Foreach Loop Editor window for Files enter *.txt or *.csv whatever file extension is required.
[![For Each File extension property][2]][2]    
6. On the left hand side of the Foreach Loop Editor select **Variable Mappings**, on the right select *User::CompleteSourceFilePath* which will automatically assigned Index 0. Click OK.
[![Foreach editor variable mapping][3]][3]
7. From the SSIS toolbox drag and drop *Data Flow Task* from the favorites section inside the *Foreach Loop Container*. Each file name returned by the *Foreach loop container* in CompleteSourceFilePath variable will be used in the *data flow task*.
[![Data Flow task inside for each][4]][4]
8. Now double click on the Data Flow task which will take us into the Data flow. Drag and drop a *Flat File Source* from the other Source section of the toolbox.
9. At the bottom of the screen in **Connection Managers** section right click and then select New *Flat File Connections*. Click on the Browse button and select one of the file that you want to process, set other properties like Text qualifier (like double quote). Click OK.         
10. Click on the new *Flat file connection* created in the connection Manager section and go the **Properties** window. Find the Expressions property and click on the three dots on the right. In the Property section select *ConnectionString* and in the Expression select the @[User::CompleteSourceFilePath] variable. Click OK.
[![enter image description here][5]][5]  
11. Select a *OLE DB Destination* (according to the database) and configure it to the table that you want to load.
12. Right click on the package name (solution explorer) and click Execute package to test the package. 

  [1]: https://i.stack.imgur.com/gfXgR.png
  [2]: https://i.stack.imgur.com/ismIQ.png
  [3]: https://i.stack.imgur.com/gTRU4.png
  [4]: https://i.stack.imgur.com/H2pEX.png
  [5]: https://i.stack.imgur.com/YaTVv.png

