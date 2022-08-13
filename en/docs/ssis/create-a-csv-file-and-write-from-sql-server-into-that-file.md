---
title: "Create a CSV file and write from SQL Server into that file"
slug: "create-a-csv-file-and-write-from-sql-server-into-that-file"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

The guide helps in understanding how to import data from the SQL server table to a CSV/txt file.


1. Right click on the Data Flow Task and select property. **DefaultBufferMaxRows** and **DefaultBufferSize** properties can be changed to improve data load performance.
2. Multiple Data Flow Task can be executed in parallel for better performance.
3. Each Task has two flows success and failure. It is important to handle the failure flow to make the package more robust.  
4. Inside the Data Flow Task right click on the blue arrow and select **Enable Data Viewer** to check data flow at run-time.
5. If any column is deleted in the source or the destination to check which column get deleted. Inside Data Flow right click on the blue arrow and select **Resolve References**, in the new window we can see Unmapped Output columns (left) and Unmapped Input Columns (right). 

## Steps to import data
Here is what is require to complete this Objective.
1. **Data Flow Task:** Inside this task we will perform data import.
2. **OLE DB Source:** To select the source of data i.e SQL server database table.
3. **Flat File Destination:** Destination in which we want to load the data.

**Steps**
1. Drag and drop a *Data Flow Task* from the SSIS toolbox from the Favorites section.
[![enter image description here][1]][1]
2. Double click on the *Data Flow Task* in the Control Flow it will take us to the Data Flow.
3. Drag and drop a *OLE DB Source*, by default a cross will appear on it, it means it is not configured with a connection. Double click on the *OLE DB Source* task, click on the New.
[![enter image description here][2]][2]  
4. On the Configure OLE DB Connection Manager window click New. Now in the Connection Manager window select the Server Name to which you want to connect. Select Windows Authentication if your server is on your machine otherwise Use SQL Server Authentication and enter user name and password. Click on the **Test Connection** at the buttom left to check the validity of the credential entered. Click OK and then again OK.
[![enter image description here][3]][3]
5. In the OLE DB Source Editor select name of the table or the view and click on the Preview to check the data. Click Close then OK.
[![enter image description here][4]][4]
6. Drag and drop a *Flat File Destination* task from the SSIS toolbox under the section other destinations. Connect the *OLE DB Source* to the *Flat File Destination*. 
7. Double click on the *Flat File Destination*, click on New it will open Flat File Format window. Select **Delimited** if you want to specify the separator, text qualifier, end of line etc. Click OK.
[![enter image description here][5]][5] 
8. In the Flat File Connection Manager Editor click Browse button, select the path for file and enter file name click Open. Even though we have not selected any file we have just entered the file name the file will get created.
9. Now select the Code Page, Text Qualifier etc. Remember to tick the checkbox **column name in the first data row**. On the left side select Columns here you can specify the data separator like comma or a pipe (|). Click OK.
[![enter image description here][6]][6]   
10. In the Flat File Destination Editor **overwrite data in the file** is selected, update it according to requirement. On the left select Mappings and check if columns are mapped correctly. Click OK.
11. In the Solution Explorer right click on the package name and execute it to check. 
 

  [1]: https://i.stack.imgur.com/FQav9.png
  [2]: https://i.stack.imgur.com/IRQkT.png
  [3]: https://i.stack.imgur.com/5adZV.png
  [4]: https://i.stack.imgur.com/6Giap.png
  [5]: https://i.stack.imgur.com/ohpwF.png
  [6]: https://i.stack.imgur.com/X5K4E.png

