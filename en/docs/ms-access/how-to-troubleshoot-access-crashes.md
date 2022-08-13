---
title: "How to troubleshoot Access crashes"
slug: "how-to-troubleshoot-access-crashes"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

When you receive an error: "Microsoft Access has encountered a problem and needs to close", there is often not a lot of information to help you identify the cause of the error.  Below are a series of steps you can take to troubleshoot the cause of the errors.

**Be sure to remove other variables from the equation while testing**

**Network Corruption**

Do not load the client off of a network. Put it on the local drive and run it from there.

**Corporate Builds**

If you are in a corporate environment that is using "computer builds" and have had no success with Decompiling, Testing Memory, nor stripping Binary data - then refuse to do further testing until the IT team can provide the user with a test machine that has only Windows, Office, and Service Packs installed. 

All software and updates should be installed by hand without using unattended installs. Do not install antivirus on this machine for testing.

Understand that many IT departments simply try to do a One-Size-Fits-All approach with the builds, and their builds are all based on one another.  Over time, software conflicts can directly cause Access to crash or act strange. 

**Bad Power**

As mentioned in the memory example - power fluctuations can cause computer errors. If the database is in an industrial building, then try to get your hands on a power conditioner or a UPS that provides clean power (off the battery, not off the main passing through a Metal-oxide Varistor)

Also, check the power supply cable that is plugging into the power bar or outlet. Make sure that the gauge and voltage specs are sufficient. IT departments often leave power cables plugged in at the station and just remove the machine. After many years, they are using beefier power supplies, but haven't switched out the cable. It makes a difference. When in doubt, bring a new, thicker cable.

## Decompile Database
This should always be your initial fix. A good policy is to decompile the database before each release.

1. **Create a decompile shortcut**.  This loads the database with a "/decompile" switch.
   1. Right Click your Database File. Select Copy
   2. Right Click in the explorer window and select "Paste Shortcut"
   3. Right Click the shortcut and select "Properties"
   4. In the Target box, go to the end of the line and add `/decompile`
   5. Click Ok to close the shortcut

2. **Open Database with Shift.**  
   Hold down the shift key while double clicking on this shortcut. 
   This prevents any auto runs from executing within the database. 
   You should go straight to the navigation window.

3. **Compact and Repair the database.** 
   Once the database is loaded, you will need to click the Compact and Repair button.
   1. Locate the *Compact and Repair Database* button on the **Tools** Ribbon.
   2. Hold down the Shift Key.  Keep holding it down while you click the *Compact and Repair* button.

4. **Recompile the Database**   
   1. Go into the VBA window  (Control + G)
   2. Select Debug -> Compile from the menu

This is the complete decompile process.  Generally it should fix 99% of all Access crashes or weird form behaviour. 

## Test Computer Memory
If your crashes are random or sporadic, then do this step. If your crashes occur every single time you run the database, then this step won't fix the issue (although bad memory may be the reason why the corruption occurred in the first place).

Use a memory tester that boots outside the operating system and runs multiple passes. Two popular choices are [MemTest86][1] (Commercial) and [MemTest86+][2] (Open Source)

Start the test and let it run during working hours. The reason for this is because other factors in the building such as noise on the power circuits can cause memory errors, so you want to try to keep the variables the same.

If you have memory errors, then you will need to identify whether it is due to bad memory in the computer, or some other factor.   This is beyond the scope of this document however.

  [1]: http://www.memtest86.com/
  [2]: http://www.memtest.org/

## Remove Binary Data from Form
Sometimes the crashes occur constantly in a single form or report, or occur only when printing.  It is possible that the binary data within the form / report has become corrupt.  

**Save the Form / Report object as text**
There are two undocumented functions.  Application.SaveAsText and Application.LoadFromText.  You can use these functions to export the form/report definitions, clean up the definition, and then import it again.

1. Make a backup of your database before proceeding
2. Go to the VBA Immediate Window (Control + G)
3. Type `Application.SaveAsText acForm, "MyForm", CurrentProject.Path & "\MyForm.txt"` (Replace MyForm with the name of the Form / Report.  Use acReport if it is a corrupt report you are fixing)
4. Rename the original form item (e.g. rename to MyForm.Bak) within the Database Window

**Clean up the Form / Report Definitions file**
1. Open the exported file (e.g. MyForm.txt) in notepad
2. Delete the "Checksum=" line (should be on line 3)
3. Clear out binary data
   1. Identify the binary data blocks. Look through the file and you will see lines that start with "Parameter = Begin".  Following those lines you will have lines of encoded binary data.  Finally, the binary block will end with a line consisting only of "End".  The Binary Data block includes the first line (with the Begin statement) and all lines up to and including the last line (with the End Statement). 

      Note: All of these blocks should appear BEFORE your form control definitions

   2. Delete the binary data blocks for the following parameters:
      * NameMap
      * PrtMip
      * PrtDevMode
      * PrtDevNames
      * PrtDevModeW
      * PrtDevNamesW
   
  4. Look for other issues.  While you have the file open, scroll through the rest of the file and look for anything that catches your eye, especially in the VBA module code at the bottom.  You will be looking for anything that sticks out from the rest, and may be corruption.  
  5. Save the file.  

**Load the form / report back into Access and Test**

1. Load the form back into Access.  
    * In Access, go to the immediate window (Control + G)
    * Type `Application.LoadFromText acForm, "MyForm", CurrentProject.Path & "\MyForm.txt"`
    * Decompile / Compact Repair / Recompile  (See the other example within the documentation)
    * Open the form / report to test.  Hopefully everything is working now.
    * Delete the old corrupt form (e.g. MyForm.bak)

**Prevent this corruption in the future**

The most common cause of corrupt binary data within a report / form is when multiple computers / users use the same database client file instead of having their own separate copy.  This is why each user should have their own client file on their desktop that they run.  

## Remove "OLE Object" fields
If you have images or other data stored in Access itself as OLE Objects, then you should find a better approach. When the OLE data is stored, it is stored according to the software (and version of software) on the computer storing it. When another computer goes to display that OLE Object data on the form, but doesn't have the exact software / version installed - quite often this results in an application crash.

If you are storing image data, then a better approach is to store the filename, and instead save the images to a standard location. Newer versions of access have the native controls to make this the way to go.  


## Rebuild the entire database
This is a lot of work, so do this as a last resort after exhausting all other options.  You only need to do this if the problem is occurring for different users, on different machines. If it isn't occurring for all users, then most likely it is not a corrupt database container.

Similar to the steps in removing binary data, you are going to rebuild your database from scratch. This process is a little ritualistic, but if done meticulously with care not to "preserve" any possible corruption, then the process is highly effective. 

**Create a new access database container.**
* In Access, on the File Tab, you can select "New".  Create a new, empty database in ACCDB format.

**Move all objects to the new container**

Do **not** use the Import / Export functions within Access to move the objects, and do not simply click and drag.  Doing this can copy the corrupt items over to the new container.

**Tables:**

* For each table in the old access container, create a new table in the new container. 
* From design view, copy/paste the field definitions.
* Check the table properties to ensure they match in both databases
* Move any Data Macros over as well (see the Macros section for how to do this)
* To move the data, export the old data to XML or CSV, and then import from that format.

**Queries:**
* Load each query into SQL view.
* Copy / Paste the SQL text.  
* Paste into the new database.
* Compare Query properties to ensure they match.

**Forms / Reports:**

* For each Form / Report, use the Application.SaveAsText function to export the forms/reports to a text file.
* Remove the Binary Data  (see *Remove Binary Data from Form* documentation to acquaint yourself with this process)
* Use the Application.LoadFromText function to reimport the objects into the new database

**Macros**

You have three methods of moving the Macros.
1. Recreate each macro by hand in the new database container.
2. Use the `Application.SaveAsText` / `Application.LoadFromText` method with the `acMacro` parameter.
3. Copy/Paste Macro definitions for each macro
   * Select All (Control + A) to select all macro elements.  Then Copy (Control + C).  
   * Open a blank Notepad document and Paste (Control + V) the Macro XML.
   * Create a new blank macro in the new database container.
   * In Notepad, Select All text (Control + A).  Then Copy (Control + C)
   * In the blank macro, Paste (Control + V).  The Macro should appear.  Save it.

**Modules**
* For each module, select all code (Control + A) and paste (Control + V) into the new database container.
* Be sure to check the Database Properties (In VBA Window, go Tools -> Client Properties)

**Data Macros**

For each Data Macro, use the SaveAsText / LoadFromText methods.
1. Go into the VBA Immediate Window (Control + G)
2. Type `Application.SaveAsText acTableDataMacro, "MyTableName", CurrentProject.Path & "\MyTableName.txt"` (Replace MyTableName with the name of the table containing the data macros)
3. Review the file for any signs of corruption
4. In the new database container, load the definition using `Application.LoadFromText acTableDataMacro, "MyTableName", CurrentProject.Path & "\MyTableName.txt"`

As previously mentioned, this is a **LOT** of work, but it has results.  This method should also be used when migrating an Access 97 database to 2000, or an Access 2000 database to 2003.

