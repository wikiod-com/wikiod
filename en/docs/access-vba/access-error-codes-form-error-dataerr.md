---
title: "Access Error Codes (Form.Error, DataErr)"
slug: "access-error-codes-formerror-dataerr"
draft: false
images: []
weight: 9658
type: docs
toc: true
---

The most tricky part is finding description for error codes.
[This site][1] has a most complete list of codes I've found so far. 

Below you can find a copy cleaned of "@@@"s.

|Error Number|Error Description|
|---------|------------------------------|
|3|Return without GoSub|
|5|Invalid procedure call or argument|
|6|Overflow|
|7|Out of memory|
|9|Subscript out of range|
|10|This array is fixed or temporarily locked|
|11|Division by zero|
|13|Type mismatch|
|14|Out of string space|
|16|Expression too complex|
|17|Can't perform requested operation|
|18|User interrupt occurred|
|20|Resume without error|
|28|Out of stack space|
|35|Sub or Function not defined|
|47|Too many DLL application clients|
|48|Error in loading DLL|
|49|Bad DLL calling convention|
|51|Internal error|
|52|Bad file name or number|
|53|File not found|
|54|Bad file mode|
|55|File already open|
|57|Device I/O error|
|58|File already exists|
|59|Bad record length|
|61|Disk full|
|62|Input past end of file|
|63|Bad record number|
|67|Too many files|
|68|Device unavailable|
|70|Permission denied|
|71|Disk not ready|
|74|Can't rename with different drive|
|75|Path/File access error|
|76|Path not found|
|91|Object variable or With block variable not set|
|92|For loop not initialized|
|93|Invalid pattern string|
|94|Invalid use of Null|
|96|Unable to sink events of object because the object is already firing events to the maximum number of event receivers that it supports|
|97|Can not call friend function on object which is not an instance of defining class|
|98|A property or method call cannot include a reference to a private object, either as an argument or as a return value|
|321|Invalid file format|
|322|Can't create necessary temporary file|
|325|Invalid format in resource file|
|380|Invalid property value|
|381|Invalid property array index|
|382|Set not supported at runtime|
|383|Set not supported (read-only property)|
|385|Need property array index|
|387|Set not permitted|
|393|Get not supported at runtime|
|394|Get not supported (write-only property)|
|422|Property not found|
|423|Property or method not found|
|424|Object required|
|429|ActiveX component can't create object|
|430|Class does not support Automation or does not support expected interface|
|432|File name or class name not found during Automation operation|
|438|Object doesn't support this property or method|
|440|Automation error|
|442|Connection to type library or object library for remote process has been lost. Press OK for dialog to remove reference.|
|443|Automation object does not have a default value|
|445|Object doesn't support this action|
|446|Object doesn't support named arguments|
|447|Object doesn't support current locale setting|
|448|Named argument not found|
|449|Argument not optional|
|450|Wrong number of arguments or invalid property assignment|
|451|Property let procedure not defined and property get procedure did not return an object|
|452|Invalid ordinal|
|453|Specified DLL function not found|
|454|Code resource not found|
|455|Code resource lock error|
|457|This key is already associated with an element of this collection|
|458|Variable uses an Automation type not supported in Visual Basic|
|459|Object or class does not support the set of events|
|460|Invalid clipboard format|
|461|Method or data member not found|
|462|The remote server machine does not exist or is unavailable|
|463|Class not registered on local machine|
|481|Invalid picture|
|482|Printer error|
|735|Can't save file to TEMP|
|744|Search text not found|
|746|Replacements too long|
|2001|You canceled the previous operation. |
|2002|You tried to perform an operation involving a function or feature that was not installed in this version of Microsoft Access. |
|2004|There isn't enough memory to perform this operation. Close unneeded programs and try the operation again.|
|2005|There isn't enough free memory to start Microsoft Access. Close unneeded programs and try again. For information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'.|
|2006|The object name '_1' you entered doesn't follow Microsoft Access object-naming rules. For more information about naming objects, click Help. |
|2007|You already have an open database object named '_’. Use a different name for each database object of the same type. If you want this object to replace the original object, close the original object, and then save this object using the same name. For more information on renaming a database object, click Help. |
|2008|You can't delete the database object '_' while it's open. Close the database object, and then delete it. |
|2009|You can't rename the database object '_' while it's open. Close the database object, and then rename it.|
|2010|You can't delete the database object '_' while it's open. Close the database object, and then delete it. |
|2011|The password you entered is incorrect.|
|2014|You have given this _1 the same name as an existing _2 in your database. You can't give a table and a query the same name. Give this object a name that isn't already used by another table or query. |
|2015|There are no registered wizards of this type. Rerun Microsoft Access or Microsoft Office Setup to reinstall the wizards. If you want to preserve your security or custom settings, back up the Microsoft Access workgroup information file. For more information on backing up files, search the Microsoft Windows Help index for 'backing up files'. |
|2016|You can't modify the attributes of System Tables.|
|2017|Microsoft helps protect this Visual Basic for Applications Project with a password. You must supply the password in the Visual Basic Editor before you can perform this operation.|
|2018|The data access page name '_' you entered is misspelled or refers to a data access page that isn't open or doesn't exist. |
|2019|The number you used to refer to the data access page is invalid. Use the Count property to count the open data access pages and make sure that the page number is not greater than the number of open data access pages minus one. |
|2021|One or more operators in the filter expression is invalid. For a valid list of operators refer to the help file. |
|2022|You entered an expression that requires a data access page to be the active window. |
|2024|The report snapshot was not created because you don't have enough free disk space for temporary work files. To fix this, free up disk space (for example, empty the Recycle Bin or delete unnecessary files). |
|2025|The file is not in the correct format for a Microsoft Access project. |
|2027|This operation is not supported for Microsoft Access 1.X databases. |
|2028|Microsoft Access was unable to close the database object.|
|2029|Microsoft Office applications cannot suspend while you have documents open from a network location. Exit the applications or close the open documents and try again.|
|2030|The Microsoft Access project '_1' will be opened read-only because one of the following occurred: Either the file is locked for editing by another user, the file (or the folder in which it is located) is marked as read-only, or you specified that you wanted to open the file read-only.|
|2031|You can't convert or enable an MDE file. |
|2033|Name conflicts with existing module, project, or object library. |
|2034|Cannot Compile Project. |
|2035|Cannot Load Project of wrong version. |
|2037|Microsoft Access could not perform name AutoCorrect during this operation. The 'Log name AutoCorrect' option is set, but the Data and Misc. Objects is not checked out. |
|2038|The file '_' cannot be opened because it has been locked by another user.|
|2040|Microsoft Access can't run. |
|2041|Microsoft Access couldn't find file '_1'. This file is required for startup.|
|2042|A system error occurred, or there isn't enough free memory to start Microsoft Access. Close unneeded programs and try again.|
|2043|Microsoft Access can't find the database file '_1.' Make sure you entered the correct path and file name. |
|2044|You can't exit Microsoft Access now. If you're running a Visual Basic module that is using OLE or DDE, you may need to interrupt the module. |
|2045|The command line you used to start Microsoft Access contains an option that Microsoft Access doesn't recognize. Exit and restart Microsoft Access using valid command-line options. |
|2046|The command or action '_1' isn't available now. * You may be in a read-only database or an unconverted database from an earlier version of Microsoft Access. * The type of object the action applies to isn't currently selected or isn't in the active view. Use only those commands and macro actions that are currently available for this database.  |
|2048|There isn't enough free memory to open the file '_.' Close unneeded programs and try again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'. |
|2050|Enter an OLE/DDE Timeout setting from 0 through 300 seconds. |
|2051|The object name '_1' can't be longer than _2 characters according to Microsoft Access object-naming rules. |
|2052|There isn't enough free memory to update the display. Close unneeded programs and try again.|
|2053|The command name can't be blank. Please choose a name. |
|2054|Microsoft Access is unable to load the Visual Basic for Applications dynamic-link library (DLL) Vbe7. Rerun the Microsoft Access Setup program. |
|2055|The expression '_' you entered is invalid. |
|2056|Microsoft Access can't supply context-sensitive Help. |
|2057|There isn't enough stack memory left to perform the operation. The operation is too complicated. Try simplifying the operation. |
|2058|The file '_1' is incompatible. Microsoft Access needs to be reinstalled. Run Setup to reinstall Microsoft Access. If you want to preserve your security or custom settings, back up the Microsoft Access workgroup information file. For more information on backing up files, search the Microsoft Windows Help index for 'backing up files'. |
|2059|Microsoft Access cannot find the object '_1'. Make sure the object exists and that you spell its name correctly. |
|2060|You can't create a field list based on the action query '_’. Action queries don't have fields. A form or report must be based on a table, or on a select or crosstab query. Change the RecordSource property for the form or report, or open the action query and change it to a select query.  |
|2061|Enter a zero or greater-than-zero number for this option. |
|2062|The command name must be shorter than 255 Characters. Please choose a name. |
|2063|Microsoft Access can't create, open, or write to the index file '_1'; the information (.inf) file it uses to keep track of dBASE indexes. The index file may be damaged, or you may not have read/write permission for the network drive you're trying to link to. You can link to the dBASE file without specifying any dBASE indexes, but the existing indexes will not be used with the linked table.  |
|2064|The menu bar value '_' is invalid. You supplied an argument to the DoMenuItem method that refers to a menu bar that is invalid. Use an intrinsic constant or numeric value that refers to a valid menu bar value, such as acFormbar.  |
|2065|The name for the menu, command, or subcommand you entered is invalid. You supplied an argument to the DoMenuItem method that refers to a menu name, command, or subcommand that is invalid. Use an intrinsic constant or numeric value that refers to a valid menu, command, or subcommand value, such as acRecordsMenu.  |
|2067|A menu bar macro can only be run if the menu bar macro name is the setting used by particular properties or options. You tried to run a menu bar macro containing the AddMenu action. Set one of the following properties or options to the name of the menu bar macro: * The MenuBar property of a form or report. * The ShortcutMenuBar property of a form, report, or control. * The Menu Bar or Shortcut Menu Bar option in the Startup dialog box. This error also occurs if Microsoft Access attempts to run a menu bar macro containing an AddMenu action that follows an action that makes some other object the active object. For example, the OpenForm action. |
|2068|The selected item is customized and doesn't have context-sensitive Help. For more information on creating custom Help for a form, report, or control, click Help. |
|2069|The key or key combination _1 in _2 has invalid syntax or is not allowed. Use the SendKeys syntax to specify the key or key combinations. For the allowed key or key combinations, click Help. |
|2070|You already assigned the key or key combination _1 in _2 to another macro. Only the first key or key combination will be used. |
|2071|The Docking property can't be set to '_1' at this time. If you want to set the Docking property to '_2', move the toolbar from its current position and try again. |
|2072|All objects were imported successfully.|
|2073|Successfully exported '_'.|
|2074|This operation is not supported within transactions. |
|2075|This operation requires an open database. |
|2076|Successfully linked '_'.|
|2077|This Recordset is not updatable|
|2078|Help isn't available due to lack of memory or improper installation of Microsoft Windows or Microsoft Access. For more information on troubleshooting a low memory problem, search the Microsoft Windows Help index for 'memory, troubleshooting'. If you need to reinstall Microsoft Access, you may want to preserve your security or custom settings. To do so, back up the Microsoft Access workgroup information file. For more information on backing up files, search the Microsoft Windows Help index for 'backing up files'.  |
|2079|Form is read-only, because the Unique Table property is not set.|
|2080|The toolbar or menu _ already exists. Do you want to replace the existing toolbar or menu? |
|2081|The Create From Macro command only works when a macro is selected in the Navigation Pane. |
|2083|The database '_' is read-only. You can't save changes made to data or object definitions in this database. |
|2084|Field '_' is based on an expression and can't be edited|
|2085|The ODBC Refresh Interval setting must be from 1 through 32,766 seconds. |
|2086|Recordset requires a form to be updatable.|
|2087|Microsoft Access can't display the Add-ins submenu. The Add-ins submenu expression '_1' you entered exceeds the 256-character limit. Shorten the macroname or functionname expression in the Menu Add-ins key of the Windows Registry setting, and then restart Microsoft Access. For more information on customizing Microsoft Access settings in the Windows Registry, click Help.  |
|2088|Microsoft Access can't display the Add-ins submenu _1 because a setting you entered in the Windows Registry is missing a macro name or function name expression.  Supply the missing expression in the Menu Add-ins key of the Windows Registry, and then restart Microsoft Access. For more information on customizing Microsoft Access settings in the Windows Registry, click Help.|
|2089|Microsoft Access can't display the same menu more than once in a menu bar. |
|2090|An action within the current global menu's macro group can't change the global menu bar. Microsoft Access can't display the global menu bar because the macro called when you first set the global menu includes another action that tries to reset the global menu. Check your menu bar macros, and make sure that you set the global menu bar only once.  |
|2091|'_' is an invalid name. |
|2092|The value you specified for the Setting argument in the SetOption method is not the correct type of Variant for this option. You specified a string when Microsoft Access expected a number. See the Access Options dialog box (click the File tab, and then click Access Options) to see what type of data is required to set this particular option. For example, the setting for the Default Database Folder option must be a string. To see what type of Variant you passed to the SetOption method, use the VarType function. For more information, search the Help index for 'Variant data type' and 'VarType function'.  |
|2093|The numeric value for the Setting argument in the SetOption method does not correspond to any list box or option group settings in the Access Options dialog box. Valid settings are 0 (the first item in the list) through _ (the last item in the list). |
|2094|Microsoft Access cannot find the toolbar '_1’. You tried to run a macro that includes a ShowToolbar action or a Visual Basic for Applications procedure that includes a ShowToolbar method. * The toolbar name might be misspelled or might refer to a legacy toolbar that is no longer available. * This action might refer to a custom toolbar that was deleted from or renamed in the current database. * This action might refer to a custom toolbar that exists in a different database. |
|2097|The table for which you tried to create an import/export specification was created in an earlier version of Microsoft Access. To convert this database to the current version of Microsoft Access, click the File tab, and then click 'Convert'. |
|2098|The operation could not be completed because the Action Tag '_' is not recognized by your system. |
|2100|The control or subform control is too large for this location. The number you entered for the Left, Top, Height, or Width property is too large or is a negative number. Reduce the size of the control or subform control, or enter a positive number. |
|2101|The setting you entered isn't valid for this property. To see the valid settings for this property, search the Help index for the name of the property. |
|2102|The form name '_' is misspelled or refers to a form that doesn't exist. If the invalid form name is in a macro, an Action Failed dialog box will display the macro name and the macro's arguments after you click OK. Open the Macro window, and enter the correct form name. |
|2103|The report name '_' you entered in either the property sheet or macro is misspelled or refers to a report that doesn't exist. If the invalid report name is in a macro, an Action Failed dialog box will display the macro name and the macro's arguments after you click OK. Open the Macro window, and enter the correct report name. |
|2104|You entered the control name '_,' which is already in use. You already have a control on the form with this name, or an existing control has its name mapped to this name for Visual Basic. Visual Basic maps spaces in control names to underscores. For example, My Control and My_Control are treated as duplicate names. |
|2105|You can't go to the specified record. You may be at the end of a recordset. |
|2106|_1 errors occurred when you loaded the form or report. You loaded a form or report that has controls or properties that Microsoft Access doesn't recognize and will ignore. |
|2107|The value you entered does not meet the validation rule defined for the field or control. To see the validation rule, switch to Design view or Layout view, click the appropriate field, and then, if the property sheet is not open, press F4. Then, click the Data tab in the property sheet. Enter a value that meets the validation rule, or press ESC to undo your changes.  |
|2108|You must save the field before you execute the GoToControl action, the GoToControl method, or the SetFocus method. You tried to move the focus to another control using the SetFocus method, GoToControl action, or the GoToControl method. Set the macro or method to the AfterUpdate property instead of the BeforeUpdate property so it saves the field before changing the focus.  |
|2109|There is no field named '_' in the current record. |
|2110|Microsoft Access can't move the focus to the control _1. * The control may be a type that can't receive the focus, such as a label. * The control's Visible property may be set to No. * The control's Enabled property may be set to No. |
|2111|The changes you made can't be saved. The save operation may have failed due to the temporary locking of the records by another user. * Click OK to try again. You may need to click OK several times (or wait until the other user closes the table). * Click Cancel if repeated attempts to save your changes fail. |
|2112|The item on the Clipboard can't be pasted into this control. |
|2113|The value you entered isn't valid for this field. For example, you may have entered text in a numeric field or a number that is larger than the FieldSize setting permits. |
|2114|Microsoft Access doesn't support the format of the file '_1,' or file is too large. Try converting the file to BMP format. |
|2115|The macro or function set to the BeforeUpdate or ValidationRule property for this field is preventing Microsoft Access from saving the data in the field. * If this is a macro, open the macro in the Macro window and remove the action that forces a save (for example, GoToControl). * If the macro includes a SetValue action, set the macro to the AfterUpdate property of the control instead. * If this is a function, redefine the function in the Module window. |
|2116|The value violates the validation rule for the field or record. For example, you might have changed a validation rule without verifying whether the existing data matches the new validation rule. Click Undo to restore the previous value, or enter a new value that meets the validation rule for the field or record.  |
|2117|Microsoft Access has canceled the Paste operation. The text on the Clipboard is too long to paste into the form. For example, you may have pasted too much text into a label or entered too much text in the ColumnWidths property. Paste smaller sections. For labels, you must paste fewer than 2,048 characters.  |
|2118|You must save the current field before you run the Requery action. * If you are running a macro from the Navigation Pane, save the field first, and then run the macro. * If the macro name is the setting of the BeforeUpdate property in a Visual Basic function, set the AfterUpdate property to the name of the macro instead. |
|2119|The Requery action can't be used on the control '_'.  Certain controls, such as labels and rectangles, can't receive the focus; therefore, you can't apply a Requery action to them. |
|2120|To create a form, report or data access page using this wizard, you must first select the table or query on which the form, report or data access page will be based. |
|2121|Microsoft Access can't open the form '_1'. It contains data that Microsoft Access doesn't recognize. Re-create the form or, if you maintain backup copies of your database, retrieve a copy of the form. |
|2122|You can't view a form as a continuous form if it contains a subform, an ActiveX control, a bound chart or a Web browser control. Set the DefaultView property of the form to Single Form, Datasheet, PivotTable, or PivotChart. |
|2123|The control name you entered doesn't follow Microsoft Access object-naming rules. |
|2124|The form name you entered doesn't follow Microsoft Access object-naming rules. |
|2125|The setting for the FontSize property must be from 1 through 127. |
|2126|The setting for the ColumnCount property must be from 1 through 255. |
|2127|The setting for the BoundColumn property can't be greater than the setting for the ColumnCount property. |
|2128|Microsoft Access encountered errors while importing _1. For more detailed error information, see the file '_2'. |
|2129|The setting for the DefaultEditing property must be Allow Edits, Read Only, Data Entry, or Can't Add Records. Enter 1, 2, 3, or 4 for the DefaultEditing property. |
|2130|The settings for the GridX and GridY properties must be from 1 through 64. |
|2131|An expression can't be longer than 2,048 characters. |
|2132|The setting for the DecimalPlaces property must be from 0 through 15, or 255 for Auto (default). |
|2133|You can't place a form (or report) within itself. Select or enter a different form or report to serve as the subform or subreport. |
|2134|The setting for the Width property must be from 0 through 22 inches (55.87 cm). |
|2135|This property is read-only and can't be set. |
|2136|To set this property, open the form or report in Design view. For more information on this property, search the Help index for the name of the property. |
|2137|You can't use Find or Replace now. The fields are not searchable due to one of the following: * The fields are controls (such as buttons or OLE objects). * The fields have no data. * There are no fields to search. |
|2138|You can't search the field for the specified value. Resolve the error given in the previous error message before you attempt to search again. |
|2139|You can't replace the current value of the field with the replacement text. Resolve any errors before making further replacements. |
|2140|Microsoft Access cannot save the change you made to the record in the Replace operation for the reason shown in the previous message. Click Undo or enter a new value in the field. |
|2141|Microsoft Access can't find the text you specified in the Find What box. |
|2142|The FindRecord action requires a Find What argument. You tried to run a macro set to one of the current field's properties, but you left the Find What argument blank. When you click OK, an Action Failed dialog box will display the macro name and the macro's arguments. In the Macro window, enter text or an expression for the Find What argument, and try the Search operation again.  |
|2143|You didn't specify search criteria with a FindRecord action. In the Macro window, insert a FindRecord action before the FindNext action. |
|2144|The setting for the ListRows property must be from 1 through 255. |
|2145|The ColumnWidths property setting must be a value from 0 through 22 inches (55.87 cm) for each column in a list box or a combo box. If there is more than one column, separate the numbers with either a semicolon or the list separator character. List separator characters are defined in the Regional Settings section of Windows Control Panel. |
|2147|You must be in Design view to create or delete controls. |
|2148|The number you used to refer to the form or report section is invalid. Make sure that the number is less than the number of sections in the form or report. |
|2149|The constant you entered for the control type is invalid. For a list of valid constants you can use to create a control, click Help. |
|2150|This type of control can't contain other controls. |
|2151|The parent control can't contain the type of control you selected. For example, you used the CreateControl function to designate an option group as the parent of a text box. |
|2152|You can set group levels for reports only, not for forms. |
|2153|You can't specify more than 10 group levels. |
|2154|You can't call this function when the Group, Sort, and Total Pane is open. |
|2157|The sum of the top margin, the bottom margin, the height of the page header, and the height of the page footer is greater than the length of the page you are printing on. |
|2158|You can use the Print method and the report graphics methods (Circle, Line, PSet, and Scale) only in an event procedure or a macro set to the OnPrint, the OnFormat, or the OnPage event property. |
|2159|There isn't enough memory to initialize the Print method or one of the report graphics methods (Circle, Line, PSet, Scale). Close unneeded programs and try again to print or preview the report. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'. |
|2160|Microsoft Access couldn't create the graphic or text. An error occurred while initializing the Print method or one of the report graphics methods (Circle, Line, PSet, Scale). Close unneeded programs and try again to print or preview the report. For information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'.  |
|2161|The text or expression you entered doesn't match the type of data you are searching for. Redefine the text or expression, or search in a different field. |
|2162|A macro set to one of the current field's properties failed because of an error in a FindRecord action argument. In the Macro window, change the Search As Formatted argument to Yes. If you want the argument setting to remain No, do all of the following: * Select No for the Match Case argument. * Select Yes for the Only Current Field argument. * Make sure you are searching in a bound control. |
|2163|The page number you used as an argument for the GoToPage action or method doesn't exist in this form. |
|2164|You can't disable a control while it has the focus. |
|2165|You can't hide a control that has the focus. |
|2166|You can't lock a control while it has unsaved changes. |
|2167|This property is read-only and can't be modified. |
|2169|You can't save this record at this time. Microsoft Access may have encountered an error while trying to save a record. If you close this object now, the data changes you made will be lost. Do you want to close the database object anyway? |
|2170|There isn't enough memory to retrieve data for the list box. Close unneeded programs. Then close and reopen the active form, and click the list box again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'. |
|2171|You can't have more than seven nested subforms in a main form. Remove the eigth nested subform. |
|2172|You can't use a pass-through query or a non-fixed-column crosstab query as a record source for a subform or subreport. Before you bind the subform or subreport to a crosstab query, set the query's ColumnHeadings property. |
|2173|The control '_' the macro is attempting to search can't be searched. Try one of the following: * Add a GoToControl action before the FindRecord action. * For the FindRecord action, change the Only Current Field action argument from Yes to No. * Change the focus to a searchable control. |
|2174|You can't switch to a different view at this time. Code was executing when you tried to switch views. If you are debugging code, you must end the debugging operation before switching views.  |
|2175|There isn't enough free memory to continue the Search operation. Close unneeded programs. Then try the Search operation again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'. |
|2176|The setting for this property is too long. You can enter up to either 255 or 2,048 characters for this property, depending on the data type. |
|2177|You can't insert a report into a form. A report can be inserted only into a report. |
|2178|You can't add another section now. The maximum total height for all sections in a report, including the section headers, is 200 inches (508 cm). Remove or reduce the height of at least one section, and then add the new section.  |
|2181|You can't sort on a calculated field in a form. You can sort on a calculated field only in a query. Create a calculated field in a query, sort the field, and then base the form on the query. Because the query must execute before the form opens, the form will open more slowly.  |
|2182|You can't sort on this field. |
|2183|Microsoft Access can't create an object of the type requested. You are trying either to create a form from a report that has been saved as text, or to create a report from a saved form. |
|2184|The value you used for the TabIndex property isn't valid. The correct values are from 0 through _. |
|2185|You can't reference a property or method for a control unless the control has the focus. Try one of the following: * Move the focus to the control before you reference the property. In Visual Basic code, use the SetFocus method. In a macro, use the GoToControl action. * Reference or set the property from a macro or event procedure that runs when the GotFocus event for the control occurs. |
|2186|This property isn't available in Design view. Switch to Form view to access this property, or remove the reference to the property. |
|2187|This property is available only in Design view. |
|2188|The object you attempted to load from text has an invalid value for the property '_1' on a _2. |
|2189|The code contains a syntax error, or a Microsoft Access function you need is not available. If the syntax is correct, check the Control Wizards subkey or the Libraries key in the Microsoft Access section of the Windows Registry to verify that the entries you need are listed and available. If the entries are correct, either you must correct the Microsoft Access Utility Add-in, or the file acWzlib or this wizard has been disabled. To reenable this wizard, run Microsoft Access or Microsoft Office Setup again to reinstall Microsoft Access. Before you reinstall Microsoft Access, delete the Windows Registry keys for the Microsoft Access Utility Add-in and acWzlib.  |
|2190|This property has been replaced by a new property; use the new property instead. |
|2191|You can't set the _ property in print preview or after printing has started. Try setting this property in the OnOpen event. |
|2192|The bitmap you specified is not in a device-independent bitmap (.dib) format. You tried to set the PictureData property of a form, report, button, or image control. |
|2193|The left margin, right margin, or both margins are wider than the paper size specified in the Print Setup dialog box. |
|2194|You can't set the PictureData property in Datasheet view. To see the valid settings for this property, search the Help index for 'PictureData property'. |
|2195|The section name you entered doesn't follow Microsoft Access object-naming rules. |
|2196|Microsoft Access can't retrieve the value of this property. The property isn't available from the view in which you're running the macro or Visual Basic code, or Microsoft Access encountered an error while retrieving the value of the property. To see the valid settings for this property, search the Help index for the name of the property. |
|2197|You can't set a subform control's SourceObject property to a zero-length string if you're displaying the main form in Form view. You can set this property to a zero-length string from Design view, Datasheet view, or Print Preview. |
|2200|The number you entered is invalid. |
|2201|There was a problem retrieving printer information for the _1 on _2. The object may have been sent to a printer that is unavailable. |
|2202|There are currently no printers installed on your computer. To use this feature, you must first install a printer in Windows. For more information about how to install a printer, search for "install printer" in Windows Help. |
|2203|The dynamic-link library Commdlg failed: error code '0x_'. The printer driver for the selected printer may be incorrectly installed. For information on selecting another printer or reinstalling this printer from Microsoft Windows, search the Windows Help index for 'printer setup'. |
|2204|The default printer driver isn't set up correctly. For information on setting a default printer, search the Microsoft Windows Help index for 'default printer, setting'. |
|2205|The default printer driver isn't set up correctly. For information on setting a default printer, search the Microsoft Windows Help index for 'default printer, setting'. |
|2206|The page number you entered is invalid. For example, it may be a negative number or an invalid range, such as 6 to 3. |
|2207|Microsoft Access can't print macros. You tried to use the PrintOut action or method, but the active object is a macro. If you want to print an object other than a macro, use the SelectObject action or method to select the desired object before you run the PrintOut action.  |
|2210|Microsoft Access can't print or preview the page because the page size you selected is larger than 22.75 inches. |
|2211|Microsoft Access can't print or preview the Debug window. |
|2212|Microsoft Access couldn't print your object. Make sure that the specified printer is available. For information on setting a default printer, search the Windows Help index for 'default printer, setting'. |
|2213|There was a problem retrieving printer information for this object. The object may have been sent to a printer that is unavailable. |
|2214|There was a problem retrieving information from the printer. New printer has not been set. |
|2215|Microsoft Access cannot print this PivotTable because its _1 exceed(s) 22.75 inches. Reduce the _1 by making changes to the formatting or included data of the PivotTable view, and then try to print again. |
|2220|Microsoft Access can't open the file '_1'. |
|2221|The text is too long to be edited. |
|2222|This control is read-only and can't be modified. |
|2223|The file name '_' is too long. Enter a file name that's 256 characters or less. |
|2225|Microsoft Access couldn't open the Clipboard. The Clipboard isn't responding, probably because another application is using it. Close all other applications and try the operation again. |
|2226|The Clipboard isn't responding, so Microsoft Access can't paste the Clipboard's contents. * Another application may be using the Clipboard. * There may not be enough free memory for the paste operation. Close all other applications, and then copy and paste again. |
|2227|The data on the Clipboard is damaged, so Microsoft Access can't paste it. There may be an error in the Clipboard, or there may not be enough free memory. Try the operation again. |
|2229|Microsoft Access can't start the OLE server. You tried to use a form, report, or datasheet that contains an OLE object, but the OLE server (the application used to create the object) may not be registered properly. Reinstall the OLE server to register it correctly. |
|2234|Microsoft Access can't paste the OLE object. |
|2237|The text you entered isn't an item in the list. Select an item from the list, or enter text that matches one of the listed items. |
|2239|Microsoft Access has detected that this database is in an inconsistent state, and cannot attempt to recover the database because the file is read-only. To allow Access to recover the database, close the database and set the file to read/write, and then open the database. |
|2243|The data in the Clipboard isn't recognizable; Microsoft Access can't paste the OLE object. |
|2244|The file name you specified in the Picture property for a command button or toggle button can't be read. * The file you specified may be corrupted. Restore the file from a backup copy or re-create the file. * The disk where the file is located may be unreadable. |
|2245|The file you specified doesn't contain valid icon data. Specify a valid icon file. |
|2260|An error occurred while sending data to the OLE server (the application used to create the object). * You may have tried to send too much data. If you're creating a chart and the chart is based on a query, modify the query so that it selects less data. If the chart is based on a table, consider basing it on a query instead so that you can limit the data. * You may be using an OLE server that doesn't accept the Clipboard format. * You may not be able to start the OLE server because it's not properly registered. Reinstall it to register it. * Your computer may be low on memory. Close other application windows to free up memory. |
|2262|This value must be a number. |
|2263|The number is too large. |
|2264|Microsoft Access didn't recognize the unit of measurement. Type a valid unit, such as inches (in) or centimeters (cm). |
|2265|You must specify a unit of measurement, such as inches (in) or centimeters (cm). |
|2266|'_' may not be a valid setting for the RowSourceType property, or there was a compile error in the function. For information on valid settings for the RowSourceType property, click Help. |
|2267|There is not enough disk space to create a temporary buffer file for printing. Free up some disk space to make room for the temporary buffer file. |
|2269|Some library databases couldn't be loaded because too many were specified. To change library database references, click References on the Tools menu. |
|2272|The setting for the Update Retry Interval must be from 0 through 1,000 milliseconds. |
|2273|The setting for Update Retries must be from 0 through 10. |
|2274|The database '_' is already open as a library database. |
|2275|The string returned by the builder was too long. The result will be truncated. |
|2276|The custom builder you're using caused an error by changing the focus to a different window while you were using it. Enter a value without using the custom builder. |
|2277|There was a font initialization error. |
|2278|Microsoft Access can't save your changes to this bound OLE object. Either you don't have permission to write to the record in which the object is stored, or the record is locked by another user. Copy the object to the Clipboard (select the object and click Copy on the Edit menu), and click Undo Current Record on the Edit menu. Then open the application you used to create the object, paste the object from the Clipboard, and save it. |
|2279|The value you entered isn't appropriate for the input mask '_' specified for this field. |
|2280|You have added more output formats to the Windows Registry than Microsoft Access can initialize. Some output formats will not be available. Remove those formats that you never or least often use. |
|2281|Output format information is missing. There appears to be a problem with your Microsoft Access installation. Please reinstall Microsoft Access or contact your system administrator or help desk representative. |
|2282|The format in which you are attempting to output the current object is not available. Either you are attempting to output the current object to a format that is not valid for its object type, or the formats that enable you to output data as a Microsoft Excel, rich-text format, MS-DOS text, or HTML file are missing from the Windows Registry. Run Setup to reinstall Microsoft Access or, if you're familiar with the settings in the Registry, try to correct them yourself. For more information on the Registry, click Help. |
|2283|The format specification for '_1' is invalid. You can't save output data to a file in this format until you correct the setting for the format in the Windows Registry. Run Setup to reinstall Microsoft Access or, if you're familiar with the settings in the Registry, try to correct them yourself. For more information on the Registry, click Help. |
|2284|Microsoft Access can't write to the file. * The network may not be working. Wait until the network is working, and then try again. * You may be out of memory. Close one or more Microsoft Access windows, close other applications, and then try again. |
|2285|Microsoft Access can't create the output file. * You may be out of disk space on the destination drive. * The network may not be working. Wait until the network is working, and then try again. * You may be out of memory. Close one or more Microsoft Access windows, close other applications, and then try again. |
|2286|Microsoft Access can't close the file. * The network may not be working. Wait until the network is working, and then try again. * You may be out of memory. Close one or more Microsoft Access windows, close other applications, and then try again. |
|2287|Microsoft Access can't open the mail session. Check your mail application to make sure that it's working properly. |
|2288|Microsoft Access can't load the '_1' format. The setting for this format in the Windows Registry is incorrect. You can't save the output data to a file in this format until you correct the setting in the Registry. Run Setup to reinstall Microsoft Access or, if you're familiar with the settings in the Registry, try to correct them yourself. For more information on the Registry, click Help.  |
|2289|Microsoft Access can't output the module in the requested format. |
|2290|There were too many message recipients; the message was not sent. |
|2291|There are too many message attachments; the message was not sent. |
|2292|The message text is too long, so it was not sent. |
|2293|Microsoft Access can't send this e-mail message. Before attempting to send an e-mail message from Microsoft Access, resolve the problem identified in the previous message, or configure your computer to send and receive e-mail messages. |
|2294|Microsoft Access can't attach the object; the message was not sent. * The network may not be working. Wait until the network is working, and then try again. * You may be out of memory. Close one or more Microsoft Access windows, close other applications, and then try again. |
|2295|Unknown message recipient(s); the message was not sent. |
|2296|The password is invalid; the message wasn't sent. |
|2297|Microsoft Access can't open the mail session. You may be out of memory. Close one or more Microsoft Access windows, close other applications, and then try again. You may also want to check your mail application to ensure that it's working properly. |
|2298|Microsoft Access can't start the wizard, builder, or add-in. * The library database containing the wizard, builder, or add-in may not be installed. Point to Add-ins on the Tools menu, and then click Add-in Manager to see if the library database is installed. * The wizard, builder, or add-in code may not be compiled and Microsoft Access can't compile it. There may be a syntax error in the code. * The key for the add-in in the Windows Registry file may be incorrect. |
|2299|Microsoft Access can't open the Zoom box. The Microsoft Access Utility add-in is missing or was modified. Rerun Microsoft Access or Microsoft Office Setup to reinstall Microsoft Access and the Microsoft Access Utility add-in. |
|2300|Microsoft Access can't output because there are too many controls selected that have different styles, such as color and font. Select fewer controls, and then try again. |
|2301|There are not enough system resources to output the data. Close one or more Microsoft Access windows and close other applications. Then try to output the data again. |
|2302|Microsoft Access can't save the output data to the file you've selected. * The file may be open. If so, close it, and then save the output data to the file again. * If you are using a template, check to make sure the template exists. * If the file isn't open, check to make sure that you have enough free disk space. * Make sure that the file exists on the path specified. * Check to make sure you have permission to write to the specified folder. |
|2303|Microsoft Access can't output data now. * The network may not be working. Wait until the network is working, and then try again. * You may be out of disk space. Free up disk space and try again. |
|2304|Microsoft Access can't save output data to the specified file. Make sure that you have enough free disk space on your destination drive. |
|2305|There are too many columns to output, based on the limitation specified in the output format or by Microsoft Access. |
|2306|There are too many rows to output, based on the limitation specified by the output format or by Microsoft Access. |
|2308|The file '_' already exists. Do you want to replace the existing file? |
|2309|There is an invalid add-in entry for '_1.' There is an error in the Windows Registry for this add-in. Correct the setting and restart Microsoft Access. For information on the Registry, click Help. |
|2311|There isn't enough memory to run the NotInList event procedure. |
|2312|The shortcut '_' must be re-created. The file may be missing, damaged, or in an older format that can't be read. |
|2313|Microsoft Access can't find the shortcut databases '_1' or '_2.' Re-create the shortcut with the correct locations of the databases. |
|2314|Microsoft Access can't find the shortcut database '_1.' Re-create the shortcut with the correct location of the database. |
|2315|The input string is too long. |
|2316|This table or query can't be opened because it has no visible fields. This can result if the table or query has only system fields, and the Show System Objects option is off. To turn on the Show System Objects option, click Options on the Tools menu, click the View tab, and select the System Objects check box.  |
|2317|The database '_1' can't be repaired or isn't a Microsoft Access database file. |
|2320|Microsoft Access can't display the field for which you entered Where in the Total row. Clear the Show check box for that field. If you want this field to appear in the query's results, add it to the design grid twice. For the field that will appear in the query's results, don't specify Where in the Total row, and make sure the Show check box is checked. |
|2321|You can't set criteria before you add a field or expression to the Field row. Either add a field from the field list to the column and enter an expression, or delete the criteria. |
|2322|You can't sort on the asterisk (*). Because the asterisk represents all fields in the underlying table or query, you can't sort on it. Add the asterisk to the query design grid, along with the specific fields you want to sort on. Clear the Show check box for the sorting fields, and then specify a sort order.  |
|2323|You can't specify criteria for the asterisk (*). Because the asterisk represents all the fields in the underlying table or query, you can't specify criteria for it. Add the asterisk to the query design grid, along with the field(s) you want to set criteria for, and then enter criteria for the specific fields. In the query design grid, clear the Show check box for the criteria field(s), before you run the query.  |
|2324|You can't calculate totals on the asterisk (*). Because the asterisk represents all the fields in the table, you can't calculate totals on it. Remove the asterisk from the query design grid. Add the fields you want to use to the design grid, and then select the total you want to calculate for specific fields.  |
|2325|The field name you entered exceeds the 64-character limit of the LinkMasterFields property. When you use the Relationships command (on the Database Tools tab, click Relationships) to define a relationship between the tables underlying a form and subform, Microsoft Access links the form and subform automatically and sets the LinkChildFields and LinkMasterFields properties. |
|2326|You can't specify Group By, Expression, or Where in the Total row for this column. Specify an aggregate function, such as Sum or Count, for the field or expression you designate as the Value in the crosstab query. For more information on aggregate functions, click Help. |
|2327|You must enter Group By in the Total row for a field that has Column Heading in the Crosstab row. The values derived from the field or expression that you designate as the Column Heading are used to group data in the crosstab query. |
|2328|You can't run an update query on the asterisk (*). Because the asterisk represents all the fields in the table, you can't update it. Remove the asterisk from the query design grid. Add the fields you want to update to the design grid.  |
|2329|To create a crosstab query, you must specify one or more Row Heading(s) options, one Column Heading option, and one Value option. |
|2330|Microsoft Access can't represent the join expression _1 in Design view. * One or more fields may have been deleted or renamed. * The name of one or more fields or tables specified in the join expression may be misspelled. * The join may use an operator that isn't supported in Design view, such as > or <. |
|2331|You must enter Group By in the Total row for at least one of the Row Heading options you enter in the Crosstab row. |
|2332|Microsoft Access can't match the fields you added using the asterisk (*) in the append query. Because the asterisk represents all the fields in the underlying table or query, you can't append an asterisk to one field or expression, and you can't append a single field or expression to an asterisk. Append an asterisk to an asterisk (for example, a table to a table), or append specific fields.  |
|2333|You must enter the name of the table you are creating or appending records to. You tried to define a make-table or append query without specifying a destination table. |
|2334|Microsoft Access can't print '_1' because it is an action query. Because action queries don't produce a recordset, you can't print a Datasheet view of them. Note that an exclamation point (!) joined to a query icon in the Navigation Pane marks an action query. To print a Datasheet view of the records that will be selected by the query, display the query in Design view, click the Datasheet button, and then click the Print button.  |
|2335|You must specify the same number of fields when you set the LinkChildFields and LinkMasterFields properties. You entered a different number of fields for one property than you did for the other. If you use the Relationships command (on the Database Tools tab, click Relationships) to define a relationship between the tables underlying the form and subform, Microsoft Access will link the form and subform automatically and then set the LinkChildFields and LinkMasterFields properties. |
|2337|You can't specify criteria on the same field for which you entered Value in the Crosstab row. You tried to display a crosstab query after entering Value in the Crosstab row and criteria in the Criteria row. If you want this field to supply the cross-tabulated values in the crosstab query, delete the entry in the Criteria row. If you want this to be a criteria field, leave the Crosstab row blank.  |
|2338|Microsoft Access truncated the expression you entered. The expression '_1' exceeds the 1,024-character limit for the query design grid. |
|2339|Microsoft Access can't create a temporary link. You reached the limit for the number of links in your database. Microsoft Access needs to create a temporary link in order to import your ODBC table. Remove all unneeded links or tables.  |
|2340|The expression you entered exceeds the 1,024-character limit for the query design grid. |
|2342|A RunSQL action requires an argument consisting of an SQL statement. For example, an action query that appends records starts with INSERT INTO. A data-definition query that creates a table starts with CREATE TABLE. |
|2343|The value you entered exceeds the Alias property's 64-character limit. |
|2344|For the TopValues property in the query property sheet, you must enter an integer greater than zero. |
|2345|For the TopValues property in the query property sheet, you must enter a percentage from 1 through 100. |
|2346|For the TopValues property in the query property sheet, you must enter a number greater than zero. |
|2347|Microsoft Access can't find the file name you entered for the DestinationDB property in an action query's property sheet. You may have misspelled the database file name, or the file may have been deleted or renamed. |
|2348|You can't leave the Alias property blank. |
|2349|For the TopValues property in the query property sheet, you must enter a number smaller than 2,147,483,647. |
|2350|Microsoft Access can't save the query. * The query is a pass-through query and can't be represented as a simple SQL string. Save the query as a named query from the Query Builder. When you close the Query Builder, Microsoft Access will fill the RecordSource or RowSource property with the saved query name. * Make sure the query doesn't have an SQL syntax error. |
|2351|Microsoft Access can't represent an implicit VALUES clause in the query design grid. Edit this in SQL view. |
|2352|You can't modify this query because it has been deleted or renamed by another user. |
|2353|Bad query parameter '_'. |
|2354|This query or table has an expression that is failing to evaluate.|
|2355|You can select up to _ values in a column filter for a multi-valued field. Remove some values, and then try again.|
|2356|You cannot assign a multivalued or Attachment field to the Link Master Fields or Link Child Fields properties. |
|2360|A field name is missing. You have defined a data type or a description for a field without specifying the field name. Enter a name for the field, or delete the row.  |
|2361|Microsoft Access can't save this table. There are no fields in this table. Define at least one field by entering a field name and selecting a data type.  |
|2362|You already have a field named '_’. |
|2363|Microsoft Access allows only one AutoNumber field per table. Use the Number data type for similar fields. |
|2364|Microsoft Access can't open the table in Datasheet view. |
|2366|Microsoft Access was unable to save the field ordering. All other changes were saved successfully. Click the File tab, point to Manage, and then click Compact and Repair Database. |
|2370|Removing or changing the index for this field would require removal of the primary key. If you want to delete the primary key, select that field and click the Primary Key button. |
|2371|Microsoft Access can't create a primary key. Your changes weren't saved. |
|2372|The field name you entered does not follow Microsoft Access object-naming rules. If you pasted the name from another application, try pressing ESC and typing the name again. For more information about naming objects, click Help. |
|2373|The setting for the FieldSize property must be from 0 through 255. |
|2374|You can't create an index or primary key on more than 10 fields. |
|2375|You can't paste beyond the end of a table. You have attempted to paste fields beyond the 255th row in a table in Design view. |
|2376|Microsoft Access can't create a primary key. You have selected too many fields for a multiple-field primary key. |
|2377|Once you enter data in a table, you can't change the data type of any field to AutoNumber, even if you haven't yet added data to that field. Add a new field to the table, and define its data type as AutoNumber. Microsoft Access then enters data in the AutoNumber field automatically, numbering the records consecutively starting with 1. |
|2378|This table is read-only. Use a different name in the Save As dialog box to save your changes. |
|2379|You can't create a primary key on a field of this data type. You can't define a primary key on fields with an OLE Object, Memo, Attachment, or Multi-valued lookup field. |
|2380|Microsoft Access can't create a primary key because no fields have been selected. You have selected a row with no fields defined. Place the insertion point somewhere in the row of the field you want to define as the primary key.  |
|2381|Microsoft Access can't create a primary key because the field doesn't have a name. Name the field, and then define it as a primary key field. |
|2382|You can't switch to Datasheet view and you can't return to Design view. Another user has opened this table or a query, form, or report that is bound to this table. |
|2383|Microsoft Access can't change the data type. There isn't enough disk space or memory. |
|2384|You cannot change one field from an AutoNumber data type and add another AutoNumber field at the same time. Do the following: 1. Delete the AutoNumber field you just added, click the File tab, and then click Save. 2.  Add the new AutoNumber field, and then save the table again. |
|2385|Errors were encountered during the save operation. |
|2386|Microsoft Access was unable to create the table. |
|2387|You can't delete the table '_'; it is participating in one or more relationships. If you want to delete this table, first delete its relationships in the Relationships window. |
|2388|You can't change the primary key. This table is the primary table in one or more relationships. If you want to change or remove the primary key, first delete the relationship in the Relationships window.  |
|2389|You can't delete the field '_’. It is part of one or more relationships. If you want to delete this field, first delete its relationships in the Relationships window.  |
|2390|You can't change the data type or field size of this field; it is part of one or more relationships. If you want to change the data type of this field, first delete its relationships in the Relationships window. |
|2391|Field '_1' doesn't exist in destination table '_2’. Microsoft Access was unable to complete the append operation. The destination table must contain the same fields as the table you are pasting from. |
|2392|You can't set the Unique property of a primary key to No. A primary key, by definition, contains only unique values. If you want to allow nonunique values in this field, remove the primary key definition by setting the Primary property to No.  |
|2393|You can't set the IgnoreNulls property of a primary key to Yes. A primary key, by definition, can't allow null values. If you want null values in this field, remove the primary key definition by setting the Primary property to No.  |
|2394|The index name is invalid. The index name may be too long (over 64 characters) or contain invalid characters. |
|2395|Indexes must have names. |
|2396|Microsoft Access can't create an index or primary key. One or more field names are missing. Enter or select at least one field in the Field Name column for each index you name.  |
|2397|You already have an index named '_’. |
|2398|The primary key has been changed. This table is the primary table in one or more relationships. Changes to the primary key won't be saved. |
|2399|The setting for the FieldSize property must be from 1 through 8000. |
|2400|The row you inserted in the grid exceeds the limit of 255 rows (fields) for a table or 1,000 rows (actions) for a macro. |
|2401|You can't delete the '_1' column at this time.  The '_1' column is part of the primary key for the '_2' table. It is used to identify and store the rows in your table in the database. You cannot delete a primary key while using Datasheet view. To delete the primary key, open the table in Design view and remove the primary key field.|
|2420|The expression you entered has an invalid number. |
|2421|The expression you entered has an invalid date value. |
|2422|The expression you entered has an invalid string. A string can be up to 2048 characters long, including opening and closing quotation marks. |
|2423|The expression you entered has an invalid . (dot) or ! operator or invalid parentheses. You may have entered an invalid identifier or typed parentheses following the Null constant. |
|2424|The expression you entered has a field, control, or property name that Microsoft Access can't find. |
|2425|The expression you entered has a function name that Microsoft Access can't find. |
|2426|The function you entered can't be used in this expression. * You may have used a DoEvents, LBound, UBound, Spc, or Tab function in an expression. * You may have used an aggregate function, such as Count, in a design grid or in a calculated control or field. |
|2427|You entered an expression that has no value. The expression may refer to an object that has no value, such as a form, a report, or a label control. |
|2428|You entered an invalid argument in a domain aggregate function. * A field in the string expression may not be in the domain. * A field specified in the criteria expression may not be in the domain. |
|2429|The In operator you entered requires parentheses. |
|2430|You did not enter the keyword And in the Between...And operator. The correct syntax is as follows: expression [Not] Between value1 And value2|
|2431|The expression you entered contains invalid syntax. You may have entered a comma without a preceding value or identifier. |
|2432|The expression you entered contains invalid syntax, or you need to enclose your text data in quotes. You may have entered an invalid comma or omitted quotation marks. For example, if the Default Value property of a text field is ''Huey, Louie, and Dewey,'' it must be enclosed in quotes if you mean it as a literal text string. This avoids the confusion with the expression ''Huey Louie'' And ''Dewey''. |
|2433|The expression you entered contains invalid syntax. You may have entered an operator, such as the + operator, in an expression without a corresponding operand. |
|2434|The expression you entered contains invalid syntax. You may have entered an operand without an operator. |
|2435|The expression you entered has too many closing parentheses. |
|2436|The expression you entered is missing a closing parenthesis, bracket (]), or vertical bar (_). |
|2437|The expression you entered has invalid vertical bars (_). |
|2438|The expression you entered contains invalid syntax. You omitted an operand or operator, you entered an invalid character or comma, or you entered text without surrounding it in quotation marks. |
|2439|The expression you entered has a function containing the wrong number of arguments. |
|2440|You must enclose IIf function arguments in parentheses. |
|2442|The expression you entered has invalid parentheses. You may have used the parenthesis syntax for an identifier in a query. Use the standard identifier syntax: Forms![Form]![Control]. |
|2443|You can use the Is operator only in an expression with Null or Not Null. |
|2445|The expression you entered is too complex. |
|2446|There isn't enough memory available to perform this calculation. Close unneeded programs, and try again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'. |
|2447|There is an invalid use of the . (dot) or ! operator or invalid parentheses. You may have entered an invalid identifier or typed parentheses following the Null constant. |
|2448|You can't assign a value to this object. * The object may be a control on a read-only form. * The object may be on a form that is open in Design view. * The value may be too large for this field. |
|2449|There is an invalid method in an expression. For example, you may have tried to use the Print method with an object other than Report or Debug. |
|2450|Microsoft Access cannot find the referenced form '_1'. * The form you referenced may be closed or may not exist in this database.* Microsoft Access may have encountered a compile error in a Visual Basic module for the form. |
|2451|The report name '_' you entered is misspelled or refers to a report that isn't open or doesn't exist. |
|2452|The expression you entered has an invalid reference to the Parent property. For example, you may be using the Parent property with a control on a main form or report rather than with a control on a subform or subreport. |
|2453|The control name '_' you entered in your expression is misspelled or refers to a control on a form or report that isn't open or doesn't exist. |
|2454|The object name '_' you entered following the ! operator in the expression is invalid. For example, you may have tried to enter an identifier with two control names separated by the ! operator. |
|2455|You entered an expression that has an invalid reference to the property _. The property may not exist or may not apply to the object you specified. |
|2456|The number you used to refer to the form is invalid. Use the Count property to count the open forms and make sure that the form number is not greater than the number of open forms minus one. |
|2457|The number you used to refer to the report is invalid. Use the Count property to count the open reports and make sure that the report number is not greater than the number of open reports. |
|2458|The control number you specified is greater than the number of controls. Use the Count property to count the controls on the form or report and then check that the control number you cite is within the range of existing controls. |
|2459|You can't refer to the Parent property of a form or report when either is open in Design view. |
|2460|You can't refer to the RecordsetClone property of a form open in Design view. |
|2461|Use a section number, not a string, to refer to a form or report section. |
|2462|The section number you entered is invalid. |
|2463|Use a number, not a string, to refer to a group level. |
|2464|There is no sorting or grouping field or expression defined for the group level number you used. A valid group level number can be from 0 (for the first field or expression you sort or group on) through 9 (for the tenth). Count the group levels in the report starting with zero. |
|2465|Microsoft Access can't find the field '_1' referred to in your expression. You may have misspelled the field name, or the field may have been renamed or deleted. |
|2466|The expression you entered has an invalid reference to the Dynaset property. For example, you may have used the Dynaset property with a form that isn't based on a table or query. |
|2467|The expression you entered refers to an object that is closed or doesn't exist. For example, you may have assigned a form to a Form object variable, closed the form, and then referred to the object variable. |
|2468|The value you entered for the interval, number, or date argument in the function is invalid. Check the argument to make sure that you entered it correctly. For more information on valid argument values, search the Help index for 'DatePart function,' 'DateAdd function,' or 'DateDiff function'. |
|2469|The expression _2 you entered in the form control's ValidationRule property contains the error _1. Microsoft Access can't parse the ValidationRule expression you entered. For example, if you enter the expression =MyFunction() in the ValidationRule property, and the function MyFunction doesn't exist, Microsoft Access displays the following message: Unknown function name in validation rule: 'MyFunction'. To help you create expressions as arguments in Visual Basic, use the Expression Builder. For more information, search the Help index for 'Expression Builder'.  |
|2470|There is a(n) '_' in the form control's ValidationRule property. To help you create expressions as arguments in Visual Basic, use the Expression Builder. For more information, search the Help index for 'Expression Builder'. |
|2471|The expression you entered as a query parameter produced this error: '_'|
|2472|The LinkMasterFields property setting has produced this error: '_'|
|2473|The expression _2 you entered as the event property setting produced the following error: _1. * The expression may not result in the name of a macro, the name of a user-defined function, or [Event Procedure]. * There may have been an error evaluating the function, event, or macro. |
|2474|The expression you entered requires the control to be in the active window. Try one of the following: * Open or select a form or report containing the control. * Create a new control in the active window, and try the operation again. |
|2475|You entered an expression that requires a form to be the active window. |
|2476|You entered an expression that requires a report to be the active window. |
|2477|You entered an invalid objecttype value '_' in an If TypeOf object Is objecttype condition of an If...Then...Else statement. The objecttype can be any one of the following: BoundObjectFrame, CheckBox, ComboBox, CommandButton, Label, Line, ListBox, UnboundObjectFrame, OptionButton, OptionGroup, PageBreak, Rectangle, Subform, Subreport, TextBox, ToggleButton, ImageControl, or OLEControl. |
|2478|Microsoft Access doesn't allow you to use this method in the current view. Most methods, including the SetFocus and Requery methods, can't be used in form or report Design view. |
|2479|The event procedure '_' can't be a Function procedure; it must be a Sub procedure. If you want to run a Function procedure when an event occurs, try one of the following: * Set the event property to the name of a macro containing a RunCode action that runs the Function procedure. * Set the event property to =FunctionName(). |
|2480|You referred to a property by a numeric argument that isn't one of the property numbers in the collection. Check the property numbers in the collection. |
|2481|You can't set a value while a document is in Print Preview. |
|2482|Microsoft Access cannot find the name '_1' you entered in the expression. You may have specified a control that wasn't on the current object without specifying the correct form or report context. To refer to a control on another form or report, precede the control name with the name of a collection. This is usually either Forms or Reports, and the name of the form or report to which the control belongs. For example, Forms![Products]![Units In Stock].  |
|2483|You can't move to a previous control when only one control has had the focus. Use the PreviousControl property only after you've moved the focus to a second control. |
|2484|There is no active datasheet. |
|2485|Microsoft Access cannot find the object '_1’. If '_1' is a new macro or macro group, make sure you have saved it and that you have typed its name correctly. |
|2486|You can't carry out this action at the present time. You tried to run a macro or used the DoCmd object in Visual Basic to carry out an action. However, Microsoft Access is performing another activity that prevents this action from being carried out now. For example, no actions on a form can be carried out while Microsoft Access is repainting a control or calculating an expression. Carry out the action later.  |
|2487|The Object Type argument for the action or method is blank or invalid. * For a Close, GoToRecord, SearchForRecord or RepaintObject action, enter values for both arguments, or leave both blank to perform the action on the active object. * For a DeleteObject, Rename, or CopyObject action, enter values for both arguments, or leave both blank to perform the action on the object currently selected in the Navigation Pane. * For a SendObject or OutputTo action, enter values for both arguments, or leave the Object Name argument blank if you want the action performed on the active object of the specified object type. * If you're using a method with the DoCmd object, use an intrinsic constant that equates to a valid object type or the corresponding numeric value for the argument name. |
|2488|You can't use the ApplyFilter action on this window. * You tried to use the ApplyFilter action or method, but you didn't apply the filter to a table, query, form, or report. * You may have applied the filter to a form, but the form wasn't open in Form or Datasheet view. * You may have applied the filter to a report but didn't use the ApplyFilter action in a macro specified by the OnOpen property setting. Use the SelectObject action or method to select the table, query, form, or report before applying the filter.  |
|2489|The object '_' isn't open. * The macro you are running (directly or indirectly) contains a GoToRecord, RepaintObject, or SelectObject action, but the Object Name argument names an object that is closed. * The objectname argument for the GoToRecord, RepaintObject, or SelectObject method names an object that is closed. Use one of the Open actions or methods to open the object so that you can carry out the desired action.  |
|2491|The action or method is invalid because the form or report isn't bound to a table or query. You tried to use the ApplyFilter or SearchForRecord action or method. However, the form or report you applied the filter to is not based on a table or query, so the form or report doesn't have any records to apply a filter to. Use the SelectObject action or method to select the desired form or report before you run the ApplyFilter action. To base a form or report on a table or query, open the form or report in Design view, and enter the table or query name in the RecordSource property.  |
|2492|Microsoft Access can't find the macro '_2' in the macro group '_1’. You used the macrogroupname.macroname syntax to specify a macro. You then tried to run the macro (directly or indirectly), or you used the RunMacro method to run the macro. However, the macro you specified isn't in this macro group. Create the macro in the macro group, specify the correct macro group, or specify the correct macro name.  |
|2493|This action requires an Object Name argument. |
|2494|The action or method requires a Form Name argument. You tried to use the OpenForm action or method, but you left the Form Name argument blank. In the Form Name argument, enter the name of a form in the current database.  |
|2495|The action or method requires a Table Name argument. You tried to use the OpenTable, TransferSpreadsheet, or TransferText action or method, but you left the Table Name argument blank. In the Table Name argument, enter the name of a table that is in the current database.  |
|2496|The action or method requires a Query Name argument. You tried to use the OpenQuery action or method, but you left the Query Name argument blank. In the Query Name argument, enter a query name.  |
|2497|The action or method requires a Report Name argument. You tried to use the OpenReport action or method, but you left the Report Name argument blank. In the Report Name argument, enter the name of a report.  |
|2498|An expression you entered is the wrong data type for one of the arguments. You tried to run a macro or use a method to carry out an action, but an expression evaluated to the wrong data type. For example, for the Close method you specified a string for the Object Type argument, but this argument can be set only to certain intrinsic constants or their numeric equivalents. |
|2499|You can't use the GoToRecord or SearchForRecord action or method on an object in Design view. Try one of the following: * Switch to Form or Datasheet view for a form. * Switch to Datasheet view for a query or table. * If you are running a macro or Visual Basic procedure containing an action that opens the object, set the View argument to the correct view before you carry out the GoToRecord action. |
|2500|You must enter a number greater than zero for a Repeat Count argument. You tried to use the RunMacro action or method, but you entered a value less than zero (or an expression that evaluates to less than zero) in the Repeat Count argument. To run the macro once, leave this argument blank.  |
|2501|The _ action was canceled. You used a method of the DoCmd object to carry out an action in Visual Basic, but then clicked Cancel in a dialog box. For example, you used the Close method to close a changed form, then clicked Cancel in the dialog box that asks if you want to save the changes you made to the form. |
|2502|The action or method requires a Macro Name argument. * You tried to use the RunMacro action or method, but you left the Macro Name argument blank. * Microsoft Access tried to create a custom menu bar for a form or report, but the Menu Macro Name argument of the AddMenu action is blank. In the Menu Macro Name argument, enter the name of a macro or macro group that is in the current database.  |
|2503|You can't use this action with the DoCmd object. For a list of the actions that the DoCmd object doesn't support and some alternatives to using these actions, click Help. Any actions that aren't in this list can be used with the DoCmd object. |
|2504|The action or method requires at least _ argument(s). You tried to run a macro containing an action or used a method or action with the DoCmd object, but you didn't set the required number of arguments. For example, if you use the MoveSize action, you must set at least one of the four arguments. |
|2505|An expression in argument _ has an invalid value. You tried to run a macro or used the DoCmd object in Visual Basic. The argument number above is the position of the argument as it appears in the Macro window, the Action Failed dialog box, or the Object Browser (if you're using the DoCmd object). Try one of the following: * Select a setting from the drop-down list box in each argument. * Use an intrinsic constant equating to a valid object type. * Substitute the correct corresponding expression.  |
|2506|A value you entered for the Transfer Type argument is invalid. An expression in the Transfer Type argument doesn't evaluate to a valid numeric value. Valid values for the Transfer Type argument are as follows: * 0, 1, and 2 for the TransferDatabase action. * 0, 1, and 2 for the TransferSpreadsheet action. * 0 through 6 for the TransferText action.  |
|2507|The _ type isn't an installed database type or doesn't support the operation you chose. You used the TransferDatabase method, but an expression in the databasetype argument doesn't evaluate to a valid database type for importing, exporting, or linking. For information on valid database types, click Help. |
|2508|A value you entered for the spreadsheettype argument is invalid. You used the TransferSpreadsheet method, and an expression in the spreadsheettype argument doesn't evaluate to a valid numeric value. Valid values are 0, 2, 3, 4, 5, 6, 7, and 8. Note that 1 is an invalid value; you can't import or export to a Lotus .wks format file.  |
|2509|The setting for the Range argument can't be longer than 255 characters. |
|2510|The expression you entered in the Specification Name argument exceeds the 64-character limit. Select one of the existing specification names from the argument list box when you use the TransferText action in a macro, or enter a name in Visual Basic that follows Microsoft Access object-naming rules. |
|2511|The action or method requires a Specification Name argument. You tried to use the TransferText action or method and you specified a Transfer Type argument but left the Specification Name argument blank. In the Specification Name argument, enter an existing specification name from the argument list box.  |
|2512|Microsoft Access cannot parse the expression: '_1'. Click OK to return to the action argument or conditional expression where this expression appears, and then correct the syntax. |
|2513|The Macro Name argument can't be longer than 64 characters according to Microsoft Access object-naming rules. |
|2514|The action or method requires a Control Name argument. You tried to use the GoToControl action or method, but you left the control name blank. In the Control Name argument, enter a control or field name from the active form or datasheet.  |
|2515|Microsoft Access cannot open the macro '_1' because it was saved using a different version of Microsoft Access. Re-create the macro in the current version of Microsoft Access. |
|2516|Microsoft Access cannot find the module '_1’. You tried to use the OpenModule action or method, but Microsoft Access can't find the module you specified in the Module Name argument. Enter a valid module name from the current database.  |
|2517|Microsoft Access cannot find the procedure '_1’. * You may have used the Run method in Visual Basic but entered an invalid procedure name, or you used the Run method without first opening a database. * You tried to use the OpenModule action or method, but you used an invalid procedure name. |
|2520|The action or method requires a Module or Procedure Name argument. You tried to use the OpenModule action or method, but you didn't enter a name in either the Module Name or the Procedure Name argument in the Macro window. Enter a valid name in one of these arguments.  |
|2521|You have specified a Transfer Type that doesn't support the HTML Table Name argument. Leave the HTML Table Name argument blank unless you are using the Import HTML or Link HTML Transfer Types. |
|2522|The action or method requires a File Name argument. You tried to use the TransferSpreadsheet or TransferText action or method. In the File Name argument, enter a file name.  |
|2523|The value you entered for the show argument is invalid. You used the ShowToolbar method. Valid values for this argument are acToolbarYes, acToolbarWhereApprop, and acToolbarNo, or the corresponding numeric values 0, 1, and 2.  |
|2524|Microsoft Access can't invoke the application using the RunApp action. The path to the application is invalid, or a component of the application is missing. Check the path in Windows Explorer or File Manager.  |
|2525|Macros can only be called 19 times. Your macro contains one or more RunMacro actions that calls a macro more than 19 times. Use an If block to stop the macro after it has been run 19 times.  |
|2526|The SendKeys action requires the Microsoft Access Utility Add-in to be loaded. Rerun Microsoft Access or Microsoft Office Setup to reinstall Microsoft Access and the Microsoft Access Utility Add-in. |
|2527|Lotus .wks file formats aren't supported in the current version of Microsoft Access. Convert your .wks file to a more recent format, such as .wk1. |
|2528|The RunCommand macro action argument is missing, or you entered an invalid command ID for the RunCommand method. |
|2529|The Toolbar argument can't be longer than 64 characters. |
|2530|The SelectObject method can't be used on a report that is currently printing. |
|2531|Your HTML file does not contain any tabular data that Microsoft Access can import. . |
|2532|Microsoft Access cannot find the macro or sub procedure '_1’. The specified macro, macro group, or sub procedure doesn't exist.  Note that when you enter the macrogroupname.macroname syntax in an argument, you must specify the name the macro's macro group was last saved under. Also, ensure that the referenced macro has been saved, or that the referenced sub procedure expects 0 arguments.  |
|2533|The ApplyFilter action requires that either the Filter Name or Where Condition argument is set. You tried to run a macro containing an ApplyFilter action, but you didn't set the required arguments. |
|2534|The action or method requires a data access page Name argument. You tried to use the OpenDataAccessPage action or method, but you left the data access page Name argument blank. In the data access page Name argument, enter the name of a data access page in the current database.  |
|2535|The ApplyFilter action contains a Filter Name that cannot be applied. The filter name is not a valid argument in the ApplyFilter action in Client Server. |
|2537|The feature '_' is not available while the database is opened in disabled mode. |
|2538|The '_' macro action cannot be run in disabled mode. |
|2539|The Name argument of the SetLocalVar macro action is a reserved name. Please change the name argument of the SetLocalVar action.|
|2540|The file '_1' you tried to replace is a Microsoft Access system file that is being used and can't be replaced or deleted. |
|2541|The contents of the Clipboard have been deleted and can't be pasted. Some applications do not put large objects on the Clipboard. Instead, they put a pointer to the object on the Clipboard. The pointer may vanish before the paste happens. |
|2542|Specify the database name in the command line so that Microsoft Access can find the macro. |
|2543|You can't paste a database object onto itself. |
|2544|Microsoft Access cannot find the _1 you referenced in the Object Name argument. The macro you tried to run includes a SelectObject action with an invalid name for the Object Name argument. In the Navigation Pane, verify the name of the object you want the macro to select. Then open the macro in the Macro window and enter the correct name for the Object Name argument.  |
|2545|The CopyObject action requires you to specify a different destination database or a new name to copy from the current database. The macro you are running includes a CopyObject action. Open the macro in the Macro window, and select the CopyObject action. Enter a destination database or a new name in the appropriate argument box.  |
|2546|Select a database object in the Navigation Pane before you run the macro containing the _ action. |
|2547|The database '_' you tried to delete and replace is read-only and can't be deleted or replaced. Enter a different name for the new database. |
|2548|Microsoft Access can't run the Security Wizard because this database is open in exclusive mode. Do you want Microsoft Access to open the database in shared mode and run the Security Wizard? |
|2549|Microsoft Access can't delete _1 after compacting it. The compacted database has been named _2. If you compact a database using the same name, Microsoft Access creates a new compacted database and then deletes the original database. In this case, however, the original database wasn't deleted because it is read-only. If you can, remove the read-only status, delete the original database, and then rename the new database using the original name. If you can't remove the read-only status, inform your workgroup administrator.  |
|2550|Microsoft Access can't delete _1 after encoding it. The encoded database has been named _2. If you encode a database using the same name, Microsoft Access creates a new encoded database, and then deletes the original database. In this case, however, the original database can't be deleted because it is read-only. If you can, remove the read-only status, delete the original database, and then rename the new database using the original name. If you can't remove the read-only status, inform your workgroup administrator.  |
|2551|Microsoft Access can't delete _1 after decoding it. The decoded database has been named _2. If you decode a database using the same name, Microsoft Access creates a new decoded database, and then deletes the original database. In this case, however, the original database can't be deleted because it is read-only. If you can, remove the read-only status, delete the original database, and then rename the new database using the original name. If you can't remove the read-only status, inform your workgroup administrator.  |
|2552|You can't encode a database that you didn't create or don't own. See the owner of the database or your workgroup administrator. |
|2553|You can't decode a database that you didn't create or don't own. See the owner of the database or your workgroup administrator. |
|2554|Can't find the database you specified, or you didn't specify a database at all. Specify a valid database name in the command line, and include a path if necessary. |
|2556|Microsoft Access can't run the Security Wizard because the database uses a password. Remove the database password by clicking Unset Database Password in the Database Tools group on the Database Tools tab. |
|2557|The database you tried to convert was either created in or was already converted to the requested version of Microsoft Access. |
|2559|Microsoft Access was unable to refresh the linked table '_1' in database '_2' during conversion. Try to refresh the links manually by using the Linked Table Manager command in the Database Tools group on the Database Tools tab.|
|2560|Microsoft Access is unable to load the Database Properties. |
|2561|Microsoft Access can't display the Database Properties dialog box. |
|2562|Microsoft Access is unable to save the Database Properties. |
|2564|You can't hide the document '_' while it is open. Close the database object first, and then hide it. |
|2565|You can't unhide the database object '_' while it is open. Close the database object first, and then unhide it. |
|2566|Microsoft Access is unable to set the application's icon to the file '_1'. Make sure the file is a valid icon (.ico) file. If you're using Microsoft Windows, you can also use .bmp files. |
|2567|Microsoft Access can't open or convert this previous version database. The database was created in an earlier version of Microsoft Access. You don't have appropriate security permissions to open or convert databases created in earlier versions. |
|2568|Microsoft Access can't undo this operation. An object with the same name already exists. Another user might have created an object named '_' after you had performed this operation on an object with the same name. |
|2571|You cannot modify objects created in an earlier version of Microsoft Access. To convert this database to the current version of Microsoft Access, close the database, click the File tabn, and then click Convert. |
|2572|This database is in an unexpected state and Microsoft Access cannot open it. This database has been converted from a prior version of Microsoft Access by using the DAO CompactDatabase method instead of the Convert Database command (click the File tab and then click Convert). Converting by using the DAO CompactDatabase method has left the database in a partially converted state. If you have a copy of the database in its original format, click the File tab and then click Convert to convert it. If the original database is no longer available, create a new database and import your tables and queries to preserve your data and try again. Your other database objects cannot be recovered.  |
|2573|This database is a replica created in a different version of Access.  You can only convert this replica by synchronizing with its Design Master. Convert the Design Master of this replica set then synchronize the replica with the Design Master.|
|2574|You can't create another Microsoft Access database with the same name and location as an existing database. You carried out the Make MDE File command, but tried to give the new database the same extension as the old one. Accept the default .mde extension for your new MDE database.  |
|2575|You can't create a Microsoft Access MDE database from a database replica. |
|2576|This database is a Microsoft Access 7.0/8.0/9.0 Design Master/Replica. If you click OK, the database you selected will be renamed to _1 and then converted to _2. Everyone using a replica of this database will have to upgrade to Microsoft Access 2000 after the next synchronization. |
|2577|The database _ is already open. Close the database before carrying out the Make MDE File command. |
|2578|Microsoft Access was unable to create the .accde, .mde, or .ade file. |
|2579|Local forms, reports, macros, and modules in this replica will not be converted.  To retain these objects, please be sure to import them into the Design Master from the original replica.|
|2580|The record source '_' specified on this form or report does not exist.  The name of the recordsource may be misspelled, the recordsource was deleted or renamed, or the recordsource exists in a different database. In the Form or Report's Design view or Layout view, display the property sheet by clicking the Properties button, and then set the RecordSource property to an existing table or query.|
|2581|You must define a sort field or expression for the group header or footer in the report you tried to preview or print. |
|2582|You cannot set the GroupInterval property to 0 when the GroupOn property is set to Interval. Click the Sorting and Grouping Design tab and try one of the following: * Change the GroupInterval property setting to a number higher than 0. * Change the GroupOn property setting to Each Value. |
|2583|The ApplyFilter action or method can be carried out only from an Open macro or Open event procedure. * You may have tried to run a macro or procedure containing the ApplyFilter action or method from a report property other than the OnOpen property. * You may have tried to run a macro or event procedure on a report that is already open. To use the ApplyFilter action in a report, set the OnOpen property to the name of the macro, close the report, and then reopen it.  |
|2584|You can't use aggregate functions in a page header or footer. The page header or footer of the report you tried to preview contains a calculated control with an aggregate function in its expression. If you want to show the result of an aggregate function in a page header or footer, create a hidden calculated control in an appropriate section of the report. Then create an unbound text box in the page header or footer. If you are running a macro, use the SetValue action to set the unbound text box value to the value in the hidden control.  |
|2585|This action can't be carried out while processing a form or report event. A macro specified as the OnOpen, OnLoad, OnClose, OnFormat, OnRetreat, OnPage, or OnPrint property setting contains an invalid action for the property. When you click OK, an Action Failed dialog box will display the name of the macro that failed and its arguments.  |
|2586|Microsoft Access changed the MoveLayout and NextRecord properties to True from False. The macro or Visual Basic function run by the OnFormat property of one of the sections of the report set both the MoveLayout and NextRecord properties to False. Having both properties set to False can make the report print continuously. Revise the macro or function so that it sets these properties to the values you want.  |
|2587|Microsoft Access can't complete the Output operation. The Visual Basic code you entered contains a syntax error or the Output procedures are not available. Make sure there isn't a syntax error in your code. If the syntax is correct, run Setup to reinstall Microsoft Access. If you want to preserve your security or custom settings, back up the Microsoft Access workgroup information file. For information on backing up files, search the Microsoft Windows Help index for 'backing up files'.   |
|2588|You must select a form to save as a report. |
|2589|The expression '_' is invalid. Aggregate functions are only allowed on output fields of the Record Source. |
|2590|The Var and VarP aggregate functions are not supported in an Access project. |
|2591|You can't change printer properties in the OnOpen event of a report. |
|2593|This feature is not available in an MDB or ACCDB. |
|2594|You cannot Filter By Form when form record source is a recordset object. |
|2595|Microsoft Access cannot set this property when DefaultSize property is set to True. |
|2596|Printer object is not available on subforms and subreports. |
|2597|Unable to bind the report to the specified recordset because the shape does not match the sorting and grouping specified on the report. |
|2599|Report view is not available for this report. |
|2600|Verify the new password by retyping it in the Verify box and clicking OK. |
|2601|You don't have permission to read '_’. To read this object, you must have Read Design permission for it. For more information on permissions and who can set them, click Help. |
|2602|You don't have permission to modify '_’. To modify this object, you must have Modify Design permission for it. If the object is a table, you must also have Delete Data and Update Data permissions for it. For more information on permissions and who can set them, click Help. |
|2603|You don't have permission to run '_’. To run this object, you must have Open/Run permission for it. For more information on permissions and who can set them, click Help. |
|2604|You can't view this object's permissions. To view or change permissions for this object, you must have Administer permission for it. For more information on permissions and who can set them, click Help. |
|2605|You can't remove this user account from group '_1’. * You may have tried to remove a user account from the default Users group. Microsoft Access automatically adds all users to the default Users group. To remove a user account from the Users group, you must first delete the account. * You may have tried to remove all users from the Admins group. There must be at least one user in the Admins group. |
|2606|The object type is invalid. |
|2607|You don't have permission to cut '_’. To cut this object, you must have Modify Design permission for it. If the object is a table, you must also have Delete Data permission for it. For more information on permissions and who can set them, click Help. |
|2608|You don't have permission to copy '_’. To copy this object, you must have Read Design permission for it. If the object is a table, you must also have Read Data permission for it. For more information on permissions and who can set them, click Help. |
|2609|You don't have permission to delete '_’. To delete this object, you must have Modify Design permission for it. If the object is a table, you must also have Delete Data permission for it. For more information on permissions and who can set them, click Help. |
|2610|You must enter a personal identifier (PID) consisting of at least 4 and no more than 20 characters and digits. Microsoft Access uses the combination of the user or group name and the PID to identify the user or group. Note that Microsoft Access hides the PID after you create it, so make sure to write down the exact user or group account name and the PID entries. If you ever have to re-create the account, you must supply the same name and PID entries. |
|2611|Microsoft Access can't find the workgroup file '_1.' Would you like to use the default workgroup file? |
|2612|The account name is invalid. For information about naming conventions, click Help. |
|2613|You don't have permission to rename '_’. To rename a database object, you must have Modify Design permission for the object. For more information on permissions and who can set them, click Help. |
|2614|You don't have permission to insert this form into another form. To insert a form into another form as a subform, you must have Read Design permission for the form being inserted. For more information on permissions and who can set them, click Help. |
|2615|You don't have permission to change the owner of '_’. To change the owner of a database object, you must have Administer permission for it. For more information on permissions and who can set them, click Help. |
|2616|You can't change permissions for '_’. To change permissions for this object, you must have Administer permission for it. For more information on permissions and who can set them, click Help. |
|2617|You don't have permission to import, export, or link to '_’. To import, export, or link to this object, you must have Read Design and Read Data permissions for it. For more information on permissions and who can set them, click Help. |
|2618|You must have the database open for exclusive use to set or remove the database password. To open the database exclusively, close the database, and then reopen it by clicking the File tab and using the Open command. In the Open dialog box, click the arrow next to the Open button, and then select Open Exclusive. |
|2619|You can't change permissions for '_' in a replica. Permissions may only be changed in the Design Master for the replica set. |
|2620|The password you entered in the Old Password box is incorrect. Please enter the correct password for this account. |
|2621|That password isn't valid. You may have used a semicolon. |
|2622|You cannot save '_' because it is read-only. To save, switch to Design View, click the File tab, point to Save As, and enter a new name. |
|2624|An error has occurred while changing workgroup database. |
|2625|Workgroup Administrator couldn't create the workgroup information file. Make sure that you have specified a valid path and file name, that you have adequate permissions to create the file, and that you have enough disk space on the destination drive. (_)|
|2626|Reserved error (_); there is no message for this error. |
|2627|There's not enough disk space. |
|2628|One of your parameters is invalid. |
|2629|Could not open workgroup file. This is a directory. |
|2630|The specified path is invalid. |
|2631|The specified path is too long. |
|2632|Change Workgroup cannot proceed without your Name, PIN, and a path to the new Workgroup Information File.|
|2633|Microsoft Access cannot change the password for the logon account '_1' because the current connection is using Microsoft Windows NT integrated security. |
|2634|The new password doesn't match the verify password value. |
|2635|Microsoft Access is unable to change the password because the old password doesn't match the password of the currently logged in user.|
|2636|Workgroup file already exists.|
|2637|Unable to start SQL Server service. To restart the SQL Server service, double-click the SQL Server System Manager icon in the system tray and click Start/Continue. When the service is started, in Microsoft Access, click the File tab, point to Server Tasks, click Connection, and then click OK. |
|2638|Unable to start SQL Server service. To restart the SQL Server service, double-click the SQL Server System Manager icon in the system tray and click Start/Continue. If the service fails to start, go to the Services console and verify that the MSSQLServer service Log On information is correct. When the service is started, in Microsoft Access, click the File tab, point to Server Tasks, click Connection, and then click OK. |
|2639|Microsoft Access cannot open _1 due to security restrictions. Security settings restrict access to the file because it is not digitally signed. |
|2646|Microsoft Access can't create this relationship and enforce referential integrity. Data in the table '_1' violates referential integrity rules. For example, there may be records relating to an employee in the related table, but no record for the employee in the primary table. Edit the data so that records in the primary table exist for all related records. If you want to create the relationship without following the rules of referential integrity, clear the Enforce Referential Integrity check box.  |
|2649|Microsoft Access can't enforce referential integrity for this relationship. Make sure the fields you drag are primary key fields or uniquely indexed and that the unique index or primary key is correctly set. If you want to create the relationship without following the rules of referential integrity, clear the Enforce Referential Integrity check box. |
|2650|Microsoft Access can't create this relationship and enforce referential integrity. * The fields you chose may have different data types. * The fields may have the Number data type but not the same FieldSize property setting. Try one of the following: * Select fields with the same data type. * Open the tables in Design view, and change the data types and field sizes so that the fields match. If you want to create the relationship without following the rules of referential integrity, clear the Enforce Referential Integrity check box.  |
|2651|You can't create a relationship between fields with the Memo, OLE Object, Yes/No, or Hyperlink data type. You tried to enforce referential integrity for a relationship, but one or more of the fields you chose have the Memo, OLE Object, Yes/No, or Hyperlink data type. Select fields in the grid that don't have these data types, or open the tables in Design view and change data types.  |
|2652|You can't delete a relationship inherited from a linked database. |
|2680|The form or report includes more OLE objects than Microsoft Access can display at one time. Delete some of the bound or unbound object frames. |
|2683|There is no object in this control. |
|2684|The OLE object is empty. You can't edit a bound object frame if the field in the underlying table doesn't contain an OLE object. Right-click the field, click Insert Object, and use the dialog box to locate and add the object to the field.  |
|2685|The object doesn't have an OLE object data type. The bound object frame containing the object you tried to edit isn't bound to a field with the OLE object data type. If you want to display an OLE object, set the ControlSource property for the bound object frame to a field with the OLE object data type. Or use a different control, such as a text box, to display the data.  |
|2686|Microsoft Access is unable to save the _1 object. Your computer ran out of disk space while Microsoft Access was saving the OLE object. For information on freeing disk space, search the Microsoft Windows Help index for 'disk space, freeing'. |
|2690|A system resource necessary for displaying the _ object isn't available. Your computer may be low on memory. Close unneeded programs, and try the operation again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'.  |
|2691|Microsoft Access can't communicate with the OLE server. The OLE server may not be registered. To register the OLE server, reinstall it.  |
|2694|The Clipboard isn't available. The Clipboard may be in use by another application, or your computer may be low on memory. If your computer is low on memory, close unneeded programs, and then try the operation again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'.  |
|2695|Microsoft Access is unable to display the converted _1 object. Delete the object in the bound object frame, and then re-create it. |
|2696|Microsoft Access can't read the OLE object. Delete the object in the bound object frame, and then re-create it. |
|2697|There was a problem loading the _ object. The object you tried to create or edit is not a valid OLE object. Re-create the object, and then embed or link it again.  |
|2698|The _ object you tried to create or edit is too large to save. * Your database may not contain enough space for the object. * Your computer may be out of disk space. For information on freeing disk space, search the Microsoft Windows Help index for 'disk space, freeing'. |
|2699|The connection with the OLE server was lost, or the OLE server encountered an error while you were using it. Restart the OLE server, and then try the operation again. |
|2700|Microsoft Access can't find an OLE server or a dynamic-link library (DLL) required for the OLE operation. The OLE server or DLL may not be registered. To register the OLE server or DLL, reinstall it.  |
|2701|The OLE server for the OLE object you tried to create is already open. Switch to the OLE server window and close it. Then try to create or edit the OLE object again. |
|2702|The _ object isn't registered. The object may be calling an application that isn't installed. To register the application, reinstall it.  |
|2703|Microsoft Access can't read the _1 object because communication was interrupted. If the OLE server application is located on a network server, make sure your computer is connected to it. |
|2704|The _ object you tried to edit doesn't have any displayable information. |
|2707|Microsoft Access can't open the file containing the OLE object. * You may have specified an invalid file name or an invalid unit of data (such as a range of cells from a worksheet) within the file for the OLE object. * The file you specified may not be available because it's locked by another user or you don't have permission to use it. Try one of the following: * Make sure the file is available and that you used the correct file name. * Check the OLE server's documentation for information about the syntax to use when specifying an OLE object's data.  |
|2711|The file name argument in the GetObject function of the Visual Basic procedure you ran is invalid. * You may not have entered, or may have misspelled, the file name. * The unit of data (such as a range of cells from a worksheet) may not be valid. Try one of the following: * Make sure the file is installed on your computer and that you used the correct file name. * Check the OLE server's documentation for information about the syntax to use when specifying an OLE object's data.  |
|2713|A problem occurred when Microsoft Access tried to access the _1 object. * You may have specified an invalid file name or an invalid unit of data (such as a range of cells from a worksheet) within the file for the OLE object. * The file you specified may not be available because it's locked by another user or you don't have permission to use it. Try one of the following: * Make sure that the file is installed on your computer and that you used the correct file name. * Check the OLE server's documentation for information about the syntax to use when specifying an OLE object's data.  |
|2714|The _ object doesn't support verbs that can be performed on an OLE object, such as play or edit. Check the OLE server's documentation for information on the verbs the OLE object supports, or use the ObjectVerbs property or the ObjectVerbsCount property to find the verbs supported by an OLE object. |
|2715|The index for the Action or the Verb property for the _ object is invalid. The setting you entered may be a negative number or may be too large. |
|2717|The _ object has no information that can be displayed. You tried to perform an operation on a bound or unbound object frame containing an OLE object, but the OLE object is empty. Right-click the frame, click Insert Object, and then use the dialog box to locate and either add or link to an object from a file that is not empty.  |
|2719|A problem occurred while accessing the _1 object. * The OLE server may not be available because it's on a network server and you lost the connection. Try re-establishing the connection. * The OLE object may be stored in a linked file, but the file isn't available. Activate the OLE server outside of Microsoft Access, and then open the file containing the OLE object to verify that it still exists and can be accessed. |
|2723|The _ object doesn't support the attempted operation. The OLE object was changed to a picture, or the link to the object was broken. If you want to perform the operation, delete the OLE object, and then embed or link it again.  |
|2724|One or more dynamic-link libraries required for using OLE objects is an incorrect version. Run Setup to reinstall Microsoft Access. If you want to preserve your security or custom settings, back up the Microsoft Access workgroup information file. For information on backing up files, search the Microsoft Windows Help index for 'backing up files'. |
|2725|The OLE server isn't registered. To register the OLE server, reinstall it. |
|2726|Microsoft Access can't perform the OLE operation because it was unable to read the Windows Registry where the OLE server is registered. Reinstall the OLE server, and then try the operation again. If problems continue, reinstall Microsoft Windows and the other applications on your computer. If you reinstall Microsoft Access, you may want to back up your Microsoft Access workgroup information file first to preserve any custom settings. For information on backing up files, search the Microsoft Windows Help index for 'backing up files'. For information on the Windows Registry, search the Microsoft Windows Help index for 'registry'. |
|2727|Microsoft Access can't perform the OLE operation because it was unable to write to the Windows Registry where the OLE server is registered. Reinstall the OLE server, and then try the operation again. If problems continue, reinstall Microsoft Windows and the other applications on your computer. If you reinstall Microsoft Access, you may want to back up your Microsoft Access workgroup information file first to preserve any custom settings. For information on backing up files, search the Microsoft Windows Help index for 'backing up files'. For information on the Windows Registry, search the Microsoft Windows Help index for 'registry'. |
|2729|The OLE object you tried to edit is busy. Try again later. |
|2730|There was a problem communicating with the OLE server. Try again later. If you still can't access the object, try one or more of the following: * Free up system memory. For information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'. * Reinstall the OLE server to make sure it's registered. * Check the OLE server's documentation for information about the syntax to use when specifying an OLE object's data. |
|2731|An error occurred while accessing the OLE server. The OLE server may not be registered. To register the OLE server, reinstall it.  |
|2732|Microsoft Access can't read the _1 object. Communication between Microsoft Access and the OLE server was interrupted. Make sure your computer is connected to the network server on which the OLE server is located.  |
|2733|The OLE object you tried to edit can't be accessed. You don't have permission to change the object, or another user opened and locked the object. |
|2734|You can't save the _ object now. The OLE server is running an operation, or another user opened and locked the object. Try to save the object again later.  |
|2735|This disk is write-protected. You can't save the _ object to it. |
|2737|Microsoft Access can't find the file containing the linked OLE object you tried to update using the OLE/DDE Links command. You may have misspelled the file name, or the file may have been deleted or renamed. If the file has been moved to a different location, use the OLE/DDE Links command to change the source. Or delete the object, and create a new linked object.  |
|2738|There isn't enough memory to complete the operation. Close unneeded programs and try the operation again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'. |
|2739|An error occurred during the operation with an OLE object. The object is in use. |
|2741|Your computer ran out of disk space while Microsoft Access was saving the changes you made to the _1 object. For information on freeing disk space, search the Microsoft Windows Help index for 'disk space, freeing'. |
|2742|Microsoft Access was unable to create more files. Your computer may be low on memory or disk space. Close unneeded programs and try the operation again. For information on freeing memory or disk space, search the Microsoft Windows Help index for 'memory, troubleshooting' or 'disk space, freeing'.  |
|2743|The _ object is stored in a format that is incompatible with the version of OLE on your computer. |
|2744|Microsoft Access can't find the OLE server. The setting for the SourceDoc property may be invalid, or the file may have been deleted, renamed, or moved. |
|2745|Share.exe or Vshare.386 is missing from your computer; OLE support needs these files to work correctly. Rerun Microsoft Access or Microsoft Office Setup to reinstall Microsoft Access, the Share program, and Vshare.386. If you want to preserve your security or custom settings, back up the Microsoft Access workgroup information file. Then restore the file to its original location. For information on backing up files, search the Microsoft Windows Help index for 'backing up files'.   |
|2746|You can't switch to Design view because your form contains too many OLE objects. Close other applications, close the form, and then open the form again in Design view. Then delete some of the OLE objects or move them to a different form. |
|2747|The OLE server can't display the _1 object. There is a problem with the file containing the OLE object, or there isn't enough memory available. Open the OLE server outside of Microsoft Access, and then open the OLE object file. If you can do this, then your computer may be low on memory. Close other programs, and then try the operation again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'.  |
|2748|The Automation object operation isn't available for the _ object. Check the component's documentation for information on which operations are available for an Automation object. |
|2749|There isn't enough memory to complete the Automation object operation on the _ object. Close unneeded programs and try the operation again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'. |
|2750|The operation on the _ object failed. The OLE server may not be registered. To register the OLE server, reinstall it.  |
|2751|The Exit or Update operation failed. You pressed the ESC key (or another key used in the OLE server to stop an operation) while Microsoft Access was saving the changes you made to an OLE object in a form or report. Try to exit or update again.  |
|2753|A problem occurred while Microsoft Access was communicating with the OLE server or ActiveX Control. Close the OLE server and restart it outside of Microsoft Access. Then try the original operation again in Microsoft Access. |
|2754|A problem occurred while Microsoft Access was communicating with the OLE server. Try one or more of the following: * Make sure you're connected to the network server where the OLE server application is located. * Close the OLE server and restart it outside of Microsoft Access. Then try the original operation again from within Microsoft Access. * Reinstall the OLE server to ensure that it's registered. |
|2755|There was a problem referencing a property or method of the object. You tried to run a Visual Basic procedure that references an object property or method. Try one or more of the following: * Make sure the component is properly registered. * Make sure your computer is connected to the network server where the component is located. * Close the component and restart it outside of Microsoft Access. Then try again to run the procedure in Microsoft Access.  |
|2756|A problem occurred when Microsoft Access tried to access the OLE object. Close the Microsoft Access form or report that displays the OLE object, and close the OLE server. Then reopen the form or report to see if it can display the OLE object. |
|2757|There was a problem accessing a property or method of the OLE object. Try one or more of the following: * Verify that the OLE server is registered correctly by reinstalling it. * Make sure your computer is connected to the server on which the OLE server application resides. * Close the OLE server and restart it outside of Microsoft Access. Then try the original operation again from within Microsoft Access. |
|2759|The method you tried to invoke on an object failed. * You may have specified too many or too few arguments for a property or method of an object. Check the component's documentation for information on the properties and methods it makes available for Automation operations. * There may not be enough memory to run the procedure. Close unneeded programs and try to run the procedure again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'. |
|2760|An error occurred while referencing the object. You tried to run a Visual Basic procedure that improperly references a property or method of an object. |
|2761|There was a problem referencing a property or method of an object. Check the component's documentation for information on the properties and methods it makes available for Automation operations. |
|2762|_ returned an error while referencing a property of an object. Check the component's documentation for information on the properties and methods it makes available for Automation operations. |
|2763|_1 returned the error: _2. Check the component's documentation for information on the properties and methods it makes available for Automation operations. |
|2764|The object's property or method can't be set. You tried to run a Visual Basic procedure to set a property or apply a method for an object. However, the property or method doesn't support named arguments. Check the component's documentation for information on the properties and methods it makes available to Automation operations.  |
|2765|Visual Basic can't convert the data type of one of the arguments you entered. You tried to run a Visual Basic procedure that executes a method or sets a property of an object. Check the component's documentation for information on the properties and methods it makes available for Automation operations.  |
|2766|The object doesn't contain the Automation object '_’. You tried to run a Visual Basic procedure to set a property or method for an object. However, the component doesn't make the property or method available for Automation operations. Check the component's documentation for information on the properties and methods it makes available for Automation operations.  |
|2767|The object doesn't support American English; it was developed using a different language. Use a version of the object developed in Visual Basic that supports the language you are using. |
|2768|The number you used to reference an element in the array is outside the bounds of the array. For example, the array is from 0 through 10, and you entered a -1 or an 11. Check the component's documentation for information on the properties and methods it makes available for Automation operations.  |
|2769|A property of the Automation object requires or returns a data type that isn't supported by Visual Basic. You tried to run a Visual Basic procedure that references an Automation object's property. However, the value of the property isn't supported by Visual Basic. Check the component's documentation for information on the properties and methods it makes available for Automation operations.  |
|2770|The object you referenced in the Visual Basic procedure as an OLE object isn't an OLE object. |
|2771|The bound or unbound object frame you tried to edit does not contain an OLE object. Right-click the frame, click Insert Object, and then use the dialog box to locate and either add or link to an object from a file that is not empty. |
|2774|The component doesn't support Automation. You tried to run a Visual Basic procedure that references an Automation object. Check the component's documentation for information on whether it supports Automation.  |
|2775|You specified too many arguments in the Visual Basic procedure, or there isn't enough memory to run the procedure. Specify fewer arguments, or close unneeded programs, and then try to run the procedure again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'. |
|2777|The class argument in the CreateObject function of the Visual Basic procedure you're trying to run is invalid. Try one of the following: * Make sure the file is installed on your computer and that you used the correct file name. * Check the OLE server's documentation for information about the syntax to use when specifying an OLE object's data. |
|2778|Microsoft Access tried to create an OLE link, but there was no source document for this object. |
|2782|You must specify a property or method for the object. You tried to run a Visual Basic procedure that references and sets a property or method for the object. Enter a property or method for the object.  |
|2783|You entered an invalid setting for the Action property. Use one of the Microsoft Access intrinsic constants for the Action property. For a list of valid settings you can use with the Action property, click Help. |
|2784|The path you entered for the SourceDoc property setting for a linked OLE object is too long. Move the file to a location with a shorter path. |
|2785|The OLE server wasn't able to open the object. * The OLE server may not be installed. * You may have specified an invalid setting for the SourceDoc or SourceItem property in a property sheet, a macro, or a Visual Basic procedure. To see the valid settings for either of these properties, search the Help index for the property topic.  |
|2786|The OLE server doesn't support linking. You tried to run a Visual Basic procedure using the Action property. However, you provided insufficient information to establish a link. |
|2788|The _ object isn't a linked object. The property you tried to set in Visual Basic applies only to linked objects. |
|2790|You can't embed an OLE object into a bound or unbound object frame if the OLETypeAllowed property for the bound or unbound object frame is set to Linked. Insert a linked object, or set the OLETypeAllowed property to Embedded or Either, and then embed the object. |
|2791|Microsoft Access can't link the OLE object or the bound or unbound object frame. The OLETypeAllowed property for the bound or unbound object frame is set to Embedded. Embed the object, or set the OLETypeAllowed property to Linked or Either, and then link the object.  |
|2792|You can't save a locked OLE object. |
|2793|Microsoft Access can't perform the operation specified in the Action property of the Visual Basic procedure you're trying to run. The object frame may be locked or disabled. Set the Locked property to No and the Enabled property to Yes.  |
|2794|The ActiveX control you tried to insert isn't registered. For information on registering an ActiveX control, click Help. |
|2797|This OLE object was created in an earlier version of OLE so it can't be displayed as an icon. For an effect similar to displaying an object as an icon, add an image control to your form, and add the icon for the application to the image control. Then set the image control's OnDblClick property to a Visual Basic procedure that opens the OLE object. |
|2798|You can't use the Action property to delete a bound OLE object from its underlying table or query. You tried to run a Visual Basic procedure that deletes the object in a bound object frame by setting the Action property to acOLEDelete. Delete the object in a different way, such as with the DAO Delete method in Visual Basic.  |
|2799|The OLE object can't be activated upon receiving the focus. If you selected an OLE object or a chart, and the AutoActivate property for that control is set to GetFocus, the OLE object or chart should be activated automatically when it receives the focus. However, the ActiveX component doesn't support this operation. Check the component's documentation for information on the properties and methods it makes available to Automation operations.  |
|2800|This object is locked. Any changes you make will be discarded when the form is closed. Click the File tab, point to Save As, and save the object under a different name. |
|2801|The OLE object isn't loaded because the unbound ActiveX control hasn't been initialized. |
|2802|You can't insert an ActiveX control in a bound or unbound object frame. ActiveX controls are automatically contained in ActiveX control frames. |
|2803|You don't have the license required to use this ActiveX control. You tried to open a form containing an OLE object or an ActiveX control or you tried to create an ActiveX control. To obtain the appropriate license, contact the company that provides the licensed OLE object or ActiveX control.  |
|2804|You can't create an ActiveX control in an unbound object frame. ActiveX controls are automatically contained in ActiveX control frames. |
|2805|There was an error loading an ActiveX control on one of your forms or reports. Make sure all the controls that you are using are properly registered. For information on registering an ActiveX control, click Help. |
|2806|Microsoft Access doesn't support this ActiveX control. |
|2807|You can't paste this object as the type you specified. Choose another object type. |
|2808|Microsoft Access can't find the Active Accessibility dynamic-link library (DLL) OleAcc. Rerun the Microsoft Access Setup program. |
|2811|Microsoft Access is unable to create the data access page.|
|2812|The path specified was invalid, or may be too long. Please check the path and ensure it is correct.  |
|2813|The file could not be opened. It may currently be in use. |
|2814|Unable to save the file. |
|2815|Unable to save the file to an alternate location. |
|2816|Unable to close the file. |
|2817|Microsoft Access is unable to save (or send) the data access page.|
|2818|Microsoft Access is unable to retrieve the file: _1. Either the file is not available, or you do not have enough disk space to copy the file. |
|2819|Microsoft Access is unable to open the data access page.|
|2820|Microsoft Access is unable to change the BASE HREF for your document.|
|2821|File in use|
|2822|Microsoft Access encountered an unexpected error while attempting to recover from a failed save (or send). Your data access page may not be in a usable state. Please attempt to save to a different location. |
|2823|The Microsoft Access data access page name '_1' is misspelled or refers to a Page that doesn't exist. If the invalid Page name is in a macro, an Action Failed dialog box will display the macro name and the macro's arguments after you click OK. Open the Macro window, and enter the correct Page name. |
|2824|You do not have adequate file permissions. |
|2825|The file does not exist, or you do not have read access to the file. |
|2827|File read error. |
|2828|File write error. The disk may be full.  |
|2832|Check file permissions and delete them from their location in your computer's file system. |
|2833|An unexpected error has occurred. |
|2835|An attempt to create this file has failed. Please select another location and retry the operation.  |
|2837|There was not enough memory. Please close other applications and try the operation again.  |
|2838|Microsoft Access is unable to preview the selected theme.|
|2839|An attempt to create a temporary file has failed. Please confirm that you have adequate disk space on your system drive and try the operation again.  |
|2840|Unable to read the list of supporting files from the data access page. |
|2842|Microsoft Access encountered an error after saving (or sending) your data access page.|
|2845|Microsoft Access is unable to open the data access page from the mail envelope.|
|2846|The save destination is full. Please clear space at the destination or save to another location.  |
|2847|Unable to create a folder for the supporting files. You may not have adequate permissions at the save destination.  |
|2848|The maximum path length was exceeded. Please specifiy a shorter filename, or use a folder that is closer to the root.  |
|2849|There are too many supporting files in your document. Please remove a few supporting files from your document, and try again.  |
|2850|You do not have write permission at the save destination. |
|2851|You are saving to a server that does not support long filenames and do not have permission to create a folder. You must have permission to create a folder at the save destination to complete this operation.  |
|2854|Microsoft Access was unable to parse the document properties for this data access page. They may be corrupted. |
|2855|Microsoft Access could not delete one or more files related to the page.|
|2859|Access could not load the e-mail envelope. This could be caused by a network connection problem or a problem with your Office installation. |
|2860|You cannot insert a bound field to a caption or record navigation section.|
|2861|Microsoft Access is unable to preview the selected web page.|
|2862|Access cannot open the specified file type from a Web server. You may have selected the wrong file.  |
|2863|Unable to create or load a file due to network or access permission problems. |
|2864|This file (or a supporting file) is already in use, or has the read-only attribute set. |
|2865|The disk is write-protected. |
|2866|Unexpected data corruption failure. |
|2867|Unexpected data I/O failure. |
|2868|You cannot save this data access page over itself because it is read-only. Please select a different file for the save.  |
|2869|The file does not exist. You do not have adequate permission to modify the data access page link to point to a valid file. Please contact the database administrator.  |
|2870|Microsoft Access encountered an error synchronizing the HTML from the Microsoft Script Editor. Please check the HTML for syntax errors and try again. |
|2871|Microsoft Access is unable to create a data access page using the codepage selected in Web Options. The codepage may not be installed on your system. Please install the codepage, or select a different one in Web Options.  |
|2873|The filename specified is a long filename, but you have the 'use long filenames' web option turned off. Please specify a file name that uses a maximum of eight characters for the name and three characters for the file extension.  |
|2874|Cannot move or paste the grouping field '_' into a section at a higher group level|
|2875|Unable to complete the save. The drive or network connection you attempted to save to may no longer be available.  |
|2876|The data definition of this data access page has been corrupted and can't be repaired. You must recreate the page. Save has been disabled.|
|2877|In a Microsoft Access database (.mdb), you can't group on a control bound to a field that has a Memo or OLE Object data type. In a Microsoft Access project (.adp), you can't group on a control bound to a field that has an Image or Text data type.|
|2878|You cannot add a bound field to a caption or record navigation section.|
|2879|Caption and record navigation sections cannot contain bound fields.|
|2880|Can't edit pages that contain framesets.|
|2881|This web page contains XML namespaces that may conflict with Access namespaces. You should edit the HTML source to ensure that all namespaces have a unique prefix.|
|2882|The folder that this Web page would use to organize supporting files is already reserved for use in the current location. Please choose a different name or location for this Web page.  |
|2883|A supporting file path for this data access page has been altered outside of Access. Please save this page to a different location and ensure that all supporting files are maintained. |
|2884|Cannot find the database or some database objects that this page refers to. Update the connection information of the page, or fix the references to the missing database objects.|
|2885|This page uses a database which is not supported. You will not be able to make data changes until you connect to a supported database|
|2886|Components necessary for data access pages are not installed.|
|2887|The path specified is not a valid absolute (non-relative) path or URL. Please enter a valid path.  |
|2888|Microsoft Access detects some HTML elements between the banner and the section of your data access page. Saving this page in Access will corrupt it. Close the page without saving it, and then edit the page in another HTML editor to remove these elements.|
|2889|This section cannot be deleted.|
|2890|You cannot edit this page because it contains frames. The data access page designer cannot edit pages with frames. |
|2892|You can't move the group filter control to another section. Delete the group filter control from the current section and create it in a different section.|
|2893|A link to this data access page could not be created because the database cannot be exclusively locked. To create the link later, open the page by selecting 'Edit web page that already exists', and then save. |
|2894|The link to the data access page specified could not be updated because the database cannot be exclusively locked. To update the link, open this page again when you are the only person using the database. |
|2895|This page was designed with a version of the Microsoft Office Web Components that is not currently installed on this machine. If you have not been prompted to install those components on this page, please contact the page author for the installation location.|
|2896|The operation is only valid on a data access page opened in Design View. Please switch the page to Design View and try the operation again. |
|2897|You have opened a page that was last modified using Access 2000. To be able to edit the page, you must save it using a more recent version of the Microsoft Office Web Components. Do you want Access to convert this page by saving it using a more recent version of the Microsoft Office Web Components?|
|2898|Microsoft Access has created a backup copy of your original page. This page can be used if you want to revert to the Office 2000 Web Components. The backup page name is: '_1'|
|2899|Microsoft Access could not create a backup copy of your original page. This page cannot be opened.|
|2900|Microsoft Access could not upgrade the Office Web Components on your page. This page cannot be opened.|
|2901|Error loading ActiveX control '_1' on form or report '_2'.|
|2902|Access is unable to save the _ object because it does not support persistence, or your computer may have run out of disk space.|
|2903|Do you want to set this folder as the default location for data access pages? |
|2904|You must match each field on the left with a field on the right. |
|2905|You must choose a linking field for every parameter. |
|2906|'_' contains no fields that can participate in a relationship. The contents of the RecordSource property may be invalid, or the RecordSource may contain only fields that are not acceptable for use in a join. Correct the RecordSource property for this form or report and try your field operation again.  |
|2907|Do you want to revert to the saved '_'? |
|2908|The Control ID '_' is already in use. Specify a different ID for the control.|
|2909|This relationship is not valid because the fields in the first table do not match the fields in the second table. To repair the relationship, select at least one field from each table. |
|2910|This connection file refers to a provider not supported by data access pages. Please select a different connection file. |
|2911|You cannot change the data access page path while it it open. Please close the page and try again. |
|2912|If you create a Data Access Page in this version of Access, you cannot open it in Design view in Access 2000. If you have installed the Microsoft Office XP Web Components, however, you can open this page in Page view in Access 2000. &Don't show this warning again|
|2913|Cannot save to a URL address with a bookmark. Please specify a valid path. |
|2914|Microsoft Access could not link to a connection file. A connection string will be embedded in the page. |
|2915|Microsoft Access is unable to connect to the data source specified in the connection string of this page. The server may not exist on the network, or there may be an error in the connection string information for this page. |
|2916|You cannot edit HTML pages created using PowerPoint in Microsoft Access.|
|2917|Invalid HTML color value.|
|2918|Unable to open or read this connection file. Either the file has been damaged or the file format is not valid. |
|2919|You can't place this control in the section you specified.|
|2920|Microsoft Access is unable to load the database schema. Save has been disabled. Either repair or reinstall Microsoft Office.  |
|2921|Microsoft Access cannot open this page because it was created using a newer version of Access. Try opening the page using a newer version of Microsoft Access.|
|2922|Microsoft Access has created a backup copy of your original page. This page can be used if you want to revert to the Office XP Web Components. The backup page name is: '_1'|
|2923|You do not have the correct permissions. Contact the server administrator.|
|2924|You cannot compact a file that was opened from a Web Server.|
|2925|You cannot encrypt a file that was opened from a Web Server.|
|2926|Because of your security settings and current security policy, this control is disabled. To modify your policy and enable the database, use the Message Bar.|
|3000|Reserved error (_); there is no message for this error.|
|3001|Invalid argument.|
|3002|Could not start session.|
|3003|Could not start transaction; too many transactions already nested.|
|3005|'_' is not a valid database name.|
|3006|Database '_' is exclusively locked.|
|3007|Cannot open library database '_'.|
|3008|The table '_' is already opened exclusively by another user, or it is already open through the user interface and cannot be manipulated programmatically.|
|3009|You tried to lock table '_' while opening it, but the table cannot be locked because it is currently in use. Wait a moment, and then try the operation again.|
|3010|Table '_' already exists.|
|3011|The Microsoft Access database engine could not find the object '_'. Make sure the object exists and that you spell its name and the path name correctly. If '_' is not a local object, check your network connection or contact the server administrator.|
|3012|Object '_' already exists.|
|3013|Could not rename installable ISAM file.|
|3014|Cannot open any more tables.|
|3015|Index not found.|
|3016|Field will not fit in record.|
|3017|The size of a field is too long.|
|3018|Could not find field.|
|3019|Operation invalid without a current index.|
|3020|Update or CancelUpdate without AddNew or Edit.|
|3021|No current record.|
|3022|The changes you requested to the table were not successful because they would create duplicate values in the index, primary key, or relationship. Change the data in the field or fields that contain duplicate data, remove the index, or redefine the index to permit duplicate entries and try again.|
|3023|AddNew or Edit already used.|
|3024|Could not find file '_'.|
|3025|Cannot open any more files.|
|3026|Not enough space on disk.|
|3027|Cannot update. Database or object is read-only.|
|3028|Cannot start your application. The workgroup information file is missing or opened exclusively by another user.|
|3029|Not a valid account name or password.|
|3030|'_' is not a valid account name.|
|3031|Not a valid password.|
|3032|Cannot perform this operation.|
|3033|You do not have the necessary permissions to use the '_' object. Have your system administrator or the person who created this object establish the appropriate permissions for you.|
|3034|You tried to commit or rollback a transaction without first beginning a transaction.|
|3035|System resource exceeded.|
|3036|Database has reached maximum size.|
|3037|Cannot open any more tables or queries.|
|3038|System resource exceeded.|
|3039|Could not create index; too many indexes already defined.|
|3040|Disk I/O error during read.|
|3041|Cannot open a database created with a previous version of your application.|
|3042|Out of MS-DOS file handles.|
|3043|Your network access was interrupted. To continue, close the database, and then open it again.|
|3044|'_' is not a valid path. Make sure that the path name is spelled correctly and that you are connected to the server on which the file resides.|
|3045|Could not use '_'; file already in use.|
|3046|Could not save; currently locked by another user.|
|3047|Record is too large.|
|3048|Cannot open any more databases.|
|3049|Cannot open database '_'. It may not be a database that your application recognizes, or the file may be corrupt.|
|3050|Could not lock file.|
|3051|The Microsoft Access database engine cannot open or write to the file '_'. It is already opened exclusively by another user, or you need permission to view and write its data.|
|3052|File sharing lock count exceeded. Increase MaxLocksPerFile registry entry. See Microsoft KB Article 815281|
|3053|Too many client tasks.|
|3054|Too many Memo, OLE, or Hyperlink Object fields.|
|3055|Not a valid file name.|
|3056|Could not repair this database.|
|3057|Operation not supported on linked tables.|
|3058|Index or primary key cannot contain a Null value.|
|3059|Operation canceled by user.|
|3060|Wrong data type for parameter '_'.|
|3061|Too few parameters. Expected _.|
|3062|Duplicate output alias '_'.|
|3063|Duplicate output destination '_'.|
|3064|Cannot open action query '_'.|
|3065|Cannot execute a select query.|
|3066|Query must have at least one destination field.|
|3067|Query input must contain at least one table or query.|
|3068|Not a valid alias name.|
|3069|The action query '_' cannot be used as a row source.|
|3070|The Microsoft Access database engine does not recognize '_' as a valid field name or expression.|
|3071|This expression is typed incorrectly, or it is too complex to be evaluated. For example, a numeric expression may contain too many complicated elements. Try simplifying the expression by assigning parts of the expression to variables.|
|3073|Operation must use an updateable query.|
|3074|Cannot repeat table name '_' in FROM clause.|
|3075|_1 in query expression '_2'.|
|3076|_ in criteria expression.|
|3077|_ in expression.|
|3078|The Microsoft Access database engine cannot find the input table or query '_'. Make sure it exists and that its name is spelled correctly.|
|3079|The specified field '_' could refer to more than one table listed in the FROM clause of your SQL statement.|
|3080|Joined table '_' not listed in FROM clause.|
|3081|Cannot join more than one table with the same name (_).|
|3082|JOIN operation '_' refers to a field that is not in one of the joined tables.|
|3083|Cannot use internal report query.|
|3084|Cannot insert data with action query.|
|3085|Undefined function '_' in expression.|
|3086|Could not delete from specified tables.|
|3087|Too many expressions in GROUP BY clause.|
|3088|Too many expressions in ORDER BY clause.|
|3089|Too many expressions in DISTINCT output.|
|3090|Resultant table not allowed to have more than one AutoNumber field.|
|3091|HAVING clause (_) without grouping or aggregation.|
|3092|Cannot use HAVING clause in TRANSFORM statement.|
|3093|ORDER BY clause (_) conflicts with DISTINCT.|
|3094|ORDER BY clause (_) conflicts with GROUP BY clause.|
|3095|Cannot have aggregate function in expression (_).|
|3096|Cannot have aggregate function in WHERE clause (_).|
|3097|Cannot have aggregate function in ORDER BY clause (_).|
|3098|Cannot have aggregate function in GROUP BY clause (_).|
|3099|Cannot have aggregate function in JOIN operation (_).|
|3100|Cannot set field '_' in join key to Null.|
|3101|The Microsoft Access database engine cannot find a record in the table '_2' with key matching field(s) '_1'.|
|3102|Circular reference caused by '_'.|
|3103|Circular reference caused by alias '_' in query definition's SELECT list.|
|3104|Cannot specify fixed column heading '_' in a crosstab query more than once.|
|3105|Missing destination field name in SELECT INTO statement (_).|
|3106|Missing destination field name in UPDATE statement (_).|
|3107|Record(s) cannot be added; no insert permission on '_'.|
|3108|Record(s) cannot be edited; no update permission on '_'.|
|3109|Record(s) cannot be deleted; no delete permission on '_'.|
|3110|Could not read definitions; no read definitions permission for table or query '_'.|
|3111|Could not create; no modify design permission for table or query '_'.|
|3112|Record(s) cannot be read; no read permission on '_'.|
|3113|Cannot update '_'; field not updateable.|
|3114|Cannot include Memo, OLE, or Hyperlink Object when you select unique values (_).|
|3115|Cannot have Memo, OLE, or Hyperlink Object fields in aggregate argument (_).|
|3116|Cannot have Memo, OLE, or Hyperlink Object fields in criteria (_) for aggregate function.|
|3117|Cannot sort on Memo, OLE, or Hyperlink Object (_).|
|3118|Cannot join on Memo, OLE, or Hyperlink Object (_).|
|3119|Cannot group on Memo, OLE, or Hyperlink Object (_).|
|3120|Cannot group on fields selected with '*' (_).|
|3121|Cannot group on fields selected with '*'.|
|3122|You tried to execute a query that does not include the specified expression '_' as part of an aggregate function.|
|3123|Cannot use '*' in crosstab query.|
|3124|Cannot input from internal report query (_).|
|3125|'_' is not a valid name. Make sure that it does not include invalid characters or punctuation and that it is not too long.|
|3126|Invalid bracketing of name '_'.|
|3127|The INSERT INTO statement contains the following unknown field name: '_'. Make sure you have typed the name correctly, and try the operation again.|
|3128|Specify the table containing the records you want to delete.|
|3129|Invalid SQL statement; expected 'DELETE', 'INSERT', 'PROCEDURE', 'SELECT', or 'UPDATE'.|
|3130|Syntax error in DELETE statement.|
|3131|Syntax error in FROM clause.|
|3132|Syntax error in GROUP BY clause.|
|3133|Syntax error in HAVING clause.|
|3134|Syntax error in INSERT INTO statement.|
|3135|Syntax error in JOIN operation.|
|3136|The LEVEL clause includes a reserved word or argument that is misspelled or missing, or the punctuation is incorrect.|
|3137|Missing semicolon (;) at end of SQL statement.|
|3138|Syntax error in ORDER BY clause.|
|3139|Syntax error in PARAMETER clause.|
|3140|Syntax error in PROCEDURE clause.|
|3141|The SELECT statement includes a reserved word or an argument name that is misspelled or missing, or the punctuation is incorrect.|
|3142|Characters found after end of SQL statement.|
|3143|Syntax error in TRANSFORM statement.|
|3144|Syntax error in UPDATE statement.|
|3145|Syntax error in WHERE clause.|
|3146|ODBC--call failed.|
|3151|ODBC--connection to '_' failed.|
|3154|ODBC--could not find DLL '_'.|
|3155|ODBC--insert on a linked table '_' failed.|
|3156|ODBC--delete on a linked table '_' failed.|
|3157|ODBC--update on a linked table '_' failed.|
|3158|Could not save record; currently locked by another user.|
|3159|Not a valid bookmark.|
|3160|Table is not open.|
|3161|Could not decrypt file.|
|3162|You tried to assign the Null value to a variable that is not a Variant data type.|
|3163|The field is too small to accept the amount of data you attempted to add. Try inserting or pasting less data.|
|3164|Field cannot be updated.|
|3165|Could not open .inf file.|
|3166|Cannot locate the requested Xbase memo file.|
|3167|Record is deleted.|
|3168|Invalid .inf file.|
|3169|The Microsoft Access database engine could not execute the SQL statement because it contains a field that has an invalid data type.|
|3170|Could not find installable ISAM.|
|3171|Could not find network path or user name.|
|3172|Could not open Paradox.net.|
|3173|Could not open table 'MSysAccounts' in the workgroup information file.|
|3174|Could not open table 'MSysGroups' in the workgroup information file.|
|3175|Date is out of range or is in an invalid format.|
|3176|Could not open file '_'.|
|3177|Not a valid table name.|
|3179|Encountered unexpected end of file.|
|3180|Could not write to file '_'.|
|3181|Invalid range.|
|3182|Invalid file format.|
|3183|The query cannot be completed. Either the size of the query result is larger than the maximum size of a database (2 GB), or there is not enough temporary storage space on the disk to store the query result.|
|3184|Could not execute query; could not find linked table.|
|3185|SELECT INTO on a remote database tried to produce too many fields.|
|3186|Could not save; currently locked by user '_2' on machine '_1'.|
|3187|Could not read; currently locked by user '_2' on machine '_1'.|
|3188|Could not update; currently locked by another session on this machine.|
|3189|Table '_1' is exclusively locked by user '_3' on machine '_2'.|
|3190|Too many fields defined.|
|3191|Cannot define field more than once.|
|3192|Could not find output table '_'.|
|3195|(expression)|
|3196|The database '_' is already in use by another person or process. When the database is available, try the operation again.|
|3197|The Microsoft Access database engine stopped the process because you and another user are attempting to change the same data at the same time.|
|3198|Could not start session. Too many sessions already active.|
|3199|Could not find reference.|
|3200|The record cannot be deleted or changed because table '_' includes related records.|
|3201|You cannot add or change a record because a related record is required in table '_'.|
|3202|Could not save; currently locked by another user.|
|3203|Subqueries cannot be used in the expression (_).|
|3204|Database already exists.|
|3205|Too many crosstab column headers (_).|
|3206|Cannot create a relationship between a field and itself.|
|3207|Operation not supported on a Paradox table with no primary key.|
|3208|Invalid Deleted setting in the Xbase key of the Windows Registry.|
|3210|The connection string is too long. The connection string cannot exceed 255 characters.|
|3211|The database engine could not lock table '_' because it is already in use by another person or process.|
|3212|Could not lock table '_1'; currently in use by user '_3' on machine '_2'.|
|3213|Invalid Date setting in the Xbase key of the Windows Registry.|
|3214|Invalid Mark setting in the Xbase key of the Windows Registry.|
|3215|Too many Btrieve tasks.|
|3216|Parameter '_' specified where a table name is required.|
|3217|Parameter '_' specified where a database name is required.|
|3218|Could not update; currently locked.|
|3219|Invalid operation.|
|3220|Incorrect collating sequence.|
|3221|Invalid settings in the Btrieve key of the Windows Registry.|
|3222|Query cannot contain a Database parameter.|
|3223|'_' is invalid because it is too long, or contains invalid characters.|
|3224|Cannot read Btrieve data dictionary.|
|3225|Encountered a record locking deadlock while performing a Btrieve operation.|
|3226|Errors encountered while using the Btrieve DLL.|
|3227|Invalid Century setting in the Xbase key of the Windows Registry.|
|3228|Selected collating sequence not supported by the operating system.|
|3229|Btrieve--cannot change field.|
|3230|Out-of-date Paradox lock file.|
|3231|ODBC--field would be too long; data truncated.|
|3232|ODBC--could not create table.|
|3234|ODBC--remote query timeout expired.|
|3235|ODBC--data type not supported on server.|
|3238|ODBC--data out of range.|
|3239|Too many active users.|
|3240|Btrieve--missing Btrieve engine.|
|3241|Btrieve--out of resources.|
|3242|Invalid reference in SELECT statement.|
|3243|None of the import field names match fields in the appended table.|
|3244|Cannot import password-protected spreadsheet.|
|3245|Could not parse field names from the first row of the import table.|
|3246|Operation not supported in transactions.|
|3247|ODBC--linked table definition has changed.|
|3248|Invalid NetworkAccess setting in the Windows Registry.|
|3249|Invalid PageTimeout setting in the Windows Registry.|
|3250|Could not build key.|
|3251|Operation is not supported for this type of object.|
|3252|Cannot open a form whose underlying query contains a user-defined function that attempts to set or get the form's RecordsetClone property.|
|3254|ODBC--Cannot lock all records.|
|3256|Index file not found.|
|3257|Syntax error in WITH OWNERACCESS OPTION declaration.|
|3258|The SQL statement could not be executed because it contains ambiguous outer joins. To force one of the joins to be performed first, create a separate query that performs the first join and then include that query in your SQL statement.|
|3259|Invalid field data type.|
|3260|Could not update; currently locked by user '_2' on machine '_1'.|
|3261|Table '_' is exclusively locked by user '_2' on machine '_1'.|
|3262|Could not lock table|
|3263|Invalid Database object.|
|3264|No field defined--cannot append TableDef or Index.|
|3265|Item not found in this collection.|
|3266|Cannot append a Field that is already a part of a Fields collection.|
|3267|Property can be set only when the Field is part of a Recordset object's Fields collection.|
|3268|Cannot set this property once the object is part of a collection.|
|3269|Cannot append an Index that is already a part of an Indexes collection.|
|3270|Property not found.|
|3271|Invalid property value.|
|3272|Object is not a collection.|
|3273|Method not applicable for this object.|
|3274|External table is not in the expected format.|
|3275|Unexpected error from external database driver (_).|
|3276|Invalid database object reference.|
|3277|Cannot have more than 10 fields in an index.|
|3278|The Microsoft Access database engine has not been initialized.|
|3279|The Microsoft Access database engine has already been initialized.|
|3280|Cannot delete a field that is part of an index or is needed by the system.|
|3281|Cannot delete this index or table. It is either the current index or is used in a relationship.|
|3282|Operation not supported on a table that contains data.|
|3283|Primary key already exists.|
|3284|Index already exists.|
|3285|Invalid index definition.|
|3286|Format of memo file does not match specified external database format.|
|3287|Cannot create index on the given field.|
|3288|Paradox index is not primary.|
|3289|Syntax error in CONSTRAINT clause.|
|3290|Syntax error in CREATE TABLE statement.|
|3291|Syntax error in CREATE INDEX statement.|
|3292|Syntax error in field definition.|
|3293|Syntax error in ALTER TABLE statement.|
|3294|Syntax error in DROP INDEX statement.|
|3295|Syntax error in DROP TABLE or DROP INDEX.|
|3296|JOIN expression not supported.|
|3297|Could not import table or query. No records found, or all records contain errors.|
|3298|There are several tables with that name. Please specify owner in the format 'owner.table'.|
|3299|ODBC Specification Conformance Error (_). Report this error to the developer of your application.|
|3300|Cannot create a relationship.|
|3301|Cannot perform this operation; features in this version are not available in databases with older formats.|
|3302|Cannot change a rule while the rules for this table are in use.|
|3303|Cannot delete this field. It is part of one or more relationships.|
|3304|You must enter a personal identifier (PID) consisting of at least 4 and no more than 20 characters and digits.|
|3305|Invalid connection string in pass-through query.|
|3306|You have written a subquery that can return more than one field without using the EXISTS reserved word in the main query's FROM clause. Revise the SELECT statement of the subquery to request only one field.|
|3307|The number of columns in the two selected tables or queries of a union query do not match.|
|3308|Invalid TOP argument in select query.|
|3309|Property value is too large.|
|3310|This property is not supported for external data sources or for databases created with a previous version of Microsoft Jet.|
|3311|Property specified already exists.|
|3312|Validation rules and default values cannot be placed on system or linked tables.|
|3313|Cannot place this validation expression on this field.|
|3314|You must enter a value in the '_' field.|
|3315|Field '_' cannot be a zero-length string.|
|3317|One or more values are prohibited by the validation rule '_2' set for '_1'. Enter a value that the expression for this field can accept.|
|3318|Values specified in a TOP clause are not allowed in delete queries or reports.|
|3319|Syntax error in union query.|
|3320|_ in table-level validation expression.|
|3321|No database specified in connection string or IN clause.|
|3322|Crosstab query contains one or more invalid fixed column headings.|
|3323|The query cannot be used as a row source.|
|3324|The query is a DDL query and cannot be used as a row source.|
|3325|Pass-through query with ReturnsRecords property set to True did not return any records.|
|3326|This Recordset is not updateable.|
|3327|Field '_' is based on an expression and cannot be edited.|
|3328|Table is read-only.|
|3329|Record in table '_' was deleted by another user.|
|3330|Record in table '_' is locked by another user.|
|3331|To make changes to this field, first save the record.|
|3332|Cannot enter value into blank field on 'one' side of outer join.|
|3333|Records in table '_' would have no record on the 'one' side.|
|3334|Can be present only in version 1.0 format.|
|3335|DeleteOnly called with non-zero cbData.|
|3336|Btrieve: Invalid IndexDDF option in initialization setting.|
|3337|Invalid DataCodePage option in initialization setting.|
|3338|Btrieve: Xtrieve options are not correct in initialization setting.|
|3339|Btrieve: Invalid IndexDeleteRenumber option in initialization setting.|
|3340|Query '_' is corrupt.|
|3341|The current field must match the join key '_' in the table that serves as the 'one' side of one-to-many relationship. Enter a record in the 'one' side table with the desired key value, and then make the entry with the desired join key in the 'many-only' table.|
|3342|Invalid Memo, OLE, or Hyperlink Object in subquery '_'.|
|3343|Unrecognized database format '_'.|
|3344|The database engine does not recognize either the field '_1' in a validation expression, or the default value in the table '_2'.|
|3345|Unknown or invalid field reference '_'.|
|3346|Number of query values and destination fields are not the same.|
|3347|Cannot add record(s); primary key for table '_' not in recordset.|
|3348|Cannot add record(s); join key of table '_' not in recordset.|
|3349|You cannot record your changes because a value you entered violates the settings defined for this table or list (for example, a value is less than the minimum or greater than the maximum). Correct the error and try again.|
|3350|Object is invalid for operation.|
|3351|The ORDER BY expression (_) includes fields that are not selected by the query. Only those fields requested in the first query can be included in an ORDER BY expression.|
|3352|No destination field name in INSERT INTO statement (_).|
|3353|Btrieve: Cannot find file Field.ddf.|
|3354|At most one record can be returned by this subquery.|
|3355|Syntax error in default value.|
|3356|You attempted to open a database that is already opened exclusively by user '_2' on machine '_1'. Try again when the database is available.|
|3357|This query is not a properly formed data-definition query.|
|3358|Cannot open the Microsoft Access database engine workgroup information file.|
|3359|Pass-through query must contain at least one character.|
|3360|Query is too complex.|
|3361|Unions not allowed in a subquery.|
|3362|Single-row update/delete affected more than one row of a linked table. Unique index contains duplicate values.|
|3363|Record(s) cannot be added; no corresponding record on the 'one' side.|
|3364|Cannot use Memo, OLE, or Hyperlink Object field '_' in the SELECT clause of a union query.|
|3365|Property value not valid for REMOTE objects.|
|3366|Cannot append a relation with no fields defined.|
|3367|Cannot append. An object with that name already exists in the collection.|
|3368|Relationship must be on the same number of fields with the same data types.|
|3370|Cannot modify the design of table '_'. It is in a read-only database.|
|3371|Cannot find table or constraint.|
|3372|No such index '_2' on table '_1'.|
|3373|Cannot create relationship. Referenced table '_' does not have a primary key.|
|3374|The specified fields are not uniquely indexed in table '_'.|
|3375|Table '_1' already has an index named '_2'.|
|3376|Table '_' does not exist.|
|3377|No such relationship '_2' on table '_1'.|
|3378|There is already a relationship named '_' in the current database.|
|3379|Cannot create relationships to enforce referential integrity. Existing data in table '_2' violates referential integrity rules in table '_1'.|
|3380|Field '_2' already exists in table '_1'.|
|3381|There is no field named '_2' in table '_1'.|
|3382|Size of field '_' is too long.|
|3383|Cannot delete field '_'. It is part of one or more relationships.|
|3384|Cannot delete a built-in property.|
|3385|User-defined properties do not support a Null value.|
|3386|Property '_' must be set before using this method.|
|3387|Cannot find TEMP directory.|
|3388|Unknown function '_2' in validation expression or default value on '_1'.|
|3389|Query support unavailable.|
|3390|Account name already exists.|
|3391|An error has occurred. Properties were not saved.|
|3393|Cannot perform join, group, sort, or indexed restriction. A value being searched or sorted on is too long.|
|3394|Cannot save property; property is a schema property.|
|3396|Cannot perform cascading operation. Since related records exist in table '_', referential integrity rules would be violated.|
|3397|Cannot perform cascading operation. There must be a related record in table '_'.|
|3398|Cannot perform cascading operation. It would result in a null key in table '_'.|
|3399|Cannot perform cascading operation. It would result in a duplicate key in table '_'.|
|3400|Cannot perform cascading operation. It would result in two updates to field '_2' in table '_1'.|
|3401|Cannot perform cascading operation. It would cause field '_' to become Null, which is not allowed.|
|3402|Cannot perform cascading operation. It would cause field '_' to become a zero-length string, which is not allowed.|
|3403|Cannot perform cascading operation: '_'.|
|3404|Cannot perform cascading operation. The value entered is prohibited by the validation rule '_2' set for '_1'.|
|3405|Error '_' in validation rule.|
|3406|The expression you are trying to use for the DefaultValue property is invalid because '_'. Use a valid expression to set this property.|
|3407|The server's MSysConf table exists, but is in an incorrect format. Contact your system administrator.|
|3408|Too many FastFind Sessions were invoked.|
|3409|Invalid field definition '_' in definition of index or relationship.|
|3411|Invalid entry. Cannot perform cascading operation in table '_1' because the value entered is too large for field '_2'.|
|3412|Cannot perform cascading update on the table because it is currently in use by another user.|
|3413|Cannot perform cascading operation on table '_1' because it is currently in use by user '_3' on machine '_2'.|
|3414|Cannot perform cascading operation on table '_1' because it is currently in use.|
|3415|Zero-length string is valid only in a Text or Memo field.|
|3417|An action query cannot be used as a row source.|
|3418|Cannot open '_'. Another user has the table open using a different network control file or locking style.|
|3419|Cannot open this Paradox 4.x or 5.x table because ParadoxNetStyle is set to 3.x in the Windows Registry.|
|3420|Object invalid or no longer set.|
|3421|Data type conversion error.|
|3422|Cannot modify table structure. Another user has the table open.|
|3423|You cannot use ODBC to import from, export to, or link an external Microsoft Access or ISAM database table to your database.|
|3424|Cannot create database because the locale is invalid.|
|3425|This method or property is not currently available on this Recordset.|
|3426|This action was cancelled by an associated object.|
|3427|Error in DAO automation.|
|3428|A problem occurred in your database. Correct the problem by repairing and compacting the database.|
|3429|Incompatible version of an installable ISAM.|
|3430|While loading the Microsoft Excel installable ISAM, OLE was unable to initialize.|
|3431|This is not a Microsoft Excel 5.0 file.|
|3432|Error opening a Microsoft Excel 5.0 file.|
|3433|Invalid setting in Excel key of the Engines section of the Windows Registry.|
|3434|Cannot expand named range.|
|3435|Cannot delete spreadsheet cells.|
|3436|Failure creating file.|
|3437|Spreadsheet is full.|
|3438|The data being exported does not match the format described in the Schema.ini file.|
|3439|You attempted to link or import a Microsoft Word mail merge file. Although you can export such files, you cannot link or import them.|
|3440|An attempt was made to import or link an empty text file. To import or link a text file, the file must contain data.|
|3441|Text file specification field separator matches decimal separator or text delimiter.|
|3442|In the text file specification '_1', the _2 option is invalid.|
|3443|The fixed width specification '_1' contains no column widths.|
|3444|In the fixed width specification '_1', column '_2' does not specify a width.|
|3445|Incorrect version of the DLL file '_' was found.|
|3446|VBAJET32.dll is missing. Try reinstalling the Microsoft Access database engine.|
|3447|VBAJET32.dll failed to initialize when called. Try reinstalling the Microsoft Access database engine.|
|3448|A call to an OLE system function was not successful. Try reinstalling the application that returned the error.|
|3449|No country/region code found in connection string for a linked table.|
|3450|Syntax error in query. Incomplete query clause.|
|3451|Illegal reference in query.|
|3452|You cannot make changes to the design of the database at this replica.|
|3453|You cannot establish or maintain an enforced relationship between a replicated table and a local table.|
|3455|Cannot make the database replicable.|
|3456|Cannot make the _2 object in _1 container replicable.|
|3457|You cannot set the KeepLocal property for an object that is already replicated.|
|3458|The KeepLocal property cannot be set on a database; it can be set only on the objects in a database.|
|3459|After a database has been replicated, you cannot remove the replication features from the database.|
|3460|The operation you attempted conflicts with an existing operation involving this member of the replica set.|
|3461|The replication property you are attempting to set or delete is read-only and cannot be changed.|
|3462|Failure to load a DLL.|
|3463|Cannot find the .dll '_2'.|
|3464|Data type mismatch in criteria expression.|
|3465|The disk drive you are attempting to access is unreadable.|
|3468|Access was denied while accessing dropbox folder '_2'.|
|3469|The disk for dropbox folder '_2' is full.|
|3470|Disk failure accessing dropbox folder '_2'.|
|3471|Failure to write to the Synchronizer log file.|
|3472|Disk full for path '_1'.|
|3473|Disk failure while accessing log file '_1'.|
|3474|Cannot open the log file '_1' for writing.|
|3475|Sharing violation while attempting to open log file '_1' in Deny Write mode.|
|3476|Invalid dropbox path '_2'.|
|3477|Dropbox address '_2' is syntactically invalid.|
|3478|The replica is not a partial replica.|
|3479|Cannot designate a partial replica as the Design Master for the replica set.|
|3480|The relationship '_' in the partial filter expression is invalid.|
|3481|The table name '_' in the partial filter expression is invalid.|
|3482|The filter expression for the partial replica is invalid.|
|3483|The password supplied for the dropbox folder '_2' is invalid.|
|3484|The password used by the Synchronizer to write to a destination dropbox folder is invalid.|
|3485|The object cannot be replicated because the database is not replicated.|
|3486|You cannot add a second Replication ID AutoNumber field to a table.|
|3487|The database you are attempting to replicate cannot be converted.|
|3488|The value specified is not a ReplicaID for any member in the replica set.|
|3489|The object specified cannot be replicated because it is missing a necessary resource.|
|3490|Cannot create a new replica because the '_2' object in '_1' container could not be replicated.|
|3491|The database must be opened in exclusive mode before it can be replicated.|
|3492|The synchronization failed because a design change could not be applied to one of the replicas.|
|3493|Cannot set the specified Registry parameter for the Synchronizer.|
|3494|Unable to retrieve the specified Registry parameter for the Synchronizer.|
|3495|There are no scheduled synchronization's between the two Synchronizers.|
|3496|Replication Manager cannot find the ExchangeID in the MSysExchangeLog table.|
|3497|Unable to set a schedule for the Synchronizer.|
|3499|Cannot retrieve the full path information for a member of the replica set.|
|3500|You cannot specify two different Synchronizers to manage the same replica.|
|3502|The Design Master or replica is not being managed by a Synchronizer.|
|3503|The Synchronizer's Registry has no value set for the key you queried.|
|3504|The Synchronizer ID does not match an existing ID in the MSysTranspAddress table.|
|3505|You attempted to delete or get information about a partial filter that does not exist in MSysFilters.|
|3506|The Synchronizer is unable to open the Synchronizer log.|
|3507|Failure writing to the Synchronizer log.|
|3508|There is no active transport for the Synchronizer.|
|3509|Could not find a valid transport for this Synchronizer.|
|3510|The member of the replica set you are attempting to synchronize is currently being used in another synchronization.|
|3512|Failed to read the dropbox folder.|
|3513|Failed to write to the dropbox folder.|
|3514|Synchronizer could not find any scheduled or on-demand synchronization's to process.|
|3515|The Microsoft Access database engine could not read the system clock on your computer.|
|3516|Destination synchronizer is not configured to support indirect synchronronization, and the destination replica is unavailable for direct synchronization.|
|3517|Synchronizer could not find any messages to process.|
|3518|Could not find Synchronizer in the MSysTranspAddress table.|
|3519|Failed to send a message.|
|3520|The replica name or ID does not match a currently managed member of the replica set.|
|3521|Two members of the replica set cannot be synchronized because there is no common point to start the synchronization.|
|3522|Synchronizer cannot find the record of a specific synchronization in the MSysExchangeLog table.|
|3523|Synchronizer cannot find a specific version number in the MSysSchChange table.|
|3524|The history of design changes in the replica does not match the history in the Design Master.|
|3525|Synchronizer could not access the message database.|
|3526|The name selected for the system object is already in use.|
|3527|The Synchronizer or Replication Manager could not find the system object.|
|3528|There is no new data in shared memory for the Synchronizer or Replication Manager to read.|
|3529|The Synchronizer or Replication Manager found unread data in the shared memory. The existing data will be overwritten.|
|3530|The Synchronizer is already serving a client.|
|3531|The wait period for an event has timed out.|
|3532|Synchronizer could not be initialized.|
|3533|The system object used by a process still exists after the process has stopped.|
|3534|Synchronizer looked for a system event but did not find one to report to the client.|
|3535|Client has asked the Synchronizer to terminate operation.|
|3536|Synchronizer received an invalid message for a member of the replica set that it manages.|
|3537|The Synchronizer's client is no longer present and cannot be notified.|
|3538|Cannot initialize Synchronizer because there are too many applications running.|
|3539|A system error has occurred or your swap file has reached its limit.|
|3540|Your swap file has reached its limit or is corrupted.|
|3541|Synchronizer could not be shut down properly and is still active.|
|3542|Process stopped when attempting to terminate Synchronizer client.|
|3543|Synchronizer has not been set up.|
|3544|Synchronizer is already running.|
|3545|The two replicas you are attempting to synchronize are from different replica sets.|
|3546|The type of synchronization you are attempting is not valid.|
|3547|Synchronizer could not find a replica from the correct set to complete the synchronization.|
|3548|GUIDs do not match or the requested GUID could not be found.|
|3549|The file name you provided is too long.|
|3550|There is no index on the GUID column.|
|3551|Unable to delete the specified Registry parameter for the Synchronizer.|
|3552|The size of the Registry parameter exceeds the maximum allowed.|
|3553|The GUID could not be created.|
|3555|All valid nicknames for replicas are already in use.|
|3556|Invalid path for destination dropbox folder.|
|3557|Invalid address for destination dropbox folder.|
|3558|Disk I/O error at destination dropbox folder.|
|3559|Failure to write because destination disk is full.|
|3560|The two members of the replica set you are attempting to synchronize have the same ReplicaID.|
|3561|The two members of the replica set you are attempting to synchronize are both Design Masters.|
|3562|Access denied at destination dropbox folder.|
|3563|Fatal error accessing a local dropbox folder.|
|3564|Synchronizer cannot find the source file for messages.|
|3565|There is a sharing violation in the source dropbox folder because the message database is open in another application.|
|3566|Network I/O error.|
|3567|Message in dropbox folder belongs to the wrong Synchronizer.|
|3568|Synchronizer could not delete a file.|
|3569|This member of the replica set has been logically removed from the set and is no longer available.|
|3570|The filters defining a partial replica have been changed. The partial replica must be repopulated.|
|3571|The attempt to set a column in a partial replica violated a rule governing partial replicas.|
|3572|A disk I/O error occurred while reading or writing to the TEMP directory.|
|3573|The directory you queried for a list of replicas is not a managed directory.|
|3574|The ReplicaID for this member of the replica set was reassigned during a move or copy procedure.|
|3575|The disk drive you are attempting to write to is full.|
|3576|The database you are attempting to open is already in use by another application.|
|3577|Cannot update replication system column.|
|3578|Failure to replicate database; cannot determine whether the database is open in exclusive mode.|
|3579|Could not create replication system tables needed to make the database replicable.|
|3580|Could not add rows needed to make the database replicable.|
|3581|Cannot open replication system table '_' because the table is already in use.|
|3582|Cannot make a new replica because the _2 object in _1 container could not be made replicable.|
|3583|Cannot make the _2 object in _1 container replicable.|
|3584|Insufficient memory to complete operation.|
|3585|Cannot replicate the table; the number of columns exceeds the maximum allowed.|
|3586|Syntax error in partial filter expression on table _1.|
|3587|Invalid expression in the ReplicaFilter property.|
|3588|Error when evaluating the partial filter expression.|
|3589|The partial filter expression contains an unknown function.|
|3590|Violates the rules for partial replicas.|
|3591|Log file path '_1' is invalid.|
|3592|You cannot replicate a password-protected database or set password protection on a replicated database.|
|3593|You cannot change the data master attribute for the replica set.|
|3594|You cannot change the data master attribute for the replica set. It allows data changes only at the Design Master.|
|3595|The system tables in your replica are no longer reliable and the replica should not be used.|
|3600|Aggregation expressions cannot use GUIDs.|
|3605|Synchronizing with a non-replicated database is not allowed. The '_' database is not a Design Master or replica.|
|3607|The replication property you are attempting to delete is read-only and cannot be removed.|
|3608|Record length is too long for an indexed Paradox table.|
|3609|No unique index found for the referenced field of the primary table.|
|3610|Same table '_' referenced as both the source and destination in make-table query.|
|3611|Cannot execute data definition statements on linked data sources.|
|3612|Multi-level GROUP BY clause is not allowed in a subquery.|
|3613|Cannot create a relationship on linked ODBC tables.|
|3614|GUID not allowed in Find method criteria expression.|
|3615|Type mismatch in expression.|
|3616|Updating data in a linked table is not supported by this ISAM.|
|3617|Deleting data in a linked table is not supported by this ISAM.|
|3618|Exceptions table could not be created on import/export.|
|3619|Records could not be added to exceptions table.|
|3620|The connection for viewing your linked Microsoft Excel worksheet was lost.|
|3621|Cannot change password on a shared open database.|
|3622|You must use the dbSeeChanges option with OpenRecordset when accessing a SQL Server table that has an IDENTITY column.|
|3623|Cannot access the FoxPro 3.0 bound DBF file '_'.|
|3624|Could not read the record; currently locked by another user.|
|3625|The text file specification '_' does not exist. You cannot import, export, or link using the specification.|
|3626|The operation failed. There are too many indexes on table '_'. Delete some of the indexes on the table and try the operation again.|
|3627|Cannot find the executable file for the Synchronizer (mstran40.exe).|
|3628|Partner replica is not managed by a Synchronizer.|
|3629|Synchronizer '_1' is also using the same File System dropbox '_2'.|
|3630|Synchronizer '_1' is also using the same File System dropbox '_2'.|
|3631|Invalid Table Name In Filter|
|3632|Internet Transport not enabled on the remote Synchronizer.|
|3633|Cannot load DLL: '_'|
|3634|Cannot create a replica using a partial replica.|
|3635|Cannot create partial replica of a system database.|
|3636|Cannot populate the replica or change the replica's filter because the replica has conflicts or data errors.|
|3637|Cannot use the crosstab of a non-fixed column as a subquery.|
|3638|A Source Controlled database cannot be made replicable.|
|3639|Cannot create a replica of a System database.|
|3640|The fetch buffer was too small for the amount of data you requested.|
|3641|There are fewer records remaining in the recordset than you requested.|
|3642|A cancel was performed on the operation.|
|3643|One of the records in the recordset was deleted by another process.|
|3645|One of the binding parameters is incorrect.|
|3646|The specified row length is shorter than the sum of the column lengths.|
|3647|A column requested is not being returned to the recordset.|
|3648|Cannot synchronize a partial replica with another partial replica.|
|3649|The language-specific code page was not specified or could not be found.|
|3650|Either the Internet is very slow or there is some problem in the replication manager setup on the internet server machine.|
|3651|Invalid internet address.|
|3652|Internet login failure.|
|3653|Internet not set up.|
|3654|Internal internet failure.|
|3655|The wininet.dll cannot be loaded or initialized.|
|3656|Error in evaluating a partial expression|
|3657|Error in evaluating the boolean filter expression for table '_1'.|
|3658|Binary column '_' cannot be used in a boolean filter.|
|3659|Relationship '_1' is unenforced. Relationship in a partial filter expression must be enforced.|
|3660|Requested exchange failed because '_1'.|
|3661|Requested exchange failed because '_1'|
|3663|This operation requires a different cursor library.|
|3664|An asynchronous OpenConnection call is not yet complete; you cannot yet reference the returned connection object until it is complete.|
|3665|You cannot modify the replication system object '_'.|
|3666|You cannot modify the replication system object '_'.|
|3667|A different operation is preventing this operation from being executed.|
|3668|Cannot perform this operation because there is no active connection.|
|3669|Execution cancelled.|
|3670|Cursor is not valid.|
|3671|Cannot find table to update.|
|3672|Failed to load RDOCURS.DLL.|
|3673|This table contains cells that are outside the range of cells defined in this spreadsheet.|
|3674|Internet dll (wininet.dll) could not be found or loaded.|
|3675|Failure to read from an internet handle. Try the operation again.|
|3676|Failure to write to an internet handle. Try the operation again.|
|3677|Failure to execute the HTTP request to start internet synchronizer on the internet server. Use Replication Manager to configure internet synchronizer on the internet server.|
|3678|Failure to connect to the FTP service on the internet server. Make sure that FTP service is running properly on the server and supports anonymous connections.|
|3679|Failure to do open file using FTP service. Make sure that FTP dropbox has read permissions.|
|3680|Failure in getting a file from the server using FTP. Make sure that FTP dropbox has read permissions.|
|3681|Failure in putting a file to the server using FTP. Make sure that FTP dropbox has write permissions.|
|3682|Failure to delete a file on the server using FTP. Make sure that FTP dropbox has read and write permissions.|
|3683|Internet synchronizer exited unexpectedly on the server. Look at the partner replica exchange history on the internet server to figure out the problem.|
|3684|There is no suitable replica with which to exchange.|
|3685|Invalid HTTP address.|
|3686|Invalid replica path or name.|
|3687|Invalid SQL syntax - expected token: Option.|
|3688|Invalid SQL syntax - expected token: For.|
|3689|Invalid SQL syntax - expected token: Privileges.|
|3690|Invalid SQL syntax - expected a table right/privilege.|
|3691|Invalid SQL syntax - expected an object name.|
|3692|Invalid SQL syntax - related tokens did not match. The Microsoft Access database engine expected GRANT...TO, REVOKE...FROM, ADD...TO, or DROP...FROM.|
|3693|Invalid SQL syntax - expected user or group name.|
|3694|Invalid SQL syntax - expected token: Grant.|
|3695|Invalid SQL syntax - GRANT/REVOKE syntax error.|
|3696|Invalid SQL syntax - expected token: User (or) Group.|
|3697|Invalid SQL syntax - expected token: Password.|
|3698|Invalid SQL syntax - expected password.|
|3699|Invalid SQL syntax - expected token: User.|
|3700|Invalid precision for decimal data type.|
|3701|Invalid scale for decimal data type.|
|3702|The width of a Unicode text column must be an even number of bytes.|
|3703|Operation not supported on replicable databases that have not been converted to the current version.|
|3704|You attempted to open a database that is already opened by user '_2' on machine '_1'. Try again when the database is available.|
|3705|Cannot make the _2 table replicable - too many columns.|
|3706|Cannot make the _2 table replicable - too many indexes.|
|3707|The cascading options for the new reference conflict with existing reference '_'.|
|3708|Syntax error in Transaction statement. Expected TRANSACTION, WORK, or nothing.|
|3709|The search key was not found in any record.|
|3710|MAPI folder or address book not found.|
|3711|Recovered replicable data. This row was recovered from a corrupted replicable database. Verify that the record contents are correct and then reinsert the record, or delete this conflict record.|
|3712|Other. This record was rejected due to an undefined replication conflict problem.|
|3713|Update/update conflict. Another replica also updated this record. This record lost the conflict. Either resubmit your update or delete this conflict record.|
|3714|Locked table. This record could not be applied during synchronization since the table was locked by another user. Resubmit this conflict record.|
|3715|Unique key violation. This record has the same key value as another record, whereas only unique values are permitted. Either change the key value in this conflict record or the winning record and then resubmit this record, or delete this conflict record.|
|3716|TLV violation. This record contains a field value that does not meet the table level validation constraint. Either update the field value that is violating the validation rule and then resubmit this conflict record, or delete this conflict record.|
|3717|Delete/RI conflict. The primary key record has been deleted by another replica, therefore this referencing record has been rejected. Either create a new primary key record that satisfies the referential integrity constraint and then resubmit your update, or delete this conflict record.|
|3718|Update/RI conflict. The primary key record has been updated by another replica, therefore this referencing record has been rejected. Either create a new primary key record that satisfies the referential integrity constraint, modify the foreign key value in this conflict record to match a valid primary key value and then resubmit your update, or delete this conflict record.|
|3719|Foreign key violation resulting from an invalid primary key record that was involved in a replication conflict. Either create a new primary key record that satisfies the referential integrity constraint, modify the foreign key value in this conflict record to match a valid primary key value and then resubmit this conflict record, or delete this conflict record.|
|3720|Cannot change field '_'. It is part of one or more relationships.|
|3721|Invalid SQL syntax - expected constraint name.|
|3722|Invalid SQL syntax - expected token: DEFAULT.|
|3723|Invalid SQL syntax - expected token: COMPRESSION to follow WITH.|
|3724|Invalid SQL syntax - expected token: UPDATE or DELETE.|
|3725|Invalid SQL syntax - expected token: CASCADE, SET NULL, or NO ACTION.|
|3726|Invalid SQL syntax - expected token: NULL.|
|3727|Invalid SQL syntax - only one update rule and/or one delete rule allowed.|
|3728|Invalid SQL syntax - expected token: AS.|
|3729|Invalid SQL syntax - expected token: SELECT.|
|3730|VIEW cannot contain a parameter.|
|3731|The number of aliases specified shall be the same as the number of output columns.|
|3732|Expected query name after EXECUTE.|
|3733|The database has been placed in a state by an unknown user that prevents it from being opened or locked.|
|3734|The database has been placed in a state by user '_2' on machine '_1' that prevents it from being opened or locked.|
|3735|Too many columns in inverted index.|
|3736|Update/delete conflict. This updated record was deleted at another replica. Either reinsert this conflict record or delete it.|
|3737|Cannot create this type of replica from the given source replica.|
|3738|Local or Anonymous replicas must synch only to their designated hub replica.|
|3739|The proxy replica has been removed.|
|3740|Cannot add a new column to conflict table '_'. Delete obsolete columns and compact the database.|
|3741|Invalid partner synchronizer. Local or anonymous replica must synchronize with designated hub replica.|
|3742|An internet function timed out.|
|3743|Replica has not been synchronized within the replica set retention period.|
|3744|Counter columns in replicable tables cannot be modified.|
|3745|The combined length of Internet Server Name, HTTP Share name, and FTP alias name should not be greater than 252 characters.|
|3746|Syntax error in parameters clause. Make sure the parameter exists and that you typed its value correctly.|
|3747|Parameter has no default value.|
|3748|Parameter _ has no default value.|
|3749|The object is not a stored procedure.|
|3750|Object _ is not a stored procedure.|
|3751|Requested row locking, but DB is in page lock mode.|
|3752|Requested page locking, but DB is in row lock mode.|
|3753|Cannot create replica of a Microsoft Access database engine SQL replica.|
|3754|Cannot delete from a Prevent Deletes Replica.|
|3755|CHECK constraint '_' does not exist.|
|3756|CHECK constraint '_' already exists.|
|3757|The Microsoft Access database engine sorting DLLs could not be loaded properly.|
|3758|Scaling of decimal value resulted in data truncation.|
|3759|Scaling of decimal value resulted in data truncation.|
|3760|Scaling of decimal value resulted in data overflow.|
|3761|The decimal field's precision is too small to accept the numeric you attempted to add.|
|3762|Invalid SQL syntax - expected token: ACTION.|
|3763|Concurrent schema changes caused the create replica operation to fail. Try again.|
|3764|Failed to re-create one or more indexes.|
|3765|Syntax error in CHECK constraint clause.|
|3766|Only simple SELECT queries are allowed in VIEWS.|
|3767|Table '_' could not be made replicable because it could not be opened exclusively.|
|3768|FastFind cannot search on non-column references.|
|3769|Conflict tables cannot be renamed.|
|3770|Counter definition not in valid range.|
|3771|Local or Anonymous replicas cannot be made the design master.|
|3772|Without Administer permission, replica priority must be in the range 0 - _.|
|3773|Cannot delete one or more objects: _.|
|3774|Pin value is not valid.|
|3775|Unable to load Microsoft Access database SQL Server Reconciler - MSRPJT40.dll.|
|3776|Unable to exchange between two Microsoft Access database SQL Server Replicas.|
|3777|Illegal Operation performed on a Microsoft Access database SQL Server Replica.|
|3778|The Microsoft Access database is wrong or missing for this SQL/Microsoft Access database replica set.|
|3779|Unable to change Column Level Tracking Property on objects that are already Replicable.|
|3780|Invalid SQL syntax - expected a view name.|
|3781|Invalid SQL syntax - expected a procedure name.|
|3782|Invalid SQL syntax - currently only one column-level CHECK constraint is allowed.|
|3783|Invalid SQL syntax - cannot use multiple columns in a column-level CHECK constraint.|
|3784|Database is already replicable.|
|3785|Invalid SQL syntax - expected token: Database.|
|3786|Invalid SQL syntax - expected a database privilege, such as CREATEDB or CONNECT.|
|3787|This operation is not allowed in subqueries.|
|3788|Cannot create an index on this MAPI folder/addressbook.|
|3789|Illegal column-level constraint.|
|3790|This object requires a newer version of the Microsoft Access database engine.|
|3791|This index requires Microsoft Access database engine _.|
|3792|The index '_2' requires Microsoft Access database engine _1.|
|3793|The column '_2' requires Microsoft Access database engine _1.|
|3794|This table requires Microsoft Access database engine _.|
|3795|The table '_2' requires Microsoft Access database engine _1.|
|3796|The version of AceRecr.DLL that was loaded was too old. Run setup again to get the correct version of this file.|
|3797|The SQL/Microsoft Access database engine exchange failed, look in the SQLServer Agent history for details.|
|3798|CHECK constraints are not allowed on replicable databases.|
|3799|Could not find field '_'.|
|3800|'_' is not an index in this table.|
|3801|The object (_) cannot be used in a CHECK constraint clause.|
|3802|Error evaluating _ CHECK constraint. _|
|3803|DDL cannot be completed on this table because it is referenced by constraint _ on table _.|
|3804|There are no MAPI clients installed on this machine. Install a MAPI client (like Outlook) on this machine.|
|3805|CHECK constraints on table _ will not be transferred to this table. CHECK constraints can only be created via SQL DDL statements.|
|3806|Cannot set multiple NULL, NOT NULL attributes.|
|3807|Query _ contains an ambiguous column name that is conflicting with the correlation (alias) name _. Either fully qualify the column name or change the correlation (alias) name.|
|3808|Need a version 4.x or greater format system database to perform this operation.|
|3809|Query _ was created with a later release of Microsoft Access database engine and may have syntax that this version of Microsoft Access database engine cannot execute. This query can only be executed with the version of Microsoft Access database that created it.|
|3810|Unrecognized keyword WHEN.|
|3811|Query could not be executed.|
|3812|You cannot update this field because the value you're trying to apply is not valid or would break a data integrity rule. Please correct and try again.|
|3813|SQL pass through queries are disabled.|
|3814|Columns that accept multiple values for a record cannot be included in a multiple-column relationship.|
|3815|Columns that accept multiple values for a record cannot be included in a multiple-column index.|
|3816|The unique index on the '_' column cannot be deleted.|
|3817|The multi-valued field '_' is not valid in a CROSSTAB query.|
|3818|Operation cannot be completed on this database because it uses attachments or multi-valued lookup fields.|
|3819|Find cannot be executed on columns that accept multiple values for a record.|
|3820|You cannot enter that value because it duplicates an existing value in the multi-valued lookup or attachment field. Multi-valued lookup or attachment fields cannot contain duplicate values.|
|3821|You cannot change a multi-valued lookup field to the selected data type.|
|3822|The value cannot be added to this new row until the row has been committed. Commit the row first, and then try adding the value.|
|3823|You cannot edit this field because it resides in a linked Excel spreadsheet. The ability to edit data in a linked Excel spreadsheet has been disabled in this Access release.|
|3824|An INSERT INTO query cannot contain a multi-valued field.|
|3825|SELECT * cannot be used in an INSERT INTO query when the source or destination table contains a multi-valued field.|
|3826|An UPDATE or DELETE query cannot contain a multi-valued field.|
|3827|Cannot perform an aggregate function on a multi-valued column when a JOIN clause contains a different multi-valued column.|
|3828|Cannot reference a table with a multi-valued field using an IN clause that refers to another database.|
|3829|The multi-valued field '_' cannot be used in an ORDER BY clause.|
|3830|The multi-valued field '_' cannot be used in an GROUP BY clause.|
|3831|The multi-valued field '_' cannot be used in a WHERE or HAVING clause.|
|3832|The current file format no longer supports user-level security. The conversion or compaction process has removed any user-level permissions.|
|3833|The multi-valued field '_' is not valid in the specified JOIN clause.|
|3834|The multi-valued field '_1' is not valid in the expression '_2'.|
|3835|The DISTINCT keyword cannot be used with the multi-valued field '_'.|
|3836|You are attempting to work with a Paradox file that requires the Borland Database Engine (BDE). Microsoft Access cannot load the BDE, or the BDE is not correctly installed. To correct this, run the setup program for the BDE.|
|3837|The multi-valued field '_' cannot be used in a UNION query.|
|3838|Multi-valued fields are not allowed in SELECT INTO statements.|
|3839|The specified file exists already.|
|3840|The attachment you are adding exceeds the system resources available.|
|3841|Cannot connect to the SharePoint site '_'. Try again later.|
|3842|The expression '_' must be part of an aggregate function.|
|3843|An INSERT INTO query that contains a multivalued field cannot contain another field.|
|3844|Cannot reference a multi-valued field in an UPDATE or DELETE statement that contains other fields.|
|3845|Microsoft Access does not support linking to an Access database or Microsoft Office Excel workbook saved in a format that is a later version than the current database format.|
|3846|The multi-valued field '_' in an ORDER BY must also appear in the SELECT list.|
|3847|ODBCDirect is no longer supported. Rewrite the code to use ADO instead of DAO.|
|3848|You cannot record your changes because a value you entered violates the settings defined for field '_' (for example, a value is less than the minimum or greater than the maximum). Correct the error and try again.|
|3849|Replication is only supported on Access 2000 or Access 2002-2003 format databases. Use the conversion feature in your application to convert the database to the proper format.|
|3850|This file was created in a previous beta version of Excel 2007. Open the file with Excel 2007 to save it to the most recent version of the Excel 2007 file format before opening the file in Access 2007 or greater.|
|3851|The schema for this table has changed. You must refresh the table before editing or adding new records.|
|3852|Cannot update a multivalued field if its parent recordset is not in edit mode. To place the parent recordset in edit mode use the AddNew or Edit method.|
|3853|Microsoft Access cannot create the database in the specified format because of a Group Policy set by your administrator. Create the database using a different file format or contact your administrator.|
|3854|This operation will fail because the text file you are about to import contains more than 255 columns. We recommend that you first make a backup copy of your source file, reduce the number of columns to 255 or less, and then try again.|
|3855|The Microsoft Access database engine cannot read the data in _1. The minimum required version to read the data is _2.|
|3856|The Microsoft Access database engine cannot update the data in _1. The minimum required version to update the data is _2.|
|3857|The Microsoft Access database engine cannot change the design of _1. The minimum required version to change the design is _2.|
|3858|Expression not supported for conversion|
|3859|The expression _ cannot be used in a calculated column.|
|3860|The expression _ cannot be converted in a query.|
|3861|The expression _ cannot be converted in the results of a query.|
|3862|The expression _ cannot be converted in a UI macro.|
|3863|The expression _ cannot be converted in a default value.|
|3864|The expression _ cannot be converted in a validation rule.|
|3865|The expression cannot be saved because it contains a circular reference.|
|3866|The expression cannot be saved because it refers to itself.|
|3867|The expression cannot be saved because it refers to another table.|
|3868|There is no data macro AXL stored for the specified table.|
|3869|The data macro AXL is incompatible with the server with which you are trying to synchronize.|
|3870|Microsoft Access cannot interpret the text you are pasting as a data macro. Correct the text and then try again.|
|3871|The specified argument is invalid.|
|3872|A data macro resource limit was hit. This may be caused by a data macro recursively calling itself. The Updated() function may be used to detect which field in a record has been updated to help prevent recursive calls.|
|3873|The '_1' action failed because it is not supported from a '_2' event.|
|3874|Type mismatch on field '_'.|
|3875|The data macro '_' could not be found.|
|3876|The 'ExitForEachRecord' action is only available in a ForEachRecord loop.|
|3877|The 'SendEmail' action failed because no recipients were specified.|
|3878|The 'SendEmail' action failed because the current database is not trusted.|
|3879|The password is invalid; the message was not sent.|
|3880|The mail session could not be opened. You may be out of memory. Close other applications, and then try again. You may also want to check your mail application to ensure that it's working correctly.|
|3881|The e-mail could not be sent. Please ensure that your computer is configured to send and receive e-mail messages.|
|3882|This calculated column needs to be recalculated.|
|3883|This calculated column contains an invalid expression.|
|3884|The Microsoft Access database engine encountered an error while connecting to Data Services: '_'.|
|3885|The type of field '_' cannot be used as part of a calculated column. Examples of unsupported types include multi-value and binary fields.|
|3887|A calculated column cannot be saved without a valid expression in the Expression property.|
|3888|Calculated columns cannot be used in system relationships.|
|3889|The variable '_' could not be found.|
|3890|The identifier '_' could not be found.|
|3891|Queries that contain linked tables, action queries, and database references are not allowed in data macros.|
|3892|The function '_' is not valid for expressions used in data macros.|
|3893|Invalid constant '_' in data macro expression.|
|3894|Invalid text '_' in data macro expression.|
|3895|You cannot record your changes because a value you entered violates the settings defined for field '_' (for example, a value is less than the minimum or greater than the maximum). Correct the error and try again.|
|3896|The limit of 20 recursive events has been exceeded. This can be caused by an After Update event that changes data on the table that triggers it. You can use the Update() function to determine which fields were changed in the record.|
|3897|The alias '_' could not be found.|
|3898|There is no data context in which to perform the action. This error can be caused by using the RunDataMacro action to run a data macro that calls DeleteRecord or EditRecord with no alias specified.|
|3899|The list item could not be inserted or updated because duplicate values were found for one or more fields in the list.|
|3900|The delete operation is not allowed because a child record still exists.|
|3901|At least one relationship lookup field points to non-existent items in this list.|
|3902|Cached links cannot be added because the database '_' cannot be opened in exclusive mode.|
|3903|The database '_' cannot be opened in exclusive mode because it contains links in cached mode.|
|3904|A table cannot be updated because there is no connection to a SharePoint site. Try the update again when the connection to the server is restored.|
|3905|The table '_' cannot be updated because there is no connection to a SharePoint site. Try the update again when the connection to the server is restored.|
|3906|Linked table is unavailable. Microsoft Access cannot contact the server. Check your network connection or contact the server administrator.|
|3907|Linked table '_' is unavailable. Microsoft Access cannot contact the server. Check your network connection or contact the server administrator.|
|3908|Data cannot be added, updated or deleted until all related data has been fetched from the server.|
|3909|Data cannot be added, updated or deleted because data table '_' has not been fetched from the server.|
|3910|Your encryption settings are not valid. Re-install Microsoft Access or contact your administrator for more information.|
|3911|Microsoft Access cannot open this file because the contents have been damaged.|
|3912|A table is not synchronized with the SharePoint site. Close and reopen the table to retrieve the most recent data from the SharePoint site.|
|3913|The table '_' is not synchronized with the SharePoint site. Close and reopen the table to retrieve the most recent data from the SharePoint site.|
|3914|Selected collating sequence not supported with the specified file format.|
|3915|The expression _ cannot be converted in a data macro.|
|3916|The property '_' can only be set or changed by the Microsoft Access database engine.|
|3918|Microsoft Access does not support this operation for this IISAM type. To perform this action, you must use the 2007 Microsoft Office system or earlier.|
|3919|The Microsoft Access database engine encountered an error while communicating with SharePoint. More detailed information: '_'|
|3920|Unable to execute query. Invalid operation or syntax using multi-value field.|
|3921|Cannot reference a table with a multi-valued field using a FROM clause that refers to another database.|
|3922|The expression _ cannot be converted in a form.|
|3923|The inclusion of constant expressions in an outer join operation is not supported.|
|3924|Access was unable to convert the query for use on the Web.|
|3925|The definition of the query is invalid, so the query object cannot be created.|
|3926|Access was unable to convert the query for use on the Web because it uses an unsupported query type, unsupported expressions, unsupported criteria, or other features that are not supported on the Web.|
|3927|Access was unable to convert the query for use on the Web because it contains a sub-query.|
|3928|Access was unable to convert the query for use on the Web because it relies on a different query that is not Web compatible.|
|3929|Access was unable to convert the query for use on the Web because it doesn't include any fields in its results.|
|3930|Access was unable to convert the query for use on the Web because it displays too many fields in its results.|
|3931|Access was unable to convert the query for use on the Web because it relies on an ORDER BY clause that is not supported on the Web.|
|3932|Access was unable to convert the query for use on the Web because it specifies a JOIN type that is not supported on the Web.|
|3933|Access was unable to convert the query for use on the Web because it is a cross-product query.|
|3934|Access was unable to convert the query for use on the Web because it does not specify which table to SELECT FROM.|
|3935|Access was unable to convert the query for use on the Web because the query results contain multiple fields with the same name.|
|3936|Access was unable to convert the query for use on the Web because some of its parameters could not be converted for use on the Web.|
|3937|Access was unable to convert the query for use on the Web because some of its parameters are displayed as result fields or used in ORDER BY statements.|
|3938|The expression could not be saved because its result is an invalid type, such as NULL.|
|3940|Could not coerce argument number _1 to an integer for the _2 action.|
|3941|Could not coerce argument number _1 to a string for the _2 action.|
|3942|Could not coerce argument number _1 to a Boolean for the _2 action.|
|3943|Missing parameter '_1' when attempting to run a named data macro.|
|3944|The type of field '_' cannot be used in a validation rule for Web tables. Invalid types include Memo, Binary and Lookup.|
|3945|System tables cannot contain data macros.|
|3946|The OnError action cannot be used with Go To set to Macro Name in before events.|
|3947|The server does not support the type of field '_' as part of a calculated column. Examples of unsupported types include multi-value, memo, hyperlink, binary, AutoNumber and lookup fields.|
|3948|The expression could not be saved because its result type, such as binary or NULL, is not supported by the server.|
|3949|The Microsoft Access database engine cannot compact the data in _1 because it contains objects from a newer version.|
|3950|Syntax error in expression.|
|3951|The application has changed on the server. Your last entered row will not be saved. Copy and paste your last entered row to a temporary file and then synchronize the application with the server.|
|3952|The data macro failed to run because its AXL definition was invalid.|
|3953|The local var name '_1' is invalid. Local variable names must be less than or equal to 64 characters in length, may not start with an equal sign or a space, and may not contain any of the following characters including CR, LF or TAB: .![]/\:*?"<>_2#{}%&.|
|3954|The parameter name '_1' is invalid. Parameter names must be less than or equal to 64 characters in length, may not start with an equal sign or a space, and may not contain any of the following characters including CR, LF or TAB: .![]/\:*?"<>_2#{}%&.|
|3955|The data macro name '_1' is invalid.|
|3956|The database you are trying to open requires a newer version of Microsoft Access.|
|3957|The field could not be added on the server. Synchronize with the server and remove any references to hidden, lookup, AutoNumber, hyperlink and memo fields.|
|3958|The field could not be deleted on the server. Synchronize with the server and verify that no other field references this field.|
|3959|Calculated columns are not allowed in SELECT INTO statements.|
|3960|The expression _ cannot be converted for the Apply Filter macro action.|
|3961|The maximum number of parameters for named data macros, ForEachRecord, and LookupRecord is 255.|
|3962|EditRecord failed due to repeated data conflicts when attempting to commit data.|
|3963|The action '_1' is not valid inside of CreateRecord or EditRecord.|
|3964|The action '_1' is not valid outside of CreateRecord and EditRecord.|
|3965|The field '_1' could not be changed because the record is not currently updatable. Use EditRecord to make the record updatable.|
|3966|The field '_1' could not be changed. In the BeforeChange event only the record being changed is updatable.|
|3967|EditRecord failed because the alias '_1' represents a record which is read only.|
|3968|EditRecord failed because the default alias represents a record which is read only.|
|3969|Attachments cannot be added to new rows until the rows have been committed to the server. Commit the row first, and then try attaching the document to it.|
|3970|The expression could not be saved because it would be longer than 255 characters when published.|
|3971|Linked table cannot be opened. You do not have permission to view the entire list because it is larger than the list view threshold enforced by the server administrator.|
|3972|Linked table '_' cannot be opened. You do not have permission to view the entire list because it is larger than the list view threshold enforced by the server administrator.|
|3973|Values stored in multi-value and attachment fields are not supported within data macro queries.|
|3974|The field '_1' could not be read because it is a multi-value or attachment field.|
|3975|The field '_1' could not be set because it is a multi-value or attachment field.|
|3976|The column '_1' could not be read because it is a calculated column in a record that was retrieved from ForEachRecord or LookupRecord and has been modified by EditRecord.|
|3977|URL data '_1' in field '_2' could not be sent to the server. The URL may be invalid or longer than 255 characters.|
|3978|The calculation of day or time difference with a + or - sign is not supported. Please use the DateAdd() function instead.|
|3979|The linked table has been changed on the server. Your last entered row will not be saved. Copy and paste your last entered row to a temporary file, then synchronize the linked table with the server by right-clicking on the linked table, choosing More options and clicking Refresh List.|
|3980|The linked table '_' has been changed on the server. Your last entered row will not be saved. Copy and paste your last entered row to a temporary file, then synchronize the linked table with the server by right-clicking on the linked table, choosing More options and clicking Refresh List.|
|3981|There were errors executing the bulk query or sending data to the server. Reconnect the tables to resolve the conflicts or discard the pending changes.|
|3982|The field '_1' could not be read because it is the primary key of the record being created. This field's value will be available in the LastCreateRecordIdentity variable after the CreateRecord block has completed.|
|3983|An argument to the Updated function was invalid. The field name must be provided as a string value enclosed in quotation marks.|
|3984|Access was unable to convert the query for use on the Web because it contains a cyclical join.|
|3985|Within a ForEachRecord only the records of the outermost ForEachRecord may be edited or deleted.|
|3986|CreateRecord cannot be used inside of a ForEachRecord.|
|3987|Data cannot be inserted because there is no matching record.|
|3988|The table is too large for this change to be saved.|
|3989|You don't have permission to modify the design of this list.|
|3990|A data macro on table '_1' failed to execute, since it has not yet been synchronized with the server. Please synchronize this table with the server using the "Sync All" command and try again.|
|3991|The query failed to execute because the identifier '_1' could not be found.|
|3992|The validation rule contains a syntax error and cannot be saved. A field or function may be misspelled or missing.|
|3993|A timeout prevented the change from saving to the server.|
|3994|The field '_' cannot be modified because it contains invalid characters, invalid punctuation, or an existing field name. Correct the name on the server and refresh the table.|
|3995|This table already has the maximum number of fields with the data type specified. For the purposes of this calculation, Number and Currency are considered the same data type.|
|3996|Microsoft Access cannot reconnect one or more of the disconnected tables. Please check network connectivity and server availability.|
|3997|Microsoft Access detected changes to an open object. Please close and reopen the object to get updates.|
|3998|The Updated function is not supported for memo, rich text, hyperlink, OLE Object, multi-value, or attachment fields.|
|3999|You cannot reference rows created when you are disconnected from the server because this violates the lookup settings defined for this table or list. Please reconnect all tables with the server and try again.|
|4001|One or more values are prohibited by the validation rule set for '_1'. Enter a value that the expression for this field can accept.|
|4002|The '_1' value is not available because the field is of type memo, rich text, hyperlink, or OLE Object.|
|4003|The maximum number of return variables is 255.|
|4004|The return variable name '_1' is invalid. Return variable names must be less than or equal to 64 characters in length, may not start with an equal sign or a space, and may not contain any of the following characters, including CR, LF or TAB: .![]/\:*?"<>_2#{}%&.|
|4005|The return variable '_1' is too long. The return variable string cannot exceed _2 characters.|
|6000|Errors were encountered during the save operation. |
|6001|A form or report cannot be the subdatasheet of a table or query. Only a table or query can be inserted into another table or query. |
|6002|Microsoft Access cannot expand this subdatasheet because all of the records are locked.  Either the RecordLocks property of the form or report or the Default Record Locking option in the Advanced section of the Access Options dialog box (click the File tab, and then click Access Options) is set to All Records. Reset the value to No Locks or Edited Record, as appropriate.|
|6003|The setting for the Precision property must be from 1 through 28. |
|6004|The setting for the Scale property must be from 0 through 28. |
|6005|The table or query name '_' you entered in either the property sheet or macro is misspelled or refers to a table or query that doesn't exist. If the invalid name is in a macro, an Action Failed dialog box will display the macro name and the macro's arguments after you click OK. Open the Macro window, and enter the correct name. |
|6006|Filter by selection of a partial value is only supported for fields containing character data. |
|6007|Cannot open a form bound to a stored procedure in server filter by form mode. |
|6008|Microsoft Access encountered an error closing your connection.  Please close all of your application windows before trying again.|
|6009|Invalid connection string. You may need to specify a valid connection string and try it again. |
|6010|Invalid use of Move method. The Move method is not applicable to subforms or subreports. |
|6011|Do you want to save the data changes on form '_'? |
|6012|Microsoft Access can't set this property to No right now. You cannot set the Allow Form View, Allow Datasheet View, Allow PivotTable View, and Allow PivotChart View properties to No at the same time. Set one of the other properties to Yes before changing this one to No.  |
|6013|Unable to remove item. '_' not found in list. |
|6014|The RowSourceType property must be set to 'Value List' to use this method. |
|6015|Can't add this item. The index is too large. |
|6017|Microsoft Access doesn't support the requested property for this type of database object. The DateCreated and DateModified properties are not supported for Tables, Queries, Stored Procedures, Database Diagrams, and Functions in a Client Server database. |
|6018|The form or report template is open in Design view. Before creating a new form or report, close the corresponding template. |
|6020|This format does not support objects named '_'. Rename the object and try again. |
|6021|Microsoft Access cannot build a form or report based on the data returned by the function object that you have currently selected.  Select a table, query, form, or report, and try again.|
|6023|The Maximum Record Count setting cannot be a negative number. |
|6024|The Maximum Record Count setting cannot contain text - please enter a number. |
|6025|The Requery action cannot be used on a control bound to a Recordset. |
|6026|The RowSourceType property must be set to 'Table/Query' to use this method. |
|6028|The OLE object failed to persist itself. You will not be able to undo this operation. |
|6029||
|6030|The current record in _1 _2 must be saved before you can complete this operation. Do you want to save the current record in _1 _2? |
|6031|You can't modify the structure of table '_', because it is already in use by another person or process. |
|6032|The value you entered isn't valid for this field. For example, you may have entered a null value. |
|6033|The '_1' form does not exist. Enter the name of an existing form in the List Items Edit Form property of the '_2' control or leave the property empty. |
|6034|This property is only available for attachment controls bound to attachment fields. |
|6035|Layout view is unavailable for this _. |
|6036|You cannot add a form to another form that shows multiple items. |
|6037|This object was saved in an invalid format and cannot be read. |
|6038|Microsoft Access was not able to update the value list. The table may be locked by another user, it may be read-only, or you may not have permission to modify the table. |
|6039|Do you want to save the datasheet so that it will be available for use by other forms and reports? |
|6040|Because dependency checking is not enabled on this database, Access cannot verify whether this datasheet is used on other forms or reports. Do you want to save the datasheet so that it will be available for use by other forms and reports? |
|6041|Grouping information can only be changed in Layout View and Design View. |
|6042|Deleting the _1 control will permanently remove filter, grouping, or sort order settings that are based on the control from the form or report. Are you sure you want to delete the _1 control? |
|6043|Deleting the selected controls will permanently remove filter, grouping, or sort order settings that are based on those controls from the form or report. Are you sure you want to delete the controls? |
|6044|You will not be able to undo the creation or deletion of this object. Do you still want to continue? |
|6045|Microsoft Access cannot save the form or report, because it displays characters from a language that cannot be saved in your current system locale. Switch your system locale to the language in the form or report, and then try again. |
|6046|Your changes cannot be saved because the field '_' has been updated by another user.|
|6047|The number of fields from your currently selected table or query exceeds the maximum height in Microsoft Access forms. Microsoft Access will create a new form with no fields and open the field list so you can individually select fields to add to this object. |
|6048|The number of fields from your currently selected table or query exceeds the maximum width in Microsoft Access forms and reports. Microsoft Access will create a new form or report with no fields and open the field list so you can individually select fields to add to this object. |
|6049|Since databases in the current format do not support the Attachment control, you will not be able to complete this operation.|
|6050|Microsoft Access could not instantiate the Web Browser Control.|
|6051|Web Browser could not parse the transform|
|6052|Web Browser cannot apply the transform.|
|6053|To apply this change, close and reopen the form or report|
|6054|The macro action BrowseTo requires a valid Path argument. A valid Path argument is of the form: MainForm1.Subform1>Form1.Subform1|
|6055|This form already has a navigation control. You will not be able to complete this operation. |
|6056|The Web browser control is not supported in a report or a continuous form.|
|6057|Because databases in the current format do not support the Empty Cell control, you will not be able to complete this operation.|
|6058|The Navigation control is not supported by the current database format.|
|6059|Cannot create a Web _ based on a source that is not Web compatible.|
|6060|The navigation control is not supported on reports.|
|6061|Some controls on this object shared the same name. These controls have been given unique names. |
|6062|You must be in Design or Layout View to create or delete controls. |
|6063|The report position change was stopped|
|6064|Invalid menu command.|
|7700|Microsoft Access is unable to complete the operation. Try the operation again. If the error persists, restart Microsoft Access. |
|7701|Microsoft Access can't synchronize with Synchronizer '_1’. A possible reason for the failure to synchronize with '_1' is that both the Synchronizer and Microsoft Access were trying to write to the current database at the same time. Try synchronizing with '_1' again. Do you want to continue synchronizing with the remaining Synchronizers? |
|7702|The user-supplied function named '_', the program to assist you in resolving conflicts, could not be found. Contact the author of this customized database application. |
|7703|There are no synchronization conflicts to resolve. |
|7704|You can't modify the design of '_' at a replica. Design changes to replicated objects can be made only at the Design Master. Do you want to open it as read-only? |
|7705|This member of the replica set has conflicts from synchronizing changes with other members. Do you want to resolve these conflicts now? |
|7708|'_1' was the last designated Design Master for the replica set. Has '_2' been moved, renamed, deleted, or corrupted?  You should never have more than one Design Master for each replica set. Having more than one Design Master in a replica set prevents the members of the set from synchronizing correctly. Use Windows Explorer to determine whether the file has been moved or deleted. Open the file to determine whether it has been corrupted.|
|7709|To make this replica the Design Master for the replica set, on the Database Tools tab, in the Database Tools group, click Replication Options, click Synchronize Now, enter the path to '_' (the current Design Master), and then select the Make ... The Design Master check box. |
|7710|To make this replica the Design Master for the replica set, first synchronize this replica with all other replicas in the set. This ensures that this replica includes all design changes made at the previous Design Master. If you have already synchronized this replica, do you want to make it the Design Master? |
|7711|This member of the replica set is now the Design Master. Microsoft Access will now close and reopen the database in order for the changes to take effect. |
|7712|This member of the replica set has exceeded the maximum number of days allowed between synchronizations and can't be synchronized with any other member of the replica set. Delete this replica set member and create a new replica. |
|7713|This member of the replica set will expire in _ days because it has not been synchronized with another member of the replica set. If the member is allowed to expire, it can no longer be synchronized with any other member of the replica set. You should synchronize this member with another member as soon as possible. To synchronize, on the the Database Tools tab, in the Database Tools group, click Replication Options, and then click Synchronize Now.  |
|7714|Microsoft Access cannot close the database at this time. Make sure there is no Visual Basic code executing in the current database. You may need to run Setup again to properly install Briefcase Replication. |
|7715|Microsoft Access cannot complete this operation because it can't find or initialize the dynamic-link library AceRclr. Rerun Microsoft Access or Microsoft Office Setup again to reinstall Microsoft Briefcase Replication. During Setup, click Add/Remove, and select Microsoft Briefcase Replication. If you want to preserve your security or custom settings, back up the Microsoft Access workgroup information file. For information on backing up files, search the Microsoft Windows Help index for 'backing up files'. |
|7716|The database must be closed prior to synchronization. Do you want Microsoft Access to close the database and synchronize with '_1'? |
|7717|All open objects must be closed before synchronizing. Do you want Microsoft Access to close the objects? |
|7718|Microsoft Access can't synchronize this member of the replica set because one or more objects are open. Because synchronizing may involve updating the data in or design of the database, all objects must be closed before you synchronize. Close all objects and try again.  |
|7719|You can't save design changes to '_' at a replica. Design changes to replicated objects can be made only at the Design Master. |
|7720|You can't save design changes to '_1' at a replica. Design changes to replicated objects can be made only at the Design Master. Do you want to save it as a new, local object? |
|7721|You can't delete or rename '_' at a replica. These operations can't be performed on a replica; they can be performed only at the Design Master. |
|7724|Microsoft Access has converted '_1' to the Design Master for the replica set. Only the Design Master can accept changes to the database structure; however, data changes can be made at the Design Master or any replica. |
|7725|Microsoft Access has converted '_1' to the Design Master for the replica set and has created a replica at '_2'. Only the Design Master can accept changes to the database structure; however, data changes can be made at the Design Master or any replica. |
|7726|Microsoft Access has created a replica at '_2'. |
|7727|Microsoft Access can't create a replica because this database is already open in exclusive mode. Do you want Microsoft Access to close the database? |
|7728|Microsoft Access can't overwrite '_1': the replica can't be created at this location. The file you are trying to overwrite may be open. Close any other applications that might be using this file.  |
|7729|Microsoft Access can't make a new replica at '_1' because the source has the same path and file name. Choose a different path or file name for the new replica. |
|7730|The synchronization was completed successfully. |
|7731|Microsoft Access cannot synchronize this member of the replica set because it is open in Exclusive mode.  To open the database in Shared mode, close and reopen the database.|
|7732|'_' is saved as a local table. Changes made to the table will not be sent to the replicas in the set. To make this table available to other members of the replica set, close the table, right-click it in the Navigation Pane, click Table Properties on the shortcut menu, and then select the Replicated check box. |
|7733|The synchronization request has been received, and the Synchronizer will complete the process as resources become available. Make sure that the Synchronizer for this member of the replica set is running. It may be several minutes before the synchronization will occur. |
|7734|Microsoft Access has saved the design of table '_1,' but it will not be made replicable until the Table window is closed. |
|7735|Changes to this object can be made only at the Design Master. Any changes you make will be discarded when the form is closed. To save your changes, click the File tab, point to Save As, and then save the object under a different name.  |
|7737|Microsoft Access has converted '_1' to the Design Master for the replica set and has created a replica at '_2'. The new replica will not appear in the list of potential synchronization partners until this database is closed and reopened. Do you want Microsoft Access to close and reopen the database now? |
|7738|Microsoft Access has successfully created a replica at '_2'. However, the new replica will not appear in the list of potential synchronization partners until this database is closed and reopened. Do you want Microsoft Access to close and reopen the database now? |
|7739|Microsoft Access has deleted '_1' and removed it from the replica set. This change isn't reflected until the database is closed and reopened. Please close and reopen the database when you are finished synchronizing replicas.  |
|7740|Microsoft Access cannot synchronize with a database from a previous version. Convert the database you are trying to synchronize with, before synchronizing. |
|7741|Microsoft Access can't make a new replica at '_1' because the value entered for priority is out of range. Priority for new replicas should be in the range of 0-100. |
|7742|Microsoft Access cannot replicate a password-protected database.|
|7743|Backup, restore, and drop database operations are available only when you have a version of Microsoft SQL Server supported by Microsoft Access installed on your local computer. See the Microsoft Office Update Web site for the latest information and downloads.|
|7744|Microsoft Access can't make a new replica at '_1' because the value entered for priority is out of range. Priority for anonymous replicas should be 0. |
|7745|You can't copy local objects in a replica. Design changes to replicated objects can be made only at the Design Master.|
|7746|The Database administrative components failed to load or initialize. Verify that the components are installed and registered locally.|
|7747|Database Replicas cannot be converted to prior versions of Microsoft Access.|
|7748|This stage of the conversion process is finished. To complete the conversion process, synchronize this replica to the (converted) Design Master.|
|7749|Open the Design Master and sync to this replica before opening.|
|7750|In Datasheet view, you can't set a control property if the control is part of an option group. |
|7751|In Design view, you can't retrieve the value of the ObjectPalette property for an OLE object contained in a bound object frame. Microsoft Access doesn't display the OLE object in the object frame in Design view. Remove the reference to the ObjectPalette property, or switch to Form view before you run the macro or Visual Basic code that references the ObjectPalette property.  |
|7752|Microsoft Access cannot apply the filter because all of the records are locked. Either the RecordLocks property of the form or report or the Default Record Locking option in the Advanced section of the Access Options dialog box (click the File tab, and then click Access Options) is set to All Records. Reset the value to No Locks or Edited Record, as appropriate.  |
|7753||
|7754|You cannot position columns between two frozen columns in a datasheet. To unfreeze all columns, on the Home tab, in the Records group, click More, and then click Unfreeze All Columns. |
|7755|This feature isn't installed, or has been disabled. To install this feature, rerun the Microsoft Access or the Microsoft Office Setup program or, if you're using a third-party add-in, reinstall the add-in. To reenable this wizard, click the File tab, and then click Access Options. Click Add-Ins, and then in the Manage list, click Disabled Items, and then click Go. |
|7758|This ActiveX control isn't enabled in Form view. You can't set the Enabled or TabStop properties to True. |
|7759|This ActiveX control isn't visible in Form view. You can't set the Visible or TabStop properties to True. |
|7760|This property is locked and can't be changed. |
|7761|An error occurred when you changed the control. |
|7762|The pattern string is invalid. |
|7763|This feature isn't installed, or has been disabled. To install this feature, rerun the Microsoft Access or the Microsoft Office Setup program or, if you're using a third-party add-in, reinstall the add-in. To reenable this wizard, click the File tab, and then click Options. Click Add-Ins, and then in the Manage list, click Disabled Items, and then click Go. |
|7766|This control can't be changed to the type you requested. |
|7767|An error occurred while Microsoft Access was creating a section. |
|7768|In order to change data through this form, the focus must be in a bound field that can be modified. |
|7769|The filter operation was canceled. The filter would be too long. |
|7770|Microsoft Access didn't apply the filter. Microsoft Access may not be able to apply the filter if you entered an invalid data type in one of the fields. Do you want to close the filter anyway?  If you click Yes, Microsoft Access will build the filter, but won't apply it to the recordset. Then it will close the Filter By Form window.|
|7771|You can't set the ColumnOrder property when you are in Form view or Print Preview. |
|7773|Microsoft Access can't set the LimitToList property to No right now. The first visible column, which is determined by the ColumnWidths property, isn't equal to the bound column. Adjust the ColumnWidths property first, and then set the LimitToList property.  |
|7774|You can't set the MenuBar property while an OLE object is in-place active. |
|7775|There are too many controls on the form to allow Filter By Form. |
|7777|You've used the ListIndex property incorrectly. |
|7778|You can't save an object when you are in the Filter By Form window. Switch to Form view, and then save the object. |
|7779|You cannot set a form's MenuBar property from its menu bar macro. |
|7780|You're trying to insert into a section that can't grow enough for the control you're trying to add. The maximum total height for all sections in a report, including the section headers, is 200 inches (508 cm). Remove, or reduce the height of, at least one section. Then try to add the control again.  |
|7782|You can't create a new instance of this form or report while it is in Design view. |
|7784|The object you have selected is already open for design as a subform or subreport. To open this object in Design view, select the subform or subreport in the object in which it is already open, and then on the Design tab, in the Tools group, click Subform in New Window.|
|7785|Microsoft Access didn't build the filter. There is an error in the current field. Do you want to close the filter anyway?  If you click Yes, Microsoft Access will undo the changes to the filter, and then close the Filter By Form window.|
|7789|Type mismatch. |
|7790|Microsoft Access can't create the _1. Either there was an error getting information from the table or query you selected, or a new _2 could not be created. |
|7791|There was an error creating your _1. Some fields may have been skipped because there was an error getting information about them or because they didn't fit on the _2. |
|7792|You can't open a subform or subreport when it is also open in Design view. |
|7793|You must save the form '_' before you can embed it. |
|7794|Microsoft Access could not find the toolbar '_1'. |
|7795|The custom toolbar '_' is the wrong type (menu, shortcut menu, or toolbar) for the property it's being used in. |
|7796|The process failed because there is no printer installed. Because Microsoft Access saves the printer settings with each form or report, a printer is needed to convert, enable, copy and paste, import, or export your forms or reports. To install a printer: In Microsoft Windows XP, click Start, and then click Printers and Faxes. Under Printer Tasks, click Add a printer. In Microsoft Windows 2000, click Start, point to Settings, and then click Printers. Double-click Add Printer. Follow the instructions in the wizard. |
|7797|You must save new objects in the database before you can save them to this external format. |
|7798|You can only save select, crosstab, and union queries to this format. |
|7799|This form or report is based on a query that exceeds the limit for data in a single record. Exclude any unnecessary fields from the query, or change some of the field types to Memo in the original tables. |
|7800|You can't create a Microsoft Access MDE database from a database in an old format. Close the database and convert it to the current version of Microsoft Access. Then create the MDE database. |
|7801|This database is in an unrecognized format. The database may have been created with a later version of Microsoft Access than the one you are using. Upgrade your version of Microsoft Access to the current one, then open this database. |
|7802|The command you specified is not available in an .mde, .accde, or .ade database. |
|7803|Microsoft Access can't rename _1 to _2. The convert operation failed. |
|7805|This is already an MDE database. |
|7806|This database does not have a Visual Basic project, so it can't be made into an MDE file. Open the database in Microsoft Access to create the Visual Basic project. |
|7807|You cannot convert the open database to an MDE file by running a macro or Visual Basic code. Instead of using a macro or code, on the Database Tools tab, in the Database Tools group, click Make MDE. |
|7808|Microsoft Access can't replace an existing file as a result of converting the Microsoft Access database. You must select a new file name. |
|7809|You cannot rename a _1 object in the client/server version of Microsoft Access. |
|7810|You cannot compact the open database by running a macro or Visual Basic code. Instead of using a macro or code, click the File tab and then click Compact and Repair Database. |
|7811|Microsoft Access requires Microsoft SQL Server 6.5 or later for Access projects. You need to upgrade the selected SQL database to Microsoft SQL Server 6.5 or later. |
|7812|Microsoft Access projects can only be connected to Microsoft SQL Server|
|7815|Microsoft Access was unable to create an ADE database. |
|7816|Microsoft Access requires this Microsoft SQL Server 6.5 installation to be upgraded to service pack 5 in order to successfully connect to this server. |
|7817|The database file '_' is already open. You cannot encode to an open file. |
|7819|You have chosen a database template file that is not supported in the current version of Microsoft Access. To create a database using the Database Wizard, click the Databases tab in the New dialog box and then double-click a database icon. |
|7821|The TEMP environment variable isn't defined. Microsoft Access can't find the temporary file directory. |
|7822|Microsoft Access can't initialize the transfer object. Make sure you have Microsoft SQL Server or the Microsoft SQL Server Desktop Engine installed on the local machine. |
|7823|The source or destination database name is missing. The transfer operation can't continue. |
|7824|The source or destination server name is missing. The transfer operation can't continue. |
|7825|Microsoft Access cannot find the server '_1'. |
|7826|The database object type isn't supported by the Microsoft Access transfer operation. |
|7827|The current database '_1' has _2 active connections. Microsoft Access must disconnect all applications before you can copy the database file. What do you want to do to all the active connections? &Disconnect Cancel|
|7828|The object '_1' is not a valid Microsoft SQL Server database object. Microsoft Access can't transfer this object. |
|7829|The SQL script used to create database objects is corrupted. Microsoft Access can't create the new objects. |
|7830|The SQL script used to create database object '_1' is corrupted. Microsoft Access can't create the new object. |
|7831|Microsoft Access cannot create the file '_1'. |
|7832|The current database '_' is replicated and can't be copied until you disable replication. Use Microsoft SQL Server Enterprise Manager to remove publications and subscriptions before copying the database file. |
|7833|The current database '_' has more than one data file and can't be copied. |
|7834|The Transfer Database Wizard didn't provide enough information to perform the transfer operation. |
|7835|Microsoft Access can't copy the database file because one or more objects are open. Do you want to close all the objects? |
|7836|Microsoft Access can't copy the database file because one or more objects are open. Close all objects and try again. |
|7837|Invalid argument in script. |
|7838|There were errors during the database transfer operation. Open the transfer log file for details. |
|7839|Errors occurred during the database transfer operation. |
|7840|The database transfer operation failed. The destination database '_' was created and is in an unknown state. What would you like to do with the destination database? &Keep &Delete|
|7841|The destination file '_' already exists. Set the Overwrite flag to TRUE if you want to replace the existing file. |
|7842|This database operation is available only when you have a version of Microsoft SQL Server supported by Microsoft Access installed on your local computer. See the Microsoft Office Update Web site for the latest information and downloads. |
|7843|You can only copy a database that is on your local computer. To make a copy of a database on a remote computer, click the File tab, point to Server Tasks, and then click Transfer Database. |
|7844|Microsoft Access can't copy the database file because there are active connections to the current database. |
|7845||
|7846|Microsoft Access can't compact and repair the current database. |
|7847|_1' already exists. Microsoft Access must create a backup of your file before you perform the repair operation. Enter a name for the backup file. |
|7848|The transfer operation has been cancelled. The destination database '_' was created and is in an unknown state. What would you like to do with the destination database? &Keep &Delete|
|7849|The repair operation has been canceled because Microsoft Access could not create a backup of the file. You might not have adequate permissions or enough disk space to create the backup file. To repair the file manually, click the File tab, point to Manage, and then click Compact and Repair Database. |
|7850|The wizard you've requested is not installed or is in a bad state. Please install or reinstall the wizard. If you do not have permission to do this on your computer, please contact your help desk representative. |
|7851|The toolbar name '_' you entered already exists. Enter a unique name for this toolbar. |
|7852|The toolbar name '_1' you entered doesn't follow Microsoft Access object-naming rules. For more information about naming objects, click Help. |
|7853|The default column width must be at least 0.1 inch. |
|7854|You can't export database objects (except tables) from the current version of Microsoft Access to earlier versions of Microsoft Access. |
|7855|You must have a database open to create custom toolbars, and the database can't be read-only. |
|7856|'_' cannot be imported, exported, or copied to Access project files. |
|7858|The Tab Width setting in the Options dialog box (Tools menu) for the Module or Debug window must be from 1 through 32. |
|7859|Microsoft Access can't start because there is no license for it on this machine.|
|7860|The profile '_' that you specified on the command line doesn't exist in the Windows Registry. |
|7861|'_' cannot be imported, exported, or copied to Access database files. |
|7862|Microsoft Access can't find the wizard. This wizard has not been installed, or there is an incorrect setting in the Windows Registry, or this wizard has been disabled.  To reenable this wizard, click the File tab, and click Access Options. Click Add-Ins, and then in the Manage list, click Disabled Items, and then click Go. Rerun Microsoft Access or Microsoft Office Setup program to reinstall the wizards. If the missing wizard is not a Microsoft Access wizard, reinstall it using the Add-in Manager.|
|7863|Links can only be created between Microsoft Access database files.|
|7865|There is already an existing database with the name you entered. Give the new database a unique name.|
|7866|Microsoft Access can't open the database because it is missing, or opened exclusively by another user, or it is not an ADP file.|
|7867|You already have the database open.|
|7868|You can't use a SetMenuItem macro action on a default menu. SetMenuItem is for custom menus only. |
|7869|The SetMenuItem action does not have enough information to be carried out. An argument is invalid, or there are not enough arguments. |
|7870|Microsoft Access can't find the database '_1’. Check the database name (and path, if specified) to make sure you entered it correctly. |
|7871|The table name you entered doesn't follow Microsoft Access object-naming rules. For more information about naming objects, click Help. |
|7872|This recordset is not updatable.|
|7873|'_' cannot be imported, exported, or copied to another database or project file. |
|7874|Microsoft Access can't find the object '_1'. * You misspelled the object name. Check for missing underscores ( _ ) or other punctuation, and make sure you didn't enter leading spaces. * You tried to open a linked table, but the file containing the table isn't on the path you specified. Use the Linked Table Manager to update the link and point to the correct path. |
|7875|The table '_1' already exists. You created or renamed a table, and then tried to save it. Before Microsoft Access could save the table, another user created or renamed one using the same name. |
|7876|There isn't enough temporary disk space to complete the operation. Free disk space, and then try the operation again. For more information on freeing temporary disk space, search the Microsoft Windows Help index for 'disk space, freeing'. For information on compacting a database to free disk space, click Help. |
|7877|Microsoft Access can't sort on the Memo, OLE Object, or Hyperlink '_1'. The ORDER BY clause of an SQL statement can't include any Memo, OLE Object, or Hyperlink fields. |
|7878|The data has been changed. Another user edited this record and saved the changes before you attempted to save your changes. Re-edit the record.  |
|7879|An invalid IDA has been passed to Microsoft Access error handling. Please report this error and the steps that caused it. |
|7880|The value you entered is the wrong data type for this field. Enter an integer. |
|7881|The Macro Conversion Wizard can't be started. This wizard may not be installed, or this wizard has been disabled. To reenable this wizard, click the File tab, and then click Access Options. Click Add-Ins, and then in the Manage list, click Disabled Items, and then click Go. Rerun Microsoft Access or the Microsoft Office Setup program to reinstall this wizard. If you want to preserve your security or custom settings, back up the Microsoft Access workgroup information file. For more information on backing up files, search for 'backing up files' in Microsoft Windows Help.  |
|7882|You can't convert, enable, or encode an Access Project file. |
|7883|You can't set the MenuBar property unless a database is open. |
|7884|You can't export an object to itself. Select a different database to export to, or give the object a new name. |
|7886|Microsoft Access can't find the dynamic-link library (DLL) Mso. Rerun the Microsoft Access or Microsoft Office Setup program. |
|7887|A unique record identifier can't consist of more than 10 fields. |
|7888|You must set the default font size to a number from _1 through _2. For more information on the FontSize property, click Help. |
|7889|The file '_' does not exist. |
|7890|The file '_' doesn't contain any data. You can't import from, or link to, an empty spreadsheet. |
|7892|An error occurred while saving module options. You may be running Visual Basic in break mode. Reset the running code before changing the module options.  |
|7893|You cannot import objects into a database created in an earlier version of Microsoft Access. To convert the database to the current version of Microsoft Access, click the File tab, and then click Convert|
|7895|Microsoft Access was unable to create a window. The system is out of resources or memory. Close unneeded programs and try again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'.  |
|7896|Error '_' in the validation rule. |
|7897|Error '_' in the default value. |
|7898|Execution of this application has stopped due to a run-time error. The application can't continue and will be shut down. |
|7899|Microsoft Access can't be started. Microsoft Access was unable to initialize the Windows Registry. Rerun Microsoft Access or Microsoft Office Setup to reinstall Microsoft Access.   |
|7900|Microsoft Access can't convert the database because it can't create an error table. |
|7901|Microsoft Access can't convert the database because it can't write to the error table. |
|7902|Errors occurred converting _1 validation rules and default values. See error table '_2' for a list of errors. |
|7903|Microsoft Access can't insert this field. There are too many fields in the table. The limit is 255. |
|7904|An error occurred in the Field Builder. You've replaced or modified the default Field Builder, and Microsoft Access is unable to run the new version. Reinstall Microsoft Access to correct the error. |
|7905|Microsoft Access can't change these field data types. Some data in this table violates the record validation rule. Before any data types can be changed, remove the record validation rule or correct the data.  |
|7906|Microsoft Access couldn't launch the Lookup Wizard, or this wizard has been disabled. Rerun the Microsoft Access or the Microsoft Office Setup program, click Add/Remove, and select the Wizards check box. To reenable this wizard, click the File tab, and then click Access Options. Click Add-Ins, and then in the Manage list, click Disabled Items, and then click Go. |
|7907|You don't have permission to modify the design of this table. For more information on permissions and who can set them, click Help. |
|7908|You can't modify this table now because someone else is using it. |
|7909|Microsoft Access can't add another column to this table. You can have up to 255 columns in your table. |
|7910|Microsoft Access can't delete this column. Tables must have at least one column. |
|7911|Microsoft Access can't delete this index. This table participates in one or more relationships. Delete its relationships in the Relationships window first.  |
|7912|Once you save the change to the data type of this column, you will not be able to change back to the previous data type. Are you sure you want change the data type? |
|7913|You can't change a random-valued AutoNumber field to an incremental AutoNumber field. The values of random-valued AutoNumber fields aren't continuous. |
|7914|You can't convert ReplicationID values to other data types. |
|7915|You can't delete the field '_'; it is a replication system field. Use a make-table query to create a new table that contains all the fields in the table except system fields. For information on make-table queries, click Help. |
|7916|Microsoft Access is unable to start the Lookup Wizard. The Lookup Wizard doesn't apply to fields of this data type. |
|7917|'_' cannot be created because the project is disconnected. To connect the project to a database, click Connection under Server Tasks on the File menu. |
|7919|The default data type you selected is not supported in all versions of Microsoft SQL Server. If the server doesn't support the data type you have chosen, the varchar data type will be used in its place. |
|7920|The '_' lookup column was changed to store multiple values. You cannot reverse this setting. |
|7921|You have changed the '_' lookup column to store multiple values. You will not be able to undo this change once you save the table. Do you want to change '_' to store multiple values? |
|7922|You have changed the '_' lookup column to be displayed by a text box. After you save the table, this change will make the column read-only and lose the lookup properties. Do you want to change '_' to be displayed by a text box? |
|7923|The data type of field '_1' is _2. This data type cannot be changed once the field has been saved.|
|7924|You cannot set '_1' to the _2 data type. Use Add Field in the Modify Fields ribbon to add a field with the _2 data type. |
|7925|The table or query you are exporting contains more than one attachment column. Microsoft SharePoint Foundation only supports one attachment column. Remove all but one attachment column, and then retry exporting the table or query.|
|7926|You can't change the column '_' to store multiple values; it is part of a multiple column relationship. If you want to change the column to store multiple values, first delete the relationship in the Relationships window. |
|7927|You can't change the column '_' to store multiple values; it is part of an enforced relationship. If you want to change the column to store multiple values, first delete the relationship in the Relationships window. |
|7928|The calculated field cannot be created. Verify that expression '_1' includes fields that exist in the current table. |
|7929|There are calculated columns in this table that depend on the column '_1'. Changing or deleting this column may cause errors in one or more of the dependent calculated columns. Do you want to continue? |
|7930|'_' cannot be removed from the list definition.|
|7931|The data type of '_' cannot be changed.|
|7932|Microsoft SharePoint Foundation does not support this data type change.|
|7933|An error prevents the data type change of field '_'.|
|7934|The Web does not support multiple attachment columns.|
|7935|An error has occurred trying to save the formula for the calculated column. There may be an error in the formula, or a disallowed field type might have been used. Memo fields, hyperlink fields, and fields with a table or query lookup cannot be used in a calculation. |
|7951|You entered an expression that has an invalid reference to the RecordsetClone property. For example, you may have used the RecordsetClone property with a form or report that isn't based on a table or query. |
|7952|You made an illegal function call. Check the syntax of the function you are trying to use. |
|7953|The value you entered doesn't match the type required. * The variable, property, or object may not be of the correct type. * You may have used an If TypeOf construct with something other than a control. |
|7954|The expression you entered requires the control to be in the active window. Try one of the following: * Open or select a form or report containing the control. * Create a new control in the active window, and try the operation again. |
|7955|There is no current code context object. |
|7956|The syntax of the subquery in this expression is incorrect. Check the subquery's syntax and enclose the subquery in parentheses. |
|7957|The LIKE syntax in the expression is not valid.|
|7958|The expression contains a malformed GUID constant.|
|7959|The expression contains an ambiguous name. Verify that each name in the expression refers to a unique object.|
|7960|There was an error compiling this function. The Visual Basic module contains a syntax error. Check the code, and then recompile it.  |
|7961|Microsoft Access can't find the module '_1' referred to in a macro expression or Visual Basic code. The module you referenced may be closed or may not exist in this database, or the name may be misspelled. |
|7962|The index number you used to refer to the module is invalid. Use the Count property to count the open modules and make sure that the module number is not greater than the number of open modules minus 1. |
|7963|Microsoft Access cannot run the macro or callback function '_1'. Make sure the macro or function exists and takes the correct parameters. |
|7964|You entered an expression that has an invalid reference to the Recordset property. You can only use a dynaset or snapshot recordset to set this property. |
|7965|The object you entered is not a valid Recordset property. For example, you may have used a forward-only recordset, or tried to set it to null. |
|7966|The format condition number you specified is greater than the number of format conditions. Use the Count property to count the format conditions for the control and then check that the format condition number you cite is within the range of existing format conditions. |
|7967|A required minimum or maximum value is missing. |
|7968|The format condition Type you specified is invalid. Valid values for the Type property are 0 to 2 for the first condition, and 0 to 1 for all other format conditions. |
|7969|The format condition Operator you specified is invalid. Valid values for the Operator property are 0 to 7. |
|7971|Microsoft Access cannot follow the hyperlink to '_1'. Please verify the destination. |
|7972|Microsoft Access encountered an error while trying to show the Hyperlink dialog. |
|7974|Microsoft Access failed to insert a hyperlink at the current location. |
|7975|You chose an invalid control type for use with hyperlinks. You can only use hyperlinks with labels, images, command buttons, or bound text boxes. |
|7976|There is no stored hyperlink in this control. |
|7977|Microsoft Access is unable to paste the data on the clipboard as a hyperlink. |
|7978|Microsoft Access is unable to add the current hyperlink to the favorites folder. |
|7979|You can't use the hyperlink property builder with more than one control selected. |
|7980|The HyperlinkAddress or HyperlinkSubAddress property is read-only for this hyperlink. |
|7981|The values for 'File or Web page name' and/or 'Text to Display' are too long. The values will be truncated. To keep your original values, click Cancel in the Edit Hyperlink dialog. |
|7983|Microsoft Access can't follow the hyperlink. |
|7990|You entered an invalid project name. You may have deleted the default project name and forgotten to specify a new one. |
|7991|The Microsoft Access Source Code Control Add-in is not available; this object will be opened read-only. |
|7992|The Microsoft Access Source Code Control Add-in could not be started. |
|7993|The object _ is currently checked in and is therefore read-only. To modify the object, close it first, then check it out and reopen it. |
|7994|There was a problem communicating with the Source Code Control Add-in. |
|7995|The menus on your form or report that are based on Microsoft Access macros will not be visible while the Customize dialog box is open. To have the full power of menu or toolbar customization, convert your macro-based menus to menus or toolbars. With the macro selected in the Navigation Pane, point to Macro on the Tools menu, and create a menu, a toolbar, or a shortcut menu from that macro. |
|7996|Microsoft Access could not create the Data and Misc. objects because the linked table '_1' could not be found. To create the data and miscellaneous objects, use the Linked Table Manager (on the Database Tools tab, in the Database Tools group) to update the link to the source table or file, or delete the link from your current database. |
|7997|You can't save the standard module '_' over a class module with the same name. Save the standard module to a different name, or delete the class module first. |
|7998|You can't save the class module '_' over a standard module with the same name. Save the class module to a different name, or delete the standard module first. |
|7999|Microsoft Access can't delete this relationship because you don't have the Data and Misc. Objects checked out. Check out the Data and Misc. Objects and then delete the relationship. |
|8000|The name you entered already exists for another object of the same type in this database. Do you want to replace the existing _? |
|8001|'_' has been changed since the last time you opened it, either by another user or because another instance of it was opened on your own machine. Do you want to replace the changes that you or another user made?  * To save your most recent changes and discard the other user's changes or your previous changes, click Yes. * To save this version of the object with another name, click No.|
|8003|You must save the _ before you can create a new object based on it. Do you want to save the _ and create a new object? |
|8004|The layout of '_' has been changed since the last time you opened it, either by another user or because another instance of it was opened on your own machine. Do you want to replace the changes that you or another user made?  * To save your most recent changes and discard the other user's changes or your previous changes, click Yes. * To cancel saving this version of the object, click No.|
|8006|The name you entered already exists for another object of the same type in this database. Do you want to replace the existing _? You will not be able to undo this operation. |
|8007|You cannot get into Exclusive Mode right now because this database is opened by other users or because you do not have permissions to open exclusively. |
|8008|The stored procedure executed successfully but did not return records.|
|8050|Do you want to save changes to the design of _? |
|8052|Do you want to delete the _1 and empty the Clipboard?  You tried to delete a database object that is currently on the Microsoft Access Clipboard. If you delete it from your database, you won't be able to paste it later.|
|8053|Do you want to delete the _? Deleting this object will remove it from all groups. For more information on how to prevent this message from displaying every time you delete an object, click Help. |
|8054|This action will cause Microsoft Access to empty the Clipboard. Do you want to continue? |
|8055|Microsoft Access can't change the working directory to '_1’. Verify that the drive is valid and the path is 260 characters or less in length. |
|8058|Do you want to save changes to the layout of _? |
|8059|Do you want to remove the link to the _1?  If you delete the link, you delete only the information Microsoft Access uses to open the table, not the table itself.|
|8060|The toolbar '_1' is partly unreadable.  Microsoft Access can't display all the toolbar buttons. The specified toolbar may be in a different format than the toolbars in the current version of Microsoft Access. Click OK to close the dialog box, and then add the buttons missing from the toolbar. Microsoft Access will then update the toolbar to the latest format.  |
|8061|Are you sure that you want to restore the default settings to the built-in toolbar or menu bar _1?  If you previously customized, moved, changed the visibility, or otherwise changed the toolbar or menu bar, or any of the menus that cascade from it, Microsoft Access removes your changes, returns the original buttons to their original order, and shows or hides the toolbar based on the original setting. To return the built-in toolbar or menu bar to its original state, click Yes.|
|8063|The source application is not responding. Do you want to continue waiting?  The DDE channel has been established, but the data exchange was not completed in the amount of time specified in the OLE/DDE Timeout setting in the Access Options dialog box (click the File tab and then click Access Options). * To continue waiting for the data exchange to be completed, click Yes. *  To cancel the data exchange and try again later, click No .|
|8064|This linked table has indexes that use an unsupported sort order. If you modify this table, the Microsoft Access database engine can't correctly maintain the table's indexes. As a result, your data may appear in the wrong order, and functions that use the table's indexes may have unexpected results. Try one of the following: * Cancel this operation. Use the application in which the table was created to re-create the indexes, making sure to specify an ASCII or International sort order. Then try again to link the tables. * Use this table on a read-only basis.  |
|8065|You can't delete the table '_1' until its relationships to other tables have been deleted. Do you want Microsoft Access to delete the relationships now? |
|8067|Do you want to permanently delete the _?  If you click Yes, you won't be able to undo the deletion.|
|8069|Microsoft Access couldn't create the custom toolbar '_1’. |
|8071|Would you like to remove the compacted database from Source Code Control? |
|8072|Microsoft Access must close the _1 in order to complete this operation. Would you like Microsoft Access to close it now? |
|8073|You do not have the _1 checked out. Microsoft Access can't check out an object while it is open, and you can't make design changes until you check it out. If you want to make design changes to this object, first close it, check it out, and then reopen it.  |
|8074|Microsoft Access must save the _1 in order to complete this operation. Would you like Microsoft Access to save it now? |
|8075|You do not have the Data and Misc. Objects checked out. You can't create a new table or save changes to a data access page. |
|8076|To import tables, relationships, menus, toolbars, import/export specs or data access pages you must have the Data and Misc. Objects checked out. |
|8077|Are you sure you want to cut the _? |
|8078|This object will be removed from your local database, but not from source code control. The next time you get this database from source code control, the object will reappear. Are you sure that you want to delete the local object? |
|8079|Microsoft Access was not able to add a reference to the type library for this control because Data and Misc. Objects is not checked out. Check out the Data and Misc. Objects and add a reference to the library '_1'. |
|8080|An error occurred while trying to create the linked table '_'. Check to make sure that the source table or file is available, and then retry the operation on the Data and Misc. Objects. |
|8081|You have set _1's Type property to Popup, which changes the toolbar to a shortcut menu. The shortcut menu disappears because Microsoft Access adds _2 to the Shortcut Menus toolbar. To complete the shortcut menu, close the Toolbar Properties sheet, display the Shortcut Menus toolbar, click the Custom category, and then add the commands you want. |
|8082|Microsoft Access must save the _1 to the current database in order to complete this operation. Would you like Microsoft Access to save it now? |
|8086|An error occurred trying to add one or more references from the source code control project. The library may not be registered on your computer. Check your project references and add any missing references after registering the missing components that this database requires. |
|8087|Restoring a database requires that no one is currently using the database. Do you want to close all objects, select a backup file, and restore the database from backup? |
|8088|Dropping a database requires that no one is currently using the database. Do you want to close all open objects, and drop the database? |
|8090|Are you sure you want to delete the conflict table _?  You will not be able to undo this operation.|
|8091|This database is enabled for publication. Do you want to delete existing publications and drop the database? |
|8092|Access could not save your project. Do you want to cancel the close operation? *To cancel the close operation, click Yes. *To close without saving, click No. |
|8096|Microsoft Access can't change the working directory to '_1’. Please enter a non-Internet location. |
|8097|The connection string of this page specifies an absolute path. The page might not be able to connect to data through the network. To connect through a network, edit the connection string to specify a network (UNC) path. &Don't show this warning again|
|8098|Your computer has resumed operation after Critical Suspend mode. Any unsaved changes might have been lost. To ensure proper operation, close and reopen any files. Restarting Microsoft Access is recommended. |
|8100|The report is still more than one page wide. To fit the report on one page decrease the report width or increase the page width. You can decrease the report width by selecting each control that is farthest to the right, moving it to the left, and removing the extra report space. You may need to repeat this multiple times. You can increase the page width by adjusting the page setup options.  |
|8101|The report is still more than one page wide. To fit the report on one page decrease the report width or increase the page width. You can decrease the report width by selecting each control that is farthest to the left, moving it to the right, and removing the extra report space. You may need to repeat this multiple times. You can increase the page width by adjusting the page setup options.  |
|8103|Do you want to discard the pending changes and delete the link to the list _?  The linked list you are attempting to delete has changes that have not been saved to the server. Do you still want to delete the linked list and lose these pending changes?|
|8104|The name you supplied is a reserved word. Reserved words have a specific meaning to Microsoft Access or to the Microsoft Access database engine.  If you use a reserved word, you may receive an error when referring to this field.|
|8105|You must close and reopen the current database for the specified option to take effect.|
|8106|Encrypting with a block cipher is incompatible with row level locking. Row level locking will be ignored.|
|8107|Microsoft Access cannot save the database to the _1 file format. The specified database sort order, _2, is not supported for the target database format. |
|8400|The database '_' is read-only. You won't be able to save changes made to data or object definitions in this database. |
|8401|You cannot make changes to the database objects in the database '_1’. This database was created in an earlier version of Microsoft Access. To convert this database to the current version of Microsoft Access, click the File tab, and then click Convert.  |
|8402|The file '_' already exists. Do you want to replace the existing file? If you want to compact or convert to a different file name, click No. Enter the new file name after the /compact or /convert option in the command line, and run the command again. |
|8403|The setting you entered is invalid. The database's sort order cannot be updated. The setting you entered for the New database sort order option (Personalize category) of the Access Options dialog box (click the File tab and then click Access Options) is not a valid sort order for the version of the database you are compacting. |
|8404|You can't open '_1' for exclusive use because another user has the database open or because you do not have permissions to open exclusively. Microsoft Access will open the database for shared access. |
|8405|This database was created using a previous version of Microsoft Access. - You can share the database with users of previous versions of Microsoft Access, however some enhancements will be unavailable. - You can open the database, view objects and modify records using this release of Microsoft Access, but design changes will be disabled. - To make design changes you must use an earlier version of Microsoft Access that supports the old database format, or upgrade the database. |
|8406|Does the first row of your data contain column headings? |
|8409|Microsoft Access has made a backup of database _1 at _2. Note: Only server-related objects have been backed up. To back up your project, click Back Up Project on the File menu|
|8410|Microsoft Access successfully restored database _1 from its backup at _2|
|8412|The name you entered already exists for another shortcut in this group.  Do you want to replace the existing shortcut|
|8414|Microsoft Access encountered one or more errors during conversion. To view a summary of these errors, open the '_1' table. |
|8415|The HTML file associated with this link has been moved, renamed, or deleted. File: '_' &Update Link... Cancel |
|8416|Renaming this user-defined function removes any existing permissions and extended properties. Do you want to complete the rename action? |
|8417|The file '_' cannot be opened because it has been locked by another user. Try again when the file is available. |
|8500|A form with a subform object can't have its DefaultView property set to Continuous Forms. You tried to add a subform to a form in Design view. Microsoft Access will reset the property to Single Form. |
|8501|You can't modify this field because it is read-only. Some fields, such as a calculated field, are read-only by design. However, any field can be read-only if its Locked property is set to Yes. |
|8502|Microsoft Access has reached the end of the records. Do you want to continue searching from the beginning? |
|8503|Microsoft Access has reached the beginning of the records. Do you want to continue searching from the end? |
|8504|Microsoft Access finished searching the records. The search item was not found. |
|8505|Do you want to continue to search or replace? |
|8507|There isn't enough memory to execute the macro that updates the active filter. Microsoft Access is closing the Filter window. Close unneeded programs. Then try again to open the Filter window. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'. |
|8508|Microsoft Access can't find '_1’. The text string you entered in the Find What box can't be evaluated against the current field. Reenter the text string so that it conforms to the data type of the field.  |
|8509|You won't be able to undo this Replace operation. There isn't enough free memory to use the Undo command. Do you want to continue? |
|8510|Microsoft Access can't save the current record. Do you want to undo the changes to the record and continue the Paste operation? |
|8511|Records that Microsoft Access was unable to paste have been inserted into a new table called '_1’. In the Navigation Pane, open the new table to see the unpasted records. After you fix the problems that resulted in the paste errors, copy and paste the records from the new table. |
|8512|None of the field names you pasted onto the Clipboard match the field names on the form. Microsoft Access treats the data in the first row on the Clipboard as field names. If you copied the data from another application, the first row of data may have taken the place of the field names. Do you want to paste the field names in the order you defined with the Tab Order command? |
|8513|Some of the field names for the data you tried to paste don't match field names on the form. Microsoft Access treats data in the first row on the Clipboard as field names. In this case, some of those field names don't match the field names on the form. Do you want to paste only that data whose names match the field names on the Clipboard? |
|8514|Do you want to suppress further error messages telling you why records can't be pasted?  If you click No, a message will appear for every record that can't be pasted.|
|8515|You copied a large amount of data onto the Clipboard. When you copy data onto the Clipboard, only the reference to the object is copied. If you close the source document, however, Microsoft Access must paste all the data from its source. Depending on the amount of data, this can take some time. Do you want to save this data on the Clipboard? |
|8516|You selected more records than can be copied onto the Clipboard at one time. Divide the records into two or more groups, and then copy and paste one group at a time. The maximum number of records you can paste at one time is approximately 65,000. |
|8517|Do you want to suppress further error messages telling you why records can't be deleted?  If you click No, a message will appear for every record that can't be deleted.|
|8518|You won't be able to undo this Replace operation. Do you want to continue? |
|8519|You are about to delete _ record(s). If you click Yes, you won't be able to undo this Delete operation. Are you sure you want to delete these records? |
|8520|You are about to paste _ record(s). Are you sure you want to paste these records? |
|8521|You won't be able to undo this Delete operation. The Undo command won't be available because this operation is too large, or there isn't enough free memory. Do you want to delete these items? |
|8522|All timers are in use. Set the TimerInterval property back to zero so that you can start another timer. |
|8523|Do you want to delete the custom palette information from this form or report and revert to the default palette? |
|8524|Do you want to remove this picture from this control? |
|8525|The remote data isn't accessible. You tried to open a form or report that includes a DDE or DDESend function in a calculated control that specifies an OLE server application. Do you want to start the application _? |
|8526|Microsoft Access encountered a problem while trying to switch views and must close this window. |
|8527|Do you want to delete the group section for the database object '_' and its contents?  The group header or footer you want to delete contains controls which will be deleted along with the section.|
|8528|Deleting these sections will also delete all controls in them. You will not be able to undo this action. Do you want to delete these sections anyway? |
|8529|The grouping level that you are trying to delete has a group header or group footer section. The section contains controls that will also be deleted. Do you want to continue? |
|8530|Relationships that specify cascading deletes are about to cause _ record(s) in this table, along with related records in related tables, to be deleted. Are you sure you want to delete these records? |
|8531|The section width is greater than the page width, and there are no items in the additional space, so some pages may be blank. For example, the report width may be wider than the page width. |
|8532|Do you want to remove this picture from the form? |
|8533|Some data may not be displayed. There is not enough horizontal space on the page for the number of columns and column spacing you specified. Click Page Setup on the File menu, click the Columns tab, and then reduce the number of columns or the size of the columns. 3|
|8534|When you save this form or report, any code behind it will be deleted. You have set the HasModule property to No, creating a form or report that opens more quickly. Any macros associated with this form or report will not be affected, nor will code in separate modules that is called from this property sheet. Are you sure you want to do this? |
|8539|Relationships that specify cascading deletes are about to cause record(s) in this table and in related tables to be deleted. Are you sure you want to delete these records? |
|8540|If you click Yes, you won't be able to undo this Delete operation. Are you sure you want to delete these records?|
|8541|Microsoft Access could not perform the delete operation. An error occurred during the delete operation, no records were deleted.|
|8542|Microsoft Access did not detect a relationship between the fields you have chosen.  Do you want a relationship to be created for you now?|
|8544|You won't be able to undo this paste operation. Do you want to continue? |
|8546|Current object must first be saved before exporting data to XML.  Would you like to save '_' and then continue?|
|8547|Do you want to save the changes made to the macro and update the property? |
|8548|To complete this operation, Access must modify the RecordSource property of the current form or report. Access will create a new query and embed it directly into the form's or report's RecordSource property. The form or report will no longer be based on the '_' query. Do you want to accept this change? |
|8549|This table contains one or more Lookup fields. Display values for Lookup fields cannot be changed by using the Replace operation. |
|8550|A table or query needed in order to create the new form, report, or query is currently open in Design view. Save or discard the changes to this table or query and open it in Datasheet view, then try again. |
|8551|Do you want to remove this picture from the report? |
|9502|Microsoft Access won't be able to undo this action or subsequent actions. To make more resources available so that Access can record further design actions, close and reopen this window.  Do you want to continue anyway?|
|9504|The file '_' is not readable by the Access database engine. If you think you have a valid Access database, this is most likely because of the recent file format change. Do you want to attempt to upgrade the database format?|
|9505|Microsoft Access has detected that this database is in an inconsistent state, and will attempt to recover the database. During this process, a backup copy of the database will be made and all recovered objects will be placed in a new database. Access will then open the new database. The names of objects that were not successfully recovered will be logged in the "Recovery Errors" table. |
|9507|Microsoft Access was unable to load all the library modules. |
|9508|Microsoft Access couldn't close database '_1’. Your last change may not have been saved because a record was locked by another user. To avoid losing data, wait for the other user to finish editing the record, then click OK again. If you click Cancel, you will lose unsaved changes. |
|9509|You can't undo this command and, once it's completed, you won't be able to edit this object. Do you want to continue anyway? |
|9511||
|9513|Microsoft Access was unable to completely repair the '_1' table and deleted some Memo, OLE Object, or Hyperlink field values. To recover the data, restore from your backup copy of the database. Note that the data in the deleted field may have been damaged by a bad sector on your hard disk. For more information on checking your disk's surface, files, and folders for errors, search the Windows Help index for 'checking for disk errors'. |
|9514|One or more records were unrecoverable and were deleted from the '_' table. |
|9515|The '_' table was truncated; data was lost. |
|9516|One or more indexes from the '_' table couldn't be repaired and were deleted. |
|9517|The Save As command can't process any subforms contained in your report. Do you want to proceed anyway? |
|9518|An error occurred while adding this word to the custom dictionary. * The dictionary file may be read-only. * There may be a disk error. |
|9519|An error occurred while adding this word to the Change All list. The dictionary may be full. |
|9520|An error occurred while adding the word to the Ignore All list. The dictionary may be full. |
|9521|The specified word is too large. Words can't exceed 64 characters. |
|9523|Microsoft Access can't open the '_1' custom dictionary. |
|9524|Microsoft Access can't start the spelling checker because it isn't installed. |
|9525|Microsoft Access can't open the main dictionary file. Verify that this file has been correctly installed. |
|9526|The spelling checker only works on text fields that contain text data. You're trying to check a field with a data type other than Text or Memo. |
|9527|The spelling checker can't proceed; you must first select data from a table, query, view, stored procedure, or form. |
|9529|Microsoft Access cannot run the spell checker because the data in this form or query is not updatable. |
|9530|An error occurred while trying to modify the contents of field '_’. The field may be locked or read-only, or you may not have permissions to change it. For information on security permissions and who can change them, click Help. |
|9532|The '_' field can't be modified because it's a read-only field. |
|9533|An error occurred while trying to add the word pair to the AutoCorrect list. |
|9534|The current selection does not contain any fields that can be checked for spelling errors. You can check the spelling of text box controls with Text or Memo data types. |
|9535|You entered an invalid main dictionary. Please select a valid entry. |
|9536|The spelling check is complete. |
|9537|You must restart the spelling checker for the dictionary change to take effect. |
|9539|You have specified a word that isn't found in the main or custom dictionary. Do you want to use this word and continue checking? |
|9541|The spelling checker can't undo your last change. The data in field '_' has been modified by another user. To resume the spelling check, click OK. |
|9542|This database must be closed before you can create a replica. Do you want Microsoft Access to close this database and create the replica? If you proceed, Microsoft Access will close your database and convert it to a Design Master. The database may increase in size. |
|9543|The Hangul Hanja Converter can't proceed. There is no Hangul or Hanja data to convert. |
|9544|Microsoft Access can't register the '_1' custom dictionary. |
|9545|After you have converted this file to Access 2002 - 2003 File Format, the new file cannot be shared with Access 2000 users or Access 97 users. For more information about conversion, click Help. |
|9547|After you have converted this file to Access 2000 File Format, the new file cannot be shared with Access 97 users.  Any functionality specific to Access 2002 or later will not be available in Access 2000. For more information about conversion, click Help.|
|9548|After you have converted this file to Access 97 File Format, any functionality specific to Access 2000 or later will be lost. For more information about conversion, click Help. |
|9549|This Database has been upgraded to the Access 2007 File Format. The new database cannot be shared with users of Access 2003 or earlier versions. For more information about conversion, click Help. |
|9550|The Text Format property of the bound field '_' is not currently Rich Text. You should change the Text Format property of the table field before you change the property of this control. If you change the Text Format property of this control to Rich Text before you change the property of the bound field, some data that is not valid HTML might not be displayed. Do you want to continue? |
|9551|The Text Format property of the bound field '_' is not currently Plain Text. You should change the Text Format property of the table field before you change the property of this control. If you change the Text Format property of this control to Plain Text before you change the property of the bound field, some data might be displayed as HTML tags. Do you want to continue? |
|9552|The application is already in sync with the server.|
|9553|There are pending updates to this application that must be applied before you can save your changes to the server. Do you want to apply these updates now?|
|9554|The site '_' already exists. Please select another name.|
|9555|This connection file contains definitions that conflict with connections already installed in your database. Click OK to overwrite the existing connection definition. Click Cancel to keep the existing connection definition and cancel the installation.|
|9556|Deleting this object from your database will cause associated linked tables and expressions to cease functioning.|
|10000|You must first save the table. Do you want to save the table now? |
|10001|You must first save the view. Do you want to save the view now? |
|10002|You must first save the stored procedure. Do you want to save the stored procedure now? |
|10003|You must first save the query. Do you want to save the query now? |
|10004|Do you want to permanently delete the selected field(s) and all the data in the field(s)?  To permanently delete the field(s), click Yes.|
|10005|There is no primary key defined. Although a primary key isn't required, it's highly recommended. A table must have a primary key for you to define a relationship between this table and other tables in the database. Do you want to create a primary key now? |
|10006|Changing to this data type requires removal of one or more indexes. You can't use indexes on fields with a Memo, OLE Object, or Hyperlink data type. If you click Yes, Microsoft Access will delete the indexes that include that field. Do you want to continue anyway? |
|10007|Deleting field '_1' requires Microsoft Access to delete the primary key. Do you want to delete this field anyway? |
|10008|Deleting field '_1' requires Microsoft Access to delete one or more indexes. If you click Yes, Microsoft Access will delete the field and all its indexes. Do you want to delete this field anyway? |
|10009|Either an object bound to table '_' is open or another user has the table open. Do you want to open the table as read-only?  To open the table as read-only, click Yes. To open the table as read/write, click No, make sure the table and all objects bound to it are closed, and then try opening it again in Design view.|
|10010|Table '_' is a linked table whose design can't be modified. If you want to add or remove fields or change their properties or data types, you must do so in the source database. Do you want to open it anyway? |
|10011|Microsoft Access encountered errors while converting the data. The contents of fields in _1 record(s) were deleted. Do you want to proceed anyway? |
|10012|Microsoft Access cannot retrieve field properties from the system tables. This database needs to be repaired. If you click Yes, Microsoft Access will open the table in Design view. The settings of the FieldName, DataType, FieldSize, Indexed, and Primary properties will remain intact, but the settings of other field properties will be lost. You can attempt to redefine the lost properties, but we recommend that you restore the database from a backup copy, or close it and use the Compact and Repair Database command (click the File tab and point to Manage). Do you want to continue? |
|10013|Microsoft Access can't save property changes for linked tables. Do you want to continue anyway? |
|10014|Microsoft Access was unable to append all the data to the table. The contents of fields in _1 record(s) were deleted, and _2 record(s) were lost due to key violations. * If data was deleted, the data you pasted or imported doesn't match the field data types or the FieldSize property in the destination table. * If records were lost, either the records you pasted contain primary key values that already exist in the destination table, or they violate referential integrity rules for a relationship defined between tables. Do you want to proceed anyway? |
|10015|Changing to this data type requires removal of the primary key. Do you want to continue anyway? |
|10016|Some data may be lost. The setting for the FieldSize property of one or more fields has been changed to a shorter size. If data is lost, validation rules may be violated as a result. Do you want to continue anyway? |
|10017|Your computer is out of disk space. You won't be able to undo this paste append. Do you want to continue anyway? |
|10018|Microsoft Access was unable to append all the data to the table. The contents of fields in _1 record(s) were deleted, and _2 record(s) were lost due to key violations. * If data was deleted, the data you pasted or imported doesn't match the field data types or the FieldSize property in the destination table. * If records were lost, the records you pasted contain primary key values that already exist in the destination table, or they violate referential integrity rules for a relationship defined between tables. |
|10019|Microsoft Access can't find the database containing the linked table '_1’. The properties set in Microsoft Access for the linked table will be lost. Do you want to continue with the conversion anyway? |
|10020|Data integrity rules have been changed; existing data may not be valid for the new rules. This process may take a long time. Do you want the existing data to be tested with the new rules? |
|10021|Existing data violates the new record validation rule. Do you want to keep testing with the new rule? * To keep the new rule and continue testing, click Yes. * To revert to the old rule and continue testing, click No. * To stop testing, click Cancel. |
|10022|Existing data violates the new setting for the '_1' property for field '_2’. Do you want to keep testing with the new setting? * To keep the new setting and continue testing, click Yes. * To revert to the old setting and continue testing, click No. * To stop testing, click Cancel. |
|10023|Microsoft Access deleted _1 indexes on the converted fields. Some data did not convert properly. |
|10024|You must save the table first. Microsoft Access can't test your data until you save the design changes you made. Do you want to save the table now? |
|10025|This operation will test the table's record and field validation rules, as well as the Required and AllowZeroLength properties, for all data in the table. This process may take a long time. Do you want to continue anyway? |
|10026|All data was valid for all rules. |
|10027|Existing data violates the new record validation rule. Do you want to continue testing data with this new rule? * To continue testing for other new rule violations, click Yes. * To continue to test only the old validation rules, click No. * To stop testing, click Cancel. |
|10028|The existing data violates the '_1' property for field '_2’. If you continue testing, Microsoft Access will inform you if data violates any other property settings in the table. Would you like to continue testing? |
|10030|There is no primary key defined. Although a primary key isn't required, it's highly recommended. A table must have a primary key for you to define relationships between this table and other tables in the database. Do you want to return to table design and add a primary key now? |
|10031|You must first save the function. Do you want to save the function now? |
|10032|This field will be converted to Plain Text and all the formatting in the field will be removed. Do you want to convert the column to Plain Text? |
|10033|This field will be converted to Rich Text, and all the data it contains will be HTML encoded. If your data already contains valid HTML Rich Text, you can remove any extra HTML encoding by using the PlainText function in an update query. Do you want to convert the column to Rich Text? |
|10034|Warning: This change will cause all history for column '_' to be lost. Do you want to continue? |
|10035|After this change, you will not be able to switch back to the current data type. Do you want to continue with this data type change? |
|10250|You can't print the scrollable (unfrozen) columns on the datasheet. The frozen columns are wider than the page. Do you want to print just the frozen columns?  If you want to print the unfrozen columns, try one or more of the following: * Decrease the column width and increase the row height of the frozen columns. * Change the page orientation to Landscape in the printer Properties dialog box. * Decrease the size of the left and right page margins in the Page Setup dialog box.|
|10251|The column heading is too tall to fit on the page. Part of the heading will be cut off. Do you want to print the incomplete heading?  To prevent headings from being cut off, try one or more of the following: * Change the page orientation to Portrait in the printer Properties dialog box. * Decrease the size of the top and bottom page margins in the Page Setup dialog box.|
|10252|At least one column is too wide to fit on the page. Data in that column will be cut off. Do you want to print the column with incomplete data?  To prevent data from being cut off, try one or more of the following: * Decrease the column width and increase the row height. * Change the page orientation to Landscape in the printer Properties dialog box. * Decrease the size of the left and right page margins in the Page Setup dialog box.|
|10253|The row height exceeds the space between the top and bottom margins. Do you want to print the columns with incomplete data?  To prevent data from being cut off, try one or more of the following: * Increase the column width and decrease the row height. * Change the page orientation to Portrait in the printer Properties dialog box. * Decrease the size of the top and bottom page margins in the Page Setup dialog box.|
|10500|You are about to run an update query that will modify data in your table. Are you sure you want to run this type of action query? For information on how to prevent this message from displaying every time you run an action query, click Help. |
|10501|You are about to run an append query that will modify data in your table. Are you sure you want to run this type of action query? For information on turning off confirmation messages for document deletions, click Help. |
|10502|You are about to run a make-table query that will modify data in your table. Are you sure you want to run this type of action query? For information on how to prevent this message from displaying every time you run an action query, click Help. |
|10503|You are about to run a delete quer that will modify data in your table. Are you sure you want to run this type of action query? For information on how to prevent this message from displaying every time you run an action query, click Help. |
|10504|You are about to run a data-definition query that may modify data in your table. Are you sure you want to run this type of an SQL query? If not, click No and then modify the query or close it to run later. |
|10505|You are about to update _ row(s). Once you click Yes, you can't use the Undo command to reverse the changes. Are you sure you want to update these records? |
|10506|You are about to append _ row(s). Once you click Yes, you can't use the Undo command to reverse the changes. Are you sure you want to append the selected rows? |
|10507|You are about to paste _ row(s) into a new table. Once you click Yes, you can't use the Undo command to reverse the changes. Are you sure you want to create a new table with the selected records? |
|10508|You are about to delete _ row(s) from the specified table. Once you click Yes, you can't use the Undo command to reverse the changes. Are you sure you want to delete the selected records? |
|10509|Microsoft Access can't update all the records in the update query. Microsoft Access didn't update _1 field(s) due to a type conversion failure, _2 record(s) due to key violations, _3 record(s) due to lock violations, and _4 record(s) due to validation rule violations. Do you want to continue running this type of action query anyway? To ignore the error(s) and run the query, click Yes. For an explanation of the causes of the violations, click Help. |
|10510|Microsoft Access can't append all the records in the append query. Microsoft Access set _1 field(s) to Null due to a type conversion failure, and it didn't add _2 record(s) to the table due to key violations, _3 record(s) due to lock violations, and _4 record(s) due to validation rule violations. Do you want to run the action query anyway? To ignore the error(s) and run the query, click Yes. For an explanation of the causes of the violations, click Help. |
|10511|Microsoft Access can't add all the records in the update or append query. It set _1 field(s) to Null due to a type conversion failure. A type conversion failure is caused when the data in one or more fields doesn't match the DataType or FieldSize property in the destination table. For example, leaving blank fields in a Yes/No field or entering text in a numeric field will cause this error. Do you want to ignore the errors and run the update or append query anyway? To ignore the error(s) and run the query, click Yes. |
|10512|Microsoft Access can't delete _2 record(s) in the delete quer due to key violations and _3 record(s) due to lock violations. Do you want to run this action query anyway? To ignore the error(s) and run the query, click Yes. For an explanation of the causes of the violations, click Help. |
|10513|The existing _ will be deleted before you run the query. Do you want to continue anyway? |
|10514|You won't be able to undo the changes this action query is about to make to the data in a linked table or tables. Do you want to run this action query anyway? |
|10515|Microsoft Access was unable to update all the records in the update query. Microsoft Access didn't update _1 field(s) due to a type conversion failure, _2 record(s) due to key violations, _3 record(s) due to lock violations, and _4 record(s) due to validation rule violations. For an explanation of the causes of the violations, click Help. |
|10516|Microsoft Access can't append all the records to the table. Microsoft Access set _1 field(s) to Null due to a type conversion failure, and it didn't add _2 record(s) due to key violations, _3 record(s) due to lock violations, and _4 record(s) due to validation rule violations. For an explanation of the causes of the violations, click Help. |
|10517|Microsoft Access can't add all the records in the make-table query. It set _1 field(s) to Null due to a type conversion failure. A type conversion failure is caused when the data in one or more fields doesn't match the DataType or FieldSize property in the destination table. For example, leaving blank fields in a Yes/No field or adding text in numeric field will cause this error. |
|10518|Microsoft Access can't delete all the records in the delete quer. Microsoft Access didn't delete _2 record(s) due to key violations and _3 record(s) due to lock violations. For an explanation of the causes of the violation, click Help. |
|10519|There isn't enough disk space or memory to undo the data changes this action query is about to make. Do you want to run this action query anyway? For information on freeing disk space or freeing memory, search the Microsoft Windows Help index for 'disk space, freeing' or 'memory, troubleshooting'. |
|10520|Do you want to save the changes made to the SQL statement and update the property?  The RecordSource or RowSource property contained an SQL statement when you invoked the Query Builder, so the original SQL statement was modified. To close the Query Builder without changing the original SQL statement, click No.|
|10521|Do you want to save the changes made to the query and update the property?  The RecordSource or RowSource property contained the name of a query when you invoked the Query Builder, so the original query was modified. To close the Query Builder without changing the original query, click No.|
|10522|You invoked the Query Builder on a table. Do you want to create a query based on the table? |
|10523|You are about to run a pass-through query that may modify data in your table. Are you sure you want to run this type of an SQL query? For information on how to prevent this message from displaying every time you run an SQL query, click Help. |
|10524|_ output column(s) in the query were unnamed and will not show any data. |
|10526|Do you want to save the changes made to the SQL Statement and update the text editor?  The Stored Procedure or Function contained an SQL statement when you invoked the Query Builder, so the original SQL statement was modified. To close the Query Builder without changing the original SQL statement, click No.|
|10600|Are you sure you want to permanently delete the selected relationship from your database? |
|10601|A relationship already exists. Do you want to edit the existing relationship? To create a new relationship, click No. |
|10602|The field name is missing in row _. You haven't selected a matching field for this relationship in each row of the grid. Select fields so that the grid has the same number of fields on the left and right sides, and then try to create the relationship again.  |
|10603|This relationship has been modified or deleted by another user since you opened the Relationships window. Do you want to edit the relationship and overwrite the other user's changes? To update your Relationships window to include the other user's changes, click No. |
|10604|This relationship has already been deleted by another user. To update your view, click OK. |
|10605|The layout of the Relationships window will be cleared. Do you want to continue? |
|10606|You cannot display queries or linked tables in the Relationships window for this database. The database '_1' was created in an earlier version of Microsoft Access. To convert this database to the current version of Microsoft Access, click the File tab, and then click Convert. |
|10607|The relationship has been created as a one-to-many relationship because there are duplicate entries in the related field. You've tried to create a one-to-one relationship, but the data in the tables suggests that a one-to-many relationship is more appropriate. |
|10608|To create a relationship, use the mouse to drag a field from one table to another. |
|10700|This document was previously formatted for the printer _1 on _2, but that printer isn't available. Do you want to use the default printer _3? |
|10701|This document was previously formatted for the printer '_1 on _2', but that printer isn't available. The page settings shown in the Print Setup dialog box are for the current default printer '_3'. Do you want to continue? |
|10702|Previewing or printing this PivotTable view might take a long time because it displays a large amount of detail data. Microsoft Access might not respond for some time. Do you want to continue?  * To reduce the amount of time required, hide most or all of the detail data in the view. * For more information, consult Microsoft Knowledge Base article Q282315.|
|10750|You must save the macro before you run it. Do you want to save the macro now? * To save the macro and then run it, click Yes. * To return to the Macro window for this macro or to the window you executed the Run Macro command from, click No. |
|10751|Some of the macro actions you are trying to save cannot be performed in Access 97. These actions will be permanently deleted from the macro in the Access 97 version of your database. |
|10800|Microsoft Access finished adding the index '_1’. Add another index from the Select Index Files dialog box, or click Close. |
|10801|The Microsoft Access index information (.inf) file for '_1' already exists. Do you want to replace the existing Microsoft Access .inf file for the dBASE or Microsoft FoxPro file you're linking?  * To create a new .inf file, click Yes. * To use the existing file, click No. If the .inf file contains invalid or outdated information, you must click Yes to create a new .inf file before you can link the table.|
|10803|The object '_' already exists. Do you want to replace the existing object with the one you are exporting? If you want to export this object without replacing the existing object, click No. You can then choose a new name in the Export dialog box. |
|10804|Not all of the import specifications could be imported due to name conflicts. Conflicts existed in _ of the specifications. Rename the conflicting import specifications, and try the import operation again. |
|10806|Related data is not supported with the live data option.|
|10807|Please specify XML export location.|
|10808|The current record was not unique. All identical records were exported.|
|10892|A form with a bound ActiveX control or an embedded object bound to a data source can't have its DefaultView property set to Continuous Forms. You tried to add a bound ActiveX control to a form in Design view. Microsoft Access will reset the DefaultView property to Single Form. |
|10896|You have chosen to permanently delete the link to the data access page. Delete &Link Cancel|
|10897|There is a problem with Internet Explorer registry settings. Please reinstall Internet Explorer.  |
|10898|Microsoft Access data access pages can only be viewed in Windows Internet Explorer, but it is not your default Browser. Do you want to open Internet Explorer to view this page? |
|10899|A link to this data access page could not be created because the Data and Misc. Objects are not checked out. |
|10950|Are you sure you want to delete this account?  You can't undo the deletion of a user or group account. To restore a user or group account that has been deleted, you must recreate the account using the same name and personal identifier (PID).|
|10951|You don't have permission to modify '_’. To modify this object, you must have Modify Design permission for it. If the object is a table, you must also have Delete Data and Update Data permissions for it. Do you want to save a copy of the object as a new object? |
|10952|You don't have permission to view this macro. To view a macro, you must have Read Design permission for it. Do you want to continue to run the macro? |
|10953|You don't have permission to modify '_’. To modify this object, you must have Modify Design permission for it. If the object is a table, you must also have Delete Data and Update Data permissions for it. Do you want to open it as read-only? |
|10954|You changed the permissions of '_1' for '_2’. Do you want to assign these permissions now? |
|10955|You don't have permission to open '_1' for exclusive use. Microsoft Access is opening the database for shared access. To open a database for exclusive access, you must have Open Exclusive permission for it. For more information on permissions and who can set them, click Help. |
|10956|;LCID=0x0409|
|10957|You haven't entered a workgroup ID. To ensure that your workgroup information file is unique, enter a unique workgroup ID of up to 20 numbers or letters. Continue without a workgroup ID? |
|10958|The file '_' already exists. Replace existing file? |
|10959|Workgroup Files (*.mdw)|
|10960|Select Workgroup Information File|
|10961|Open|
|10962|You have successfully created the workgroup information file '_’. |
|10964|You have successfully joined the workgroup defined by the workgroup information file '_'|
|10968|Microsoft Access cannot add a digital signature to your file because Access cannot gain exclusive access to the file.  Make sure no other person or program has the file open and try to add the digital signature again.|
|10974|Modifications to the database or project have invalidated the associated digital signature. This may require you to make a trust decision the next time you open the database or project. |
|13000|Switching fields will lose rule changes that have not been applied. Continue and &Discard Changes Continue and &Apply Changes Cancel|
|13001|Publishing an application will unencrypt your data and remove the database password. Do you want to continue?|
|13002|Unable to create or send email for collecting data. Outlook is not configured to collect data via email; please contact your system administrator.|
|13003|Access cannot perform the export operation. Verify that the export table and related tables have numeric primary keys.|
|13004|The Force New Page property cannot be changed on Web reports that contain multiple columns.|
|13005|Microsoft Access is unable to link to list '_1'. The list is not a standard SharePoint list. |
|13006|Microsoft Access was unable to connect to the Web application because your Internet settings are configured to Work Offline.|
|13007|The site '_' was not found.|
|13008|The server is busy. Try again later.|
|13009|Web queries support only the Select query type.|
|13010|Web queries must have relationships between all tables. Add relationships or remove the tables that you do not need.|
|13011|The alias name specified is not valid for Web queries. Please ensure the name does not contain special characters and is not longer than 64 characters.|
|13012|Web queries cannot sort or filter on complex data columns.|
|13013|Microsoft Access was unable to publish the database because your Internet settings are configured to Work Offline.|
|13014|The SetOrderBy action cannot sort on the '_' field.|
|13015|Local variable names must be less than or equal to 64 characters in length, may not start with an equal sign or a space, and may not contain any of the following characters including CR, LF or TAB: .![]/\:*?"<>_#{}%~&.|
|13016|'_' is not in the list of sites your administrator has allowed for publishing or syncing Access Services applications.|
|13017|This expression was last modified using another design environment and contains an unsupported function. The expression cannot be modified here.|
|13018|An error occurred when trying to save the database as a template because the table '_1' could not be exported to a Web compatible format. Run the Compatibility Checker, fix any reported issues, then try again.|
|13019|Data bars are supported only for tables and named queries.|
|13020|This action will delete the template from your hard drive. Do you want to continue?|
|13021|Your template has been successfully saved in '_'|
|13022|This database was created with the 64-bit version of Microsoft Access. Please open it with the 64-bit version of Microsoft Access.|
|13023|This database was created with the 32-bit version of Microsoft Access. Please open it with the 32-bit version of Microsoft Access.|
|13024|Network connectivity was lost while changing caching mode. Some tables may not have changed modes. You will be prompted to change modes again next time your database is closed and opened.|
|13025|All data changes made in disconnected tables will be permanently discarded. If you discard your changes, you will automatically be reconnected to the SharePoint site if a connection exists. Do you want to continue? |
|13026|This macro appears to have been changed in an older version of Access. Do you want to convert the macro to a format compatible with Access 2010? You may lose the current macro formatting. If you would like to keep the formatting and potentially lose changes made in the older version select 'No'.|
|13027|In a Web form, if the Inherit Value List property is false, then the Allow Value List Edits property must also be false.|
|13028|The SetProperty macro action cannot set the Value property on the '_' control.|
|13029|The SetProperty macro action cannot set the Value property because the solution contains links to external data and the database is not trusted.|
|13030|The RunDataMacro macro action failed because the solution contains links to external data and the database is not trusted.|
|13031|The lookup field was not created. The database engine could not lock the table '_1' because it is already in use by another person or process. Verify that the table '_1' and the forms, queries, and reports that use it are closed before creating this lookup field.|
|13032|Your change to the data integrity was not saved. The database engine could not lock the table '_1' because it is already in use by another person or process. Verify that the table '_1' and the forms, queries, and reports that use it are closed before making this change.|
|13033|The '_' link is used by Microsoft Access to maintain your Web application. It may not be deleted or renamed.|
|13034|Created a local copy of '_2' at '_1'.|
|13035|The control '_' cannot be used in Web reports because it uses padding.|
|13036|Either the Web location or application name entered was invalid.|
|13037|Operation did not succeed. Some table properties and data macros could not be saved because there is not a connection to a SharePoint site. To ensure that the properties and macros are saved, restore the connection to the SharePoint site. Data macros can be copied to the Clipboard and pasted back into the macro designer when the connection is restored.|
|13038|Only form, report, or query Web objects can be saved as client objects.|
|13039|Only ACCDB files that are not yet Web databases can be prepared for Web publish.|
|13040|Microsoft Access encountered an error while checking the database '_' for Web compatibility. The conversion failed. |
|13041|Any data changes made while your application was disconnected from the server will be discarded when the database is closed. To save these changes, click the Cancel button and reconnect with the server. Do you want to continue to close the database and discard your data?|
|13042|The server returned a response that Microsoft Access was unable to understand. Please contact your server administrator.|
|13043|The table '_' is in use and cannot be converted to a local table. Close open objects that may have dependencies on this table and ensure no other user has it open.|
|13044|The Outlook task could not be created.|
|13045|The maximum number of nesting levels has been reached.|
|13046|The '_1' argument value is not valid for the macro action '_2'. A Macro with parameters cannot be an error handler. |
|13047|This property cannot be changed because one or more of the tab pages contains a shared or linked image.|
|13048|The Unique property could not be set on the table because duplicate values exist in the field. Remove the duplicate values and try again.|
|13049|Microsoft Access cannot export your object to the database you have selected. Please select a different database or a different object to export.|
|13050|Microsoft Access cannot import the selected objects into the current database because one or more of them are not web compatible. Fix the compatibility errors and try again, or select different objects to import.|
|13051|Microsoft Access cannot export the objects to the selected database because one or more of them are not web compatible. A log table containing the errors encountered can be found in the database to which the export was attempted. Fix the compatibility errors and try again, or select different objects to export.  |
|13052|Object must be open.|
|13053|Object not found.|
|13054|Object not supported.|
|13055|The value of a Display Form property was invalid and was not saved.|
|13056|Web queries may not have more than one join between the same two tables. Extra joins must be removed from this query. Changes to the joins in the query will not affect table relationships.  |
|13057|You can't delete the '_1' field. This field is used to identify and store the rows in your table.|
|13058|The data macro did not run because the table is not on the server. Synchronize the application with the server and then run the data macro.|
|13059|The lookup field was not created. At least one value is needed for a value list.|
|13060|Only Web databases may be published to Access Services. Run "Prepare for Web" on this database first.|
|13061|The Web location you specified does not support Access Services.|
|13062|The data type cannot change on a field that is indexed. Consider setting the index property to false before changing the data type.|
|13063|The Submacro with same name already exists. Enter a different name.|
|13064|Cannot convert the expression '_' for use on the Web.|
|13065|The table is too large for this change to be saved.|
|13066|The change could not be saved to the server.|
|13067|A timeout prevented the change from saving to the server.|
|13068|Fields of type Memo, Binary and Lookup cannot be used in a validation rule for Web tables.|
|13069|Renaming the table failed because there is not a connection to a SharePoint site. Restore the connection to the SharePoint site, synchronize the application with the server, and try the rename operation again.|
|13070|The expression that you entered is not valid for Web-compatible default values.|
|13071|The expression that you entered is not valid for Web-compatible table validation rules.|
|13072|The expression that you entered is not valid for Web-compatible field validation rules.|
|13073|The expression that you entered is not valid for Web-compatible forms.|
|13074|The expression that you entered is not valid for Web-compatible reports.|
|13075|The expression that you entered is not valid for Web-compatible macros.|
|13076|The expression that you entered is not valid for Web-compatible databases.|
|13077|The expression that you entered is not valid for Web-compatible data macros.|
|13078|The expression that you entered is not valid for Web-compatible calculated columns.|
|13079|This template contains client-only information and may not be imported into a Web database.|
|13080|LoadFromAXL is not available when you have disconnected tables. Please reconnect all of your tables before attempting to use LoadFromAXL.|
|13081|The _2 database was not synced because the site '_1' is unavailable.|
|13082|The update to the field did not complete because a dependent form, query, or report is open. Synchronize with the server to complete the update.|
|13083|The server '_' does not support Access Services.|
|13084|The VBA project is password-protected.|
|13085|You can't delete the table '_'; it is participating in one or more relationships. If you want to delete this table, first delete any lookups in this table or that reference this table. |
|13086|Lookup field '_1' contains values that do not exist in the related table '_2'.|
|13087|The RunDataMacro action failed to invoke a data macro on the server. Please check your connectivity to the server.|
|13088|Unable to insert control. The selected control is not valid in web reports.|
|13089|The object cannot be imported because the database is read-only.|
|13090|The table '_' cannot be converted to a local table because it is not web compatible. Fix the compatibility errors and try again.|
|13091|The server requires lookup fields with data integrity to be indexed.|
|13092|Close and reopen the table to see the changes.|
|13093|One or more web compatibility issues were found. The issues are listed in table '_1'.|
|13094|There was an error evaluating web compatibility. Any compatibility issues found before the error are listed in table '_1'.|
|13095|This template's objects are saved in a format that is no longer supported by Access 2010. Open the template in an older version to view its contents.|
|13096|This template's objects are saved in a format that requires a newer version of Access.|
|13097|'_' did not respond. Either the server does not exist, Microsoft Access Services are not enabled on the server, or the server is using an older version of Microsoft Access Services that is not compatible with Access 2010.|
|13098|The version of Microsoft Access Services installed on the server '_' is a newer version that is not compatible with the version of Microsoft Access you are using. You must upgrade Microsoft Access in order to publish to it.|
|13099|Your application was not upgraded to the current version of Microsoft Access. Try again by selecting the Upgrade button.|
|13100|This table already has the maximum number of fields with the same data type as '_'. For the purposes of this calculation, Number and Currency are considered the same data type.|
|13101|Microsoft Access was unable to close database objects.  Please close all opened database objects before trying again.|
|13102|Microsoft Access cannot reconnect one or more of the disconnected tables. Please check network connectivity and server availability.|
|13103|The validation rule contains a syntax error and cannot be saved. A field or function may be misspelled or missing.|
|13104|Unable to insert control. The selected control is not valid in reports.|
|13105|The form or report has run out of memory. New macros cannot be added to the object. Please convert the embedded macros on the object into standalone macros and call them from the embedded macro using the RunMacro macro action.|
|13106|Table properties and data macros could not be saved because the application has changed on the server. Synchronize with the server before making changes. To save your current data, copy and paste your last entered row to another file. Data macros can be copied to the Clipboard and pasted back into the macro designer once you have synchronized.  |
|13107|An error has occurred in an AfterDelete data macro. The delete which triggered the event completed successfully. Please check in the USysApplicationLog table for more details. |
|13108|This template may contain potentially harmful content that will run on creation. If you trust the contents of this template and would like to enable it, click Yes.|
|13109|The OpenReport macro action does not support passing query parameters to the report when the View argument is set to Print.|
|13110|This operation is not supported for Microsoft SQL Server 2008 and later.|
|13111|Referential integrity cannot be enforced between linked tables and local tables. Enforcement of referential integrity will be dropped on all relationships between tables converted to local and tables that remain as links. |
|13112|Web compatibility errors prevented the upgrade from succeeding. Your application has been restored to its pre-upgrade state. To complete the upgrade, you must make a local copy of your application, resolve the compatibility errors, and republish your application to a different site.  |
|13113|The upgrade operation was cancelled. Your application has been restored to its pre-upgrade state and will remain read-only. Save a local copy of your application to make changes. |
|13114|Your application was not upgraded to the current version of Microsoft Access. Your application has been restored to its pre-upgrade state and will remain read-only. You can try again by selecting the Upgrade button, or save a local copy of your application to continue working with it.  |
|13115|Your application was successfully upgraded to the current version of Microsoft Access. A local backup copy of your application has been saved in '_1'. |
|13116|Your application was successfully upgraded to the current version of Microsoft Access.|
|13117|Microsoft Access was unable to retrieve data for one or more tables on the server. Some data may be missing or out of date.|
|13118|The column '_1' cannot be changed or deleted because a calculated field or record validation rule depends on it.|
|13119|Your data macro changes could not be saved because someone else has modified the data macros of this table. To save your changes and overwrite the other user's changes, click "Overwrite". If you click Cancel, you can re-enter the macro designer and copy your changes to the Clipboard, then synchronize with the server, enter the macro designer again and paste your changes back in. &Overwrite Cancel|
|13120|The '_' action requires a valid control name that corresponds to a subform. |
|13121|The _1 event is not available for controls on datasheet Web forms.|
|13122|This form or report contains changes that are incompatible with the current database format. The form or report was not saved. In order to save your changes, you must remove any layouts that have empty cells in them and/or set the HasModule property for the form or report to No. |
|29000|There isn't enough memory to create an input area. Close unneeded programs. Then try to create the input area again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'. |
|29001|Microsoft Access failed to save module '_1’. Your computer may be low on disk space. For information on freeing memory or disk space, search the Microsoft Windows Help index for 'memory, troubleshooting' or 'disk space, freeing'. |
|29002|Microsoft Access failed to create the Visual Basic module '_1’. If your database is on a network drive, check your network connection, and then try again. |
|29003|Microsoft Access failed to convert or enable your code modules. Your computer may be low on disk space or memory. |
|29004|The new procedure name you provided is invalid. |
|29005|The procedure '_' already exists. Choose another procedure name. |
|29006|Microsoft Access failed to create a Debug window toolbar. The system may be out of resource memory. Close unneeded programs and try again. For more information on freeing memory, search the Microsoft Windows Help index for 'memory, troubleshooting'.  |
|29007|During the paste or import operation, Microsoft Access failed to convert the '_1' module from an earlier version of a Microsoft Access database. Your computer may be low on disk space or memory. |
|29008|Microsoft Access couldn't create storage space for a Visual Basic module. If your database is on a network drive, check your network connection, and then try again. |
|29009|Microsoft Access couldn't open the storage space for a Visual Basic module. Your computer may be low on disk space. For information on freeing memory or disk space, search the Microsoft Windows Help index for 'memory, troubleshooting' or 'disk space, freeing'. |
|29010|The function name is too long. Microsoft Access will truncate the function name to 255 characters. |
|29011|Microsoft Access failed to save the database. Your computer may be low on disk space. For information on freeing memory or disk space, search the Microsoft Windows Help index for 'memory, troubleshooting' or 'disk space, freeing'. |
|29013|This action will reset the current code in break mode. Do you want to stop the running code?  * To halt the execution of the program so the Module window can be closed, select Yes. * To leave the code in the current state, select No.|
|29014|You can't add a reference to a Microsoft Access workgroup. |
|29015|You can't remove this reference. Microsoft Access needs this reference in order to work properly. |
|29016|You can't add a reference to the open database. |
|29017|The database _1 was created in an earlier version of Microsoft Access. Convert this database to the current version of Microsoft Access by using the Convert Database command on the Tools menu (Database Utilities submenu). |
|29018|You can't use a standard module for this operation. |
|29019|The module name '_' is invalid. * The module name may start with the prefix Form_ or Report_. * The module can't be named Forms, Reports, Modules, Application, Screen, Assistant, CommandBars, References, or DoCmd. * The module name may have too many characters. |
|29020|Another user has modified this database. To see the current version, close the database and open it again. |
|29021|This operation isn't available while there is a module in break mode. Reset the executing code and try the operation again. |
|29022|There were compilation errors during the conversion or enabling of this database. The database has not been saved in a compiled state. The performance of this database will be impaired because Microsoft Access will need to recompile the database for each session. For information on improving performance, click Help. |
|29023|There are calls to 16-bit dynamic-link libraries (.dll) in modules in this database. These won't work under Microsoft Windows 95 or Microsoft Windows NT. Change your code to call equivalent 32-bit dynamic-link libraries (.dll). 2|
|29024|The current user account doesn't have permission to convert or enable this database. To convert or enable a database, ensure the following: * You must join the workgroup that defines the user accounts used to access the database. * Your user account must have Open/Run and Open Exclusive permissions for the database object. * Your user account must have Modify Design or Administer permissions for all tables in the database, or it must be the owner of all tables in the database. * Your user account must have Read Design permission for all objects in the database. * Request that other users close the database. |
|29025|There are calls to 16-bit dynamic-link libraries (.dll) in modules in this database. These won't work under Microsoft Windows 95 or Microsoft Windows NT. Translate these calls to equivalent 32-bit dynamic-link libraries. 2|
|29026|The database that you are trying to open or convert is currently in use, or you do not have permission to open it exclusively. When you convert a database, or the first time you open an earlier version database, another user can't have the database open. Try one of the following: * Request that other users close the database. * Using the version of Microsoft Access in which the database is written, have your workgroup administrator grant you the permission to open the database exclusively. Multiple users can share the database after you convert it, or after you open it for the first time.  |
|29027|Microsoft Access was unable to save the project. Another user is saving the project now. Do you want to retry? |
|29028|The Save operation failed. |
|29029|You are trying to open a read-only database. The first time you open an earlier version database, you must be able to write changes to the database. * The database file's read-only attribute may be set; clear this attribute. * You may have chosen the Open Read Only command in the Open dialog box. Don't choose this command the first time you open the database. * Your license to use this application may have expired. |
|29030|Microsoft Access can't establish a reference to the specified database. The referenced database can't be found, or it's locked exclusively by another user so it can't be opened. Restore the referenced database from a backup copy or ask the user that has the database locked to open the database in nonexclusive mode.  |
|29031|Microsoft Access can't create or open the requested database now. The database may be locked exclusively by another user. |
|29032|The '_1' module is open; Microsoft Access can't set the permissions on an open module. |
|29033|Microsoft Access can't add references to a replicated database; changes will be ignored. |
|29034|Microsoft Access is unable to import the module from the source replica. Synchronize the source database. |
|29040|Microsoft Access is currently unable to rename the form, report, or module to '_1'. Close the database, reopen it, and then try the rename operation again. |
|29041|One or more forms or reports contains an ActiveX control that could not be loaded. These controls will not function properly until you register them, open the forms or reports in Design view, and save the forms or reports. |
|29042|One or more forms or reports contains an ActiveX control that could not be loaded. These controls will not function properly until you register them, open the forms or reports in Design view, and save the forms or reports. |
|29043|Microsoft Access has converted the code in '_1' to your current version of Visual Basic. To improve the performance of this database, do the following: 1. Open any module in this database in Design view. 2. On the Debug menu, click Compile And Save All Modules. |
|29044|DAO version 3.0 is not compatible with this version of Microsoft Access. Create a reference to DAO version 3.5. For information on setting references, click Help. |
|29045|You can't import, export, create, modify, or rename any forms, reports, pages or modules in an ACCDE, MDE or ADE database. |
|29046|You can't create a module for a form or report if it is read-only, or if the database is read-only. |
|29047|Microsoft Access was unable to open the Visual Basic project for this database. Another user is saving the project now. Do you want to retry? |
|29048|MDE databases can't reference MDB databases. MDE databases can only reference other MDE databases or type libraries. |
|29049|To add a code module to a form or report, you must switch to Design view and set the HasModule property of the form or report to Yes. |
|29050|Microsoft Access couldn't compile this database because one or more references couldn't be resolved. The database has not been saved in a compiled state. The performance of this database will be slower because Microsoft Access will need to recompile the database for each session. For information on improving performance, click Help. 2|
|29051|The current user account doesn't have permission to make an MDE file from this database. To make an MDE file, do the following: * Join the workgroup that defines the user accounts used to access the database. * Make sure the user account has Open/Run and Open Exclusive permissions for the database object. * Make sure the user account has Modify Design or Administer permissions for the MSysModules2 table in the database. * Request that other users close the database. |
|29052|The Visual Basic for Applications project in the database '_' can't be converted to the current Visual Basic format. If the database is an MDE, you will need to rebuild the MDE from the source MDB. If you don't have the source MDB, you will need to obtain a new version of the MDE that is compatible with the current version of Visual Basic. |
|29053|Microsoft Access can't create any more controls on this form or report. If you have deleted controls from this form or report in the past, you may be able to rename the form or report and then add more controls to it. |
|29054|Microsoft Access can't add, rename, or delete the control(s) you requested. |
|29055|The form or report '_1' has too many controls. Reduce the number of controls on the form or report before using it in this version of Microsoft Access. |
|29056|Someone else is using '_' and the Visual Basic for Applications project does not match your version of Visual Basic. You need to open the database exclusively in order to upgrade the Visual Basic for Applications project in this database. |
|29057|The database '_' is read-only and the Visual Basic for Applications project does not match your version of Visual Basic. You need to open the database read-write in order to upgrade the Visual Basic for Applications project in this database. |
|29058|You can't create a module for a form or report while an instance of the form or report is in browse mode. |
|29059|Microsoft Access was unable do display the module. |
|29060|File not found. |
|29061|There were compilation errors during the conversion or enabling of this database. This might be due to old DAO syntax that is no longer supported. For an example on how to fixup the code, click Help. |
|29062|The module name '_' is misspelled or refers to a module that doesn't exist. If the invalid module name is in a macro, an Action Failed dialog box will display the macro name and the macro's arguments after you click OK. Open the Macro window, and enter the correct module name. |
|29063|The Visual Basic for Applications project in the database is corrupt.|
|29064|You do not have exclusive access to the database at this time. If you proceed to make changes, you may not be able to save them later.|
|29065|Microsoft Access can't save design changes or save to a new database object because another user has the file open. To save your design changes or to save to a new object, you must have exclusive access to the file.|
|29066|You do not have exclusive access to the database. Your design changes cannot be saved at this time. Do you want to close without saving your changes?|
|29067|You do not have exclusive access to the database. You cannot convert this database at this time.|
|29068|Microsoft Access cannot complete this operation. You must stop the code and try again.|
|29069|Microsoft Access cannot save the digital signature at this time. * You may be in a database under Source Code Control. * You may be in a database which is read only. * The database uses either the *.accdb or *.accde file name extension. To sign such a database, click the File tab, point to the Publish menu, and then click Package and Sign. |
|29070|Your Microsoft Access database or project contains a missing or broken reference to the file '_1'_2. * To ensure that your database or project works properly, you must fix this reference.|
|29071|Missing or broken VBE reference to the file '_1'.|
|29072|Microsoft Access has detected corruption in this file. To try to repair the corruption, first make a backup copy of the file. Click the File tab, point to Manage and then click Compact and Repair Database. If you are currently trying to repair this corruption, you need to recreate this file or restore it from a previous backup. |
|29073|You do not have exclusive access to the database. You cannot continue with the Source Code Control command.|
|29074|Microsoft Access failed to create the Visual Basic module. If your database is on a network drive, check your network connection, and then try again. |
|29075|Access cannot add a digital signature to a file created in Access 97 or earlier. Convert the file to Access 2000 or later file format, and then add a digital signature. |
|29076|There was a problem with the digital certificate. The VBA project could not be signed. The signature will be discarded. |
|29079|Microsoft Access is unable to read the VBA modules in this database and cannot recover the modules because the file is read-only. To recover the VBA modules, close the database and make a backup copy of the database. Then open the database with read/write permission. |
|29080|The VBA modules in this database appear to have been saved with errors. Access can recover the modules, but you should backup the database first. To cancel so you can make a backup copy of the database, click Cancel. Then make a backup copy of the database.  If you have a backup copy of the database, click OK. When the database opens, examine the modules to see that they are correct. If they are not, you should revert to a recent backup.|
|29081|The database cannot be opened because the VBA project contained in it cannot be read. The database can be opened only if the VBA project is first deleted. Deleting the VBA project removes all code from modules, forms and reports. You should back up your database before attempting to open the database and delete the VBA project. To create a backup copy, click Cancel and then make a backup copy of your database. To open the database and delete the VBA project without creating a backup copy, click OK. |
|29082|The version of Access used to save this database cannot be detected. This problem cannot be corrected because the database is read-only. Click OK, and after the database closes, make a backup copy of the database. Then open the database with read/write permission. |
|29083|The object '_1' could not be imported because it contains elements from the namespace '_2', which is no longer supported. Use an older version of Access to import the object.|
|29084|The object '_1' could not be imported because it contains elements from the namespace '_2', which is from a newer version of Access. Use a newer version of Access to import the object.|
|29085|The object '_1' could not be imported because it contains elements from the namespace '_2', which Access does not understand.|
|29086|The object '_1' could not be imported from the file '_2'. Ensure that the file exists and is in the correct format.|
|30000|Microsoft Access could not find the SQL Server specified. Verify that the server name is correct.|
|30001|Microsoft Access could not find the database on the server. Verify that the database name is correct.|
|30002|Microsoft Access could not log on to the server. Verify that the log on information is correct.|
|30004|The Default Max Records setting must be between 0 and 2147483647.|
|30005|Missing SQL command|
|30006|Access was not able to perform this operation because the project is not connected to a SQL Server database.|
|30007|Can't sort on one or more fields specified in 'Order By' property or sorting/grouping dialog.|
|30008|The server you are trying to access is case sensitive. Objects of the same name but of different case are not supported. Using these objects may result in a loss of data.|
|30009|Another user or application has deleted this record or changed the value of its primary key.|
|30010|Cannot apply Filter on one or more fields specified in the Filter property.|
|30011|Cannot get column information for the database object being browsed.|
|30012|A Server Filter cannot be applied to a stored procedure Record Source. Filter not applied.|
|30013|You can't update the record because another user or application deleted it or changed the value of its primary key.|
|30014|The data was added to the database but the data won't be displayed in the form because it doesn't satisfy the criteria in the underlying record source.|
|30015|The specified record source contains duplicate names for some output fields. Use aliases in your SELECT statement to give each field a unique name.|
|30016|The field '_' is read only.|
|30017|This database is not enabled for publication.|
|30018|Cannot create objects of type '_' against current SQL backend. Please check your permissions and server setup.|
|30019|Microsoft Access cannot copy a table with a name that is longer than 64 characters.|
|30020|Cannot find column '_'.|
|30021|Cannot use domain functions on stored procedures.|
|30022|The Access project could not be opened. You may not have adequate permissions, or the project may be read-only.|
|30023|Cannot use domain functions on scalar functions.|
|30024|Your password will not be encrypted before it is saved to the file. Users who view the source contents of the file will be able to see the account user name and password. &Save Password Cancel |
|30025|Invalid SQL Statement. Check the server filter on the form record source|
|30026|Access is unable to connect to '_1' database on the '_2' server. Make sure that the database still exists and that the server is running.|
|30027|The value you entered is not consistent with the data type or length of the column.|
|30028|The database name for the attached database file is blank. Enter a name for your database.|
|30029|This version of Microsoft Access does not support design changes with the version of Microsoft SQL Server to which your Access project is connected. See the Microsoft Office Update Web site for the latest information and downloads. Your design changes will not be saved.|
|31000|You must specify criteria for all conditional formats.|
|31001|A conditional format expression can't be longer than 435 characters. |
|31002|Microsoft Access can't modify the control's conditional formats right now. Your program tried to modify the conditional format of a control while the conditional format was being evaluated. |
|31003|The setting for this property is too long. You can enter up to either 255 or 2,048 characters for this property, depending on the data type. |
|31004|The value of an (AutoNumber) field cannot be retrived prior to being saved. Please save the record that contains the (AutoNumber) field prior to performing this action. |
|31005|Access failed to evaluate one or more expressions because '_' was referenced in an expression. Only functions and properties that are considered to be safe are allowed in expressions when Access runs in sandbox mode. For more information on sandbox mode, search for "Sandbox mode" in Access Help.  |
|31006|This method is not supported for Attachment controls in the current database file format.|
|31007|This operation is not supported for Web browser controls in the current database file format.|
|31008|This method is not supported for Empty Cell controls in the current database file format.|
|31009|This method is not supported for creating Empty Cell controls.|
|31010|This operation is not supported for navigation controls in the current database file format.|
|31011|The expression that you entered is not valid for Web-compatible queries. |
|31012|You either have an error in your expression or you have attempted to use an undeclared parameter. Check the expression for errors or enter the parameter '_' in the Query Parameters dialog. |
|31500|Microsoft Access can't send this format in a mail message. To send this format in a mail message, click Export on the File menu, or use the OutputTo method to create the files and attach them to a mail message. |
|31501|The selected transform file '_' cannot be found. Please select a different transform.|
|31502|Microsoft Access can't find the Microsoft Office international dynamic-link library (DLL). Reinstall Microsoft Office. |
|31503|The specified transform did not successfully convert the data. Select a different transform.|
|31504|The data was transformed in a format that cannot be imported.|
|31505|Are you sure you want to remove '_' from the list of available transforms? This will not actually delete the transform file.|
|31506|The selected transform file cannot be found. Select a different transform or remove the transform. Then try to export again.|
|31507|Invalid field data type.|
|31508|Cannot load the Office Web Components. Access had a problem loading the Office Web Components Version 9.0. Try going through set-up to re-install them|
|31509|Cannot load the Office Web Components. Access had a problem loading the Office Web Components Version 10.0. Try going through set-up to re-install them.|
|31510|Microsoft Access can't load the dynamic-link library _1.  You need to resolve the failure and start Access again.|
|31511|You have chosen to alter the mode in which SQL syntax will be interpreted in this database. This will mean:  * Existing queries may return different results or not run at all. * The range of data-types and reserved words will change. * Different wildcards will be used.  It is recommended that you make a backup copy of this database before continuing. If you agree to continue, Access will close this database, compact it, and re-open in the new mode. Select OK to continue.|
|31512|Microsoft Access can't open the mail session. You may not have configured an e-mail client on this computer. Ensure that your e-mail software has been installed and configured properly. |
|31513|All open objects must be closed prior to continuing this operation. Do you want Microsoft Access to close the objects? |
|31514|Microsoft Access can't convert this database to a different version because one or more objects are open. Close all objects and try again. |
|31515|Your database is using SQL Server Compatible Syntax (ANSI 92) for which Access 2000 has limited support. This will mean: * Access 2000 users may not be able to see some queries. * Existing queries may return different results or not run at all. * The range of data-types and reserved words will change. * Different wildcards will be used.  It is recommended that you change the SQL Server Compatible Syntax (ANSI 92) setting before converting this database to the Access 2000 file format. Select Cancel to abort the conversion process and change the SQL Server Compatible Syntax (ANSI 92) setting.|
|31516|Microsoft Access can't convert this database to the requested version because it has been 'enabled' to work with newer versions of Microsoft Access than it was originally created with. You can convert this database to a newer format, and then retry this operation with the newly converted database. |
|31517|Microsoft Access can't convert this database to the requested version because it is from an older version of Microsoft Access. You can convert this database to a newer format, and then retry this operation with the newly converted database. |
|31518|The following controls contain expressions that can't be represented in a Microsoft Access project PivotTable or PivotChart view: _1. Do you still want to switch to PivotTable or PivotChart view? |
|31519|You cannot import this file. You cannot import a text file unless it has one of these extensions: _. |
|31520|You cannot import this file. You cannot import a text file that has one of these extensions: _. |
|31521|Cannot establish connection to the server. |
|31522|Microsoft SQL Server Desktop Engine must be on the machine running Microsoft Access. |
|31523|Microsoft Access was unable to open the file '_1'. The file may not be an Access file, the path or filename specified may be incorrect, or the file can not be opened exclusively. To repair the file manually, on the File menu, point to Manage Database, and then click Compact and Repair Database. |
|31524|Microsoft Access can't transfer the user-defined function '_1'. User-defined functions can only be transferred between Microsoft SQL Servers that are version 8.0 or higher. |
|31525|You can copy and paste this object only if at least Microsoft SQL Server 2000 is installed on both the source and destination servers. |
|31526|You cannot create a Microsoft Access ADE or MDE file from a database saved in Microsoft Access 2000 format. Convert the database to the current version of Microsoft Access. Then create the ADE or MDE file. |
|31527|Path not found: _. |
|31528|Table '_' is in use. Please close the table and retry import. |
|31529|View '_' is in use. Please close the view and retry import. |
|31530|Procedure '_' is in use. Please close the procedure and retry import. |
|31531|Microsoft Access cannot create table(s) using the information contained in the document(s) you are trying to import. |
|31532|Microsoft Access was unable to export the data. |
|31533|Invalid filename. |
|31534|Your database is using SQL Server Compatible Syntax (ANSI 92) for which Access 97 has limited support. This will mean: * Existing queries may return different results or not run at all. * The range of data-types and reserved words will change. * Different wildcards will be used.  It is recommended that you change the SQL Server Compatible Syntax (ANSI 92) setting before converting this database to the Access 97 file format. Select Cancel to abort the conversion process and change the SQL Server Compatible Syntax (ANSI 92) setting.|
|31535|Invalid parameters for ExportXML. Press Help for more information. |
|31536|The XML schema properties do not match the existing table structures. Overwrite existing structures? |
|31537|There is already an object named '_' in the database. |
|31538|Microsoft Access cannot create this file. To create the file, you must give the file a unique name, provide a valid connection string and user ID, and you must have the necessary connection and file creation permissions. |
|31539|There was an error loading the XSL transformation file '_1'. Please make sure that the file is a properly formatted XSL file and is the correct file to use for the data supplied. _2|
|31540|There was an error loading the XML data file '_1'. Please make sure that the file is a properly formatted XML file. _2|
|31541|Microsoft Access cannot open this file. This file is located outside your intranet or on an untrusted site. Microsoft Access will not open the file due to potential security problems.  To open the file, copy it to your machine or an accessible network location.|
|31542|The startup action associated with this file may not be safe. Do you want to execute the startup action as you open the file? |
|31543|The startup action associated with this file may not be safe. If you choose not to execute the startup action, Microsoft Access will not open the file. Do you want to execute the startup action and open the file? |
|31544|Microsoft Access cannot open this file. The author of this file has set it to open only when a startup action is executed, and Microsoft Access is currently set to disable startup actions. To address this problem, contact the author of the file or your system administrator. 2|
|31545|Microsoft Access cannot open this file. The Internet Security Zone Manager is missing or not correctly installed and therefore the safety of this file cannot be verified. To address this problem, reinstall Windows Internet Explorer or the appropriate Office System Pack. 2|
|31546|This database is a Microsoft Access 7.0/8.0/9.0 Design Master. If you click OK, the database you selected will be renamed to _1 and then converted to _2. Everyone using a replica of this database will have to upgrade to Microsoft Access 2002 after the next synchronization. |
|31547|Microsoft Access cannot export this object to XML when it is open in Print Preview. Close the object and try again. |
|31548|Microsoft Access cannot export a table to XML when it is open in Design. Close the object and try again. |
|31549|Finished importing document '_'.|
|31550|Not all of your data was successfully imported. Error descriptions with associated row numbers of bad records can be found in the Microsoft Access table '_1'.|
|31551|Only XML files can be exported to an Internet address (http://, ftp://). Enter a path that points to a location on your computer or on the network. |
|31552|The document(s) you are importing contain XML Schema (XSD) information created outside Microsoft Access. XML Schema information created outside Microsoft Access is not supported. If you continue, Microsoft Access will only import the data, and ignore the XML Schema. Would you like to continue? |
|31553|The Action Tag property value cannot exceed 1024 characters.|
|31554|You are attempting to apply too many Action Tags. Please uncheck some of the selected tags. |
|31555|The specified transform did not successfully convert the data. Would you like to save a copy of the Data to Export at '_' for troubleshooting?|
|31556|To view object dependencies or change the Track name AutoCorrect info option, Microsoft Access must close all objects and update dependency information. This could take several minutes. Do you want to continue? |
|31557|Microsoft Access cannot update dependency information because one or more objects are open. Close all objects and try again. |
|31558|The Track name AutoCorrect info option generates name maps for the objects in the database. This may take several minutes. Do you want to leave this option turned on? |
|31559|Microsoft Access could not connect to the site you specified. Verify the address of the site or contact your site administrator.|
|31560|The site you specified does not support linking to a Microsoft Access database. The site must be running Microsoft SharePoint Foundation.|
|31561|The site you specified does not support importing data to a Microsoft Access database. The site must be running Microsoft SharePoint Foundation.|
|31562|The site you specified does not contain any lists. Verify the address of the site, or contact your site administrator.|
|31563|Microsoft Access could not enable name AutoCorrect for one or more objects because Access could not open and save the objects. An open or save operation could have failed due to one or more of the following reasons - the object is already open, you don't have permissions to open the object in Design view, the database is read-only, or the source of a linked table could not be found. |
|31564|To generate object dependencies, the Track name AutoCorrect info option must be turned on. Enable name AutoCorrect and continue? |
|31565|Dependency information needs to be updated before you can view object dependencies. This could take several minutes. Do you want to continue? |
|31566|The object dependencies feature is not enabled for this database because dependency info needs to be updated. |
|31567|The object that you have selected in the Object Dependencies pane no longer exists in the database. |
|31568|You do not have the necessary permissions to generate dependency information for the selected object. |
|31569|To view dependency information, you must select a table, query, form, or report. |
|31570|Dependency information cannot be generated because name maps are either missing or outdated. Turn on the Track name AutoCorrect info option, make sure you have sufficient permissions to open an object in Design view, and make sure the database is not read-only. |
|31571|Dependency information cannot be generated because name maps are outdated, and the database is read-only. |
|31572|Microsoft Access cannot generate dependency information for a new unsaved object. Save the new object and then check for object dependencies. |
|31573|Microsoft Access cannot generate dependency information for objects in a project. |
|31574|Microsoft Access cannot export unbound forms or reports as XML documents.|
|31575|All open objects must be closed prior to backing up your database or project. Do you want Microsoft Access to close the objects? |
|31576|Microsoft Access cannot backup this database or project because one or more objects are open. Close all objects and try again. |
|31577|Microsoft Access could not connect to the site you specified. Verify the address of the site or contact your site administrator.|
|31579|The table could not be exported because you do not have sufficient permissions. Please contact your site administrator.|
|31580|A list with the specified name already exists. Type a different name and try again.|
|31581|The list name cannot contain any of the following characters: \/:*?"<>_. Type a different name and try again.|
|31582|Microsoft Access cannot generate dependency information for objects in a replica database. |
|31583|Finished importing from '_'.|
|31584|Finished linking to '_'.|
|31585|Finished exporting '_1' to '_2'.|
|31586|The site you specified does not support importing data from a Microsoft Access database. The site must be running Microsoft SharePoint Foundation.|
|31587|Microsoft Access cannot generate dependency information because the Track Name AutoCorrect Info option is turned off. This option cannot be turned on because the database is either read-only or you do not have sufficient permissions. |
|31588|Error exporting picture file '_1' to specified path '_2'.|
|31589|Specified transform failed to successfully transform your data. _|
|31590|A list named '_' already exists. You must either use a different name or go to the site and delete the list.|
|31591|Microsoft Access is unable to export '_1' to Microsoft SharePoint Foundation due to limitations on the number of times each data type can appear in a Microsoft SharePoint Foundation list. See help to find out exactly what the restrictions are and for details on how to create a query that will define the Microsoft SharePoint Foundation list that you would like to create. |
|31592|Cannot find the specified schema file '_'. Remove or update the file reference, and try again to import. |
|31593|Microsoft Access has encountered an error processing the XML schema in file '_1'. _2|
|31594|Microsoft Access was unable to export XML data to '_1'. |
|31595|This database was saved in the Microsoft Access _ file format. For information on how to import data from this database into a new file, click Office.com on the Getting Started with Microsoft Access page. |
|31596|Cannot load the specification '_'. Try re-creating the specification. |
|31597|The specification XML failed to validate against the schema. There is an error in the following line of the XML document: _. |
|31598|The specification XML failed to validate against the schema. There is an error in the following specification XML string: _1 Possible value could be one of following '_2'. |
|31599|The specified name '_1' cannot be set due to one of the following reasons: • The specified name is not unique. • The specified name is blank. • The specified name does not follow Microsoft Access object-naming rules. |
|31600|The specified description '_' is too long. The description cannot exceed 255 characters. |
|31601|The specification does not contain a description. '_'. |
|31602|The specification with the specified index does not exist. Specify a different index. '_'. |
|31603|The name '_' is already in use. Enter a different name or click on the Manage Data Tasks button to rename the existing specification. |
|31604|The required element is missing under and for a fixed width format. |
|31605|The Width attribute is required for all elements under or for a fixed width format. |
|31606|Cannot open the destination database. If the database is open, close it, and then try again. |
|31607|The Path attribute is required for all formats except ImportWSS, ImportOutlook and ExportXML.|
|31608|Specify a name for the import specification in the Save As box.|
|31609|Specify a name for the export specification in the Save As box.|
|31610|Do you want to delete '_'?|
|31611|Specify the name of the source or destination file in the File name box.|
|31612|The specification failed to execute. Try re-creating the specification.|
|31613|You have selected "Tables and Related Views" as the way to group objects in the database. Access needs to update information on object dependencies to create the groups. This will take some time for large databases. Do you want to continue? |
|31614|Are you sure you want to delete the item '_' from the Categories list?  Note that the individual groups and the shortcuts to the objects in the database will be deleted, but the objects themselves will not be deleted.|
|31615|Are you sure you want to delete the group?  Note that only the shortcuts, and not the actual objects will be deleted.|
|31616|Duplicate Item Name. An item with the name _ already exists in the Categories list. Specify a different name for the item.|
|31617|Duplicate Group Name. A group named _ already exists. Specify a different name for the group.|
|31618|Blank Item Name. You have not specified a name for the item that you added to the Categories list. |
|You have not specified a name for the group.||
|31619|Blank Group Name.|
|31620|This will clear all the contents in your MSysNavPaneXXX system tables. Do you want to continue? |
|31621|There is already a shortcut named '_1' to the '_2' object in this group. |
|31622|The table description cannot be changed for a linked table|
|31623|Microsoft Access has imported 'Favorites' and other groups into the converted database. If you change the imported groups in Access 2007 or greater, you will not see those changes if you open the database in an older version of Access. Also, if you change the imported groups by using an older version of Access, you will not see those changes if you open the database in Access 2007 or greater. |
|31624|Enter a valid date. |
|31625|Enter a valid value. |
|31626|_ issues were found that could result in unwanted behavior or missing data. Do you want to cancel the wizard and review the issues?  Click No to ignore the issues and continue with the migration.|
|31627|A copy of the database has moved to _. Continue opening current file?|
|31628|Moving to SharePoint is not supported for databases that have the AllowBypassKey property disabled.|
|31629|Template '_1' could not be instantiated. _2|
|31630|Failed to set the database property '_'.|
|31631|The object name '_' could not be used because it doesn't follow object-naming rules.|
|31632|Failed to load '_1' into '_2'.|
|31633|'_' is not a supported AccessObject type.|
|31634|Failed to create relationship '_'.|
|31635|Manifest.xml does not match the defined schema. _|
|31636|Tables.xml does not exist.|
|31637|This template cannot be used because it requires a later version of Microsoft Access. See Office.com for the latest templates you can use with your current version of Microsoft Access.|
|31638|Errors were encountered while loading Tables.xml.|
|31639|Manifest.xml does not exist.|
|31640|Relationships.xml does not match the required schema.|
|31641|An unexpected error occurred when opening the template.|
|31642|NavPane.xml does not match the required schema.|
|31643|'_' is not a valid template.|
|31644|Failed to create a database with the specified CollatingOrder.|
|31645|Failed to set _1 on _2 '_3'.|
|31646|Failed to load NavPane.xml.|
|31647|An error occurred while flipping _1 '_2' right-to-left.|
|31648|An error occurred while performing Name AutoCorrect.|
|31649|An error occurred while fixing up the labels on _1 '_2'.|
|31650|The source file '_1' for _2 '_3' does not exist.|
|31651|The object name '_' could not be used because another object with the same name already exists.|
|31652|You do not have exclusive access to the database. Your SharePoint Lists can not be taken offline or cached at this time. Try again later.|
|31653|Microsoft Access is unable to take your lists offline. A circular relationship was detected.|
|31654|Access denied. You do not have permissions to SharePoint server. Verify permissions, and retry this operation.|
|31655|Microsoft Outlook could not be started. Make sure Outlook is installed and properly set up on your computer.|
|31656|You must have Microsoft Office Outlook 2007 or later on this computer to enable this feature.|
|31657|You cannot set up a collecting data using e-mail task, because your database has reached its maximum size limit. Free some disk space and try again.|
|31658|The selected table or query does not have any fields that support collecting data using e-mail.|
|31659|_ is read-only. You cannot use e-mail messages to collect or update data on this object.|
|31660|You cannot collect data for action queries, SQL-specific queries, and parameter queries by using e-mail messages.|
|31661|The e-mail address field you have specified does not contain any valid addresses. Specify another field.|
|31662|The value you have entered is not valid for the maximum number of replies.|
|31663|The value you have entered is not valid for the date and time to stop.|
|31664|The date and time to stop processing replies has to be greater than the current date and time.|
|31666|The UpdateDependencyInfo method could not update one or more objects. Microsoft Access has created a Name AutoCorrect Save Failures table in the current database with more information about the failing objects. |
|31667|Do you want to delete all of the selected objects? Deleting these objects will remove them from all groups. You cannot undelete forms, reports, and modules. |
|31668|Microsoft Access does not support opening HTML pages.  To open a Data Access Page, use Windows Internet Explorer or Microsoft Access 2003 or earlier.|
|31669|Design changes are not supported for Data Access Pages.  To change the design of a Data Access Page, use Microsoft Access 2003 or earlier.|
|31670|Microsoft Access does not support this operation for Data Access Pages. To perform this operation, use Microsoft Access 2003 or earlier. |
|31672|This operation is not supported for SharePoint linked tables. To manage SharePoint linked tables, point to Get External Data on the Data menu, and then click SharePoint List.|
|31673|Changes have been made to this database by an older version of Access or by an external source. In order to use the Tables and Related Views group in the Navigation Pane the dependency tree must be rebuilt. This may take a few moments. Do you want to continue? |
|31674|Do you want to delete all of the selected objects? Deleting these objects will remove them from all groups. |
|31675|You cannot create an .accde or .mde file from a disabled (non-trusted) database. If you trust the source of this database, you can enable it by using the Message bar. |
|32000|The '_1' macro action cannot run with the specified '_2' argument either when in disabled mode or when called from an embedded macro. |
|32001|The '_' RunCommand cannot be run in disabled mode. |
|32002|Cannot open or run macro '_', because it is not valid. |
|32003|The '_' action requires a valid control name that corresponds to a subform or subreport. |
|32004|The control name '_' is misspelled or refers to a control that doesn't exist. If the invalid control name is in a macro, an Action Failed dialog box will display the macro name and the macro's arguments after you click OK. Open the Macro window, and enter the correct control name. |
|32005|One or more actions are unknown and cannot be pasted. |
|32006|Microsoft Access could not understand the macro format.|
|32007|The '_1' macro action needs a value for the '_2' argument. |
|32008|The '_1' macro action has an invalid value for the '_2' argument. |
|32009|The SubMacro needs a valid name argument|
|32010|The text you entered is not an item in the list.|
|32011|Microsoft Access can't parse the expression: '_1'.|
|32012|The ApplyOrderBy action requires a valid control name that corresponds to a subform or subreport. |
|32013|An unknown error has occurred in a data macro. |
|32014|Are you sure you wish to permanently delete _1 from your table? If the table is open, the change will not be committed until the table is saved.|
|32015|Deletion failed for '_1'.|
|32016|Rename failed for '_1'.|
|32017|The SQL statement is not valid.|
|32018|Changes may not have been saved.|
|32019|Time-out error. Server processing failed.|
|32020|An error occurred while checking for the status of a Data Macro. To view the error, look up the DataMacroInstanceID '_1' in the USysApplicationLog table. |
|32021|The If and Else If conditions are required arguments.|
|32022|The macro cannot be imported because it is invalid.|
|32023|The '_1' action is not valid for the current context.|
|32024|The data macro cannot support any more parameters.|
|32500|The group '_' you entered is invalid. |
|32501|The category '_' you entered is invalid. |
|32502|The value you entered isn't valid for the field '_'. For example, you may have entered text in a numeric field or a number that is greater than the FieldSize setting permits. |
|32503|Support for export to .PDF or .XPS is not installed on this computer. |
|32504|The text you entered isn't an item in the list. Do you want to edit the items in the list? |
|32505|Microsoft Access has recovered this database. Examine the database to verify that there are no missing database objects. |
|32506|Microsoft Access has recovered this database, but one or more tables were found to contain data corruption and were deleted. Deleted tables were logged in the MSysRecoveryErrors table. This table is currently displayed in the datasheet. |
|32507|Microsoft Access has recovered this database, but one or more tables were found to contain data corruption and were deleted. Deleted tables were logged in the MSysRecoveryErrors table. |
|32508|The 'Save as Outlook Contact' command failed. |
|32509|The 'Add from Outlook' command failed. |
|32510|Microsoft Access could not find any fields that match the fields in a Microsoft Outlook Contact. |
|32511|Successfully added _ contact(s). |
|32512|You cannot save this database in an earlier version format, because it uses features that require the current file format. These features include attachments, multi-valued fields, offline data, data macros, calculated columns, links to unsupported external files, newer sort orders, newer encryption types, and navigation controls. |
|32513|All open objects must be closed before you save the database. Do you want Microsoft Access to close all open objects? |
|32514|Microsoft Access can't save this database to a different version, because one or more objects are open. Close all objects, and then try again. |
|32515|You are about to delete the list and all its data from the SharePoint site. Do you want to continue?|
|32516|The filter string you entered is not valid or you cannot apply a filter at this time.|
|32517|This SharePoint list or one of related lists doesn't exist on the SharePoint site.|
|32518|The following errors have occurred: |
|32519|The value specified for the Type parameter is invalid. Specify one of the following: Report, Form, Query, or Table.|
|32520|The repair operation was cancelled. To repair the file manually, on the File menu, point to Manage Database, and then click Compact and Repair Database.|
|32521|You can't change the value of this property in the OnPaint event. |
|32522|No contacts were imported. None of the fields have the necessary '_' property specified. |
|32523|A custom macro in this report has failed to run, and is preventing the report from rendering. |
|32524|Access could not create '_1'. _2. Make sure that the file name (and location, if provided) are in the correct format, such as c:\location\file name.  |
|32525|Access could not download the template from Office.com.|
|32526|You attempted to create an .mdb file on a SharePoint site. You can create only Access 2007 database (.accdb) files on SharePoint sites. Either change the file type or select a different location.|
|32527|You attempted to create an .adp file on a SharePoint site. You can create only Access 2007 database (.accdb) files on SharePoint sites. Either change the file type or select a different location.|
|32528|The application is disabled, and databases may not be created.|
|32529|This operation is not supported for parameterized queries or objects based on parameterized queries. |
|32530|Sharing to Microsoft SharePoint Foundation requires the database to be in an Access 2007 or greater format.|
|32531|Microsoft Access failed to create the SharePoint list. This could be because you don't have the necessary permissions.|
|32532|The project cannot be converted to this format. The project can only be converted to Access 2000 or newer format.|
|32533|Failed to create field '_1' on table '_2'|
|32534|Failed to delete predefined field '_1' on table '_2'|
|32535|Failed to rename the field '_1' on the SharePoint list '_2'|
|32536|The specified command (OutputTo) is not available now. Close objects open in Design View or Layout View and try again. |
|32537|Only 256 TempVars can be created. Be sure to remove any TempVars that are not needed.|
|32538|TempVars can only store data. They cannot store objects.|
|32539|TempVars can only store strings with up to 65,356 characters.|
|32540|The name of this TempVar is too long. TempVar names must be 256 characters or less. Use a shorter name.|
|32541|You must specify a name to set or remove a temporary variable.|
|32542|You must specify a value to set the temporary variable to.|
|32543|Do you want to connect to an existing SQL Server database?  Select No to create a new SQL Server database.|
|32544|You are trying to convert an encoded database. Decode the database, and then try again. To protect the converted database, use the Encrypt with Password command.|
|32545|You are trying to convert a password protected database. Remove the password, and then try again. To protect the converted database, use the Encrypt with Password command.|
|32546|You are trying to convert an encoded database that uses a password. Decode the database, remove the password, and then try again. To protect the converted database, use the Encrypt with Password command.|
|32547|You are trying to convert a database that is encrypted with a password. Remove the password, and then try again. To protect the converted database, use the Set Database Password command.|
|32548|There was an error displaying the mail message. Make sure that there are no open dialogs in Outlook and try again.|
|32549|'_' contains over 10,000 rows. Processing may take several moments. Would you like to continue?|
|32550|'_' is an empty lookup field. You will not be able to collect data for this field. To continue, remove the empty lookup field from the list of fields to be included in this e-mail message.|
|32551|There was an error displaying the task. Make sure that there are no open dialogs in Outlook and try again.|
|32552|This command is not available when the database is opened from the Web server. Would you like to save a local copy of the database?|
|32553|To sign and deploy a database, you must use a database created with Microsoft Office Access 2007 or later. Convert the database to the Access 2007 format and start the signing process again.|
|32554|Microsoft Access could not create a deployment package with the current database. Check the path to the file. |
|32555|Microsoft Access could not sign the current database. Ensure that the digital certificate used to sign the database is valid. |
|32556|Microsoft Access could not extract the database. Check the file name and the path to the file. |
|32557|Microsoft Access could not download the packaged database. Network issues or issues with the Web site may have prevented the download. Start the download again or browse to the site and ensure that it is operating. |
|32558|You requested an invalid file. The file may be corrupt. Contact the person who created the file and request a new copy. |
|32559|The template is in an unknown format and cannot be opened.|
|32560|The specified template does not exist.|
|32561|'_' is not a valid database template.|
|32562|The specified template cannot be created in the specified file format. Specify a different file format for the new database.|
|32563|You cannot use a template file to create a database directly on a SharePoint site.|
|32564|This does not appear to be a template file. The file may have become corrupt, or the package format may contain an error.|
|32565|The _0 '_1' could not be read from the template file. The object may be in an unrecognized format or contain invalid data.|
|32566|The table '_0' in the template could not be associated with a SharePoint list with ID _1.|
|32567|The _0 '_1' in the template could not be loaded from text because of a parsing error. _2|
|32568|The table _0 or one of its fields could not be successfully renamed.|
|32569|Failed to add the VBA reference _0|
|32570|A Design Master or Replica cannot be converted to the Access 2007 file format. |
|32571|The contents of the Navigation Pane may not be changed while it is locked.|
|32572|Access Projects do not support custom groups in the Navigation Pane. You may not import or export custom groups from an Access Project.|
|32573|Microsoft Access cannot perform the export operation.|
|32575|The acCmdExport command for RunCommand has been deprecated. Use the OutputTo action or one of the more specific RunCommands, such as acCmdExportExcel, acCmdExportRTF, and acCmdExportText.|
|32576|The acCmdImport command for RunCommand has been deprecated. Use one of the more specific RunCommands, such as acCmdImportAttachAccess, acCmdImportAttachExcel, and acCmdImportAttachText.|
|32577|The acCmdLinkTables command for RunCommand has been deprecated. Use one of the more specific RunCommands, such as acCmdImportAttachAccess, acCmdImportAttachExcel, and acCmdImportAttachText.|
|32578|Microsoft Access could not load the Access database engine. Run setup again and restart the application. |
|32579|You do not have permissions to write to this SharePoint site. Contact your site administrator for assistance.|
|32580|Cannot publish the file _ as the file is either checked out or locked for editing by another user.|
|32581|SQL Distributed Management Objects (SQL-DMO) must be on the machine running Microsoft Access.|
|32582|The specified file name could not be used to create a database. Specify a different database name.|
|32583|An error occurred while trying to create the database.|
|32584|No object with the given name and type exists in the current database.|
|32585|This feature is only available in an ADP. |
|32586|This database object cannot be saved as a report. |
|32587|The Source value you have specified is not valid. |
|32588|The table which you are trying to append to is currently open and must be closed before proceeding. Do you want to save the changes and close the table? |
|32589|Microsoft Access encountered an error trying to close the table. |
|32590|You cannot link to a database that is stored on an Internet location. You should first save the database to a location that uses a Universal Naming Convention (UNC), such as a server share, and then link to it. |
|32591|The file path you specified is not valid. Please specify a full file path. |
|32592|The file name extension of '_' is not valid for the format to which you are trying to export. This action is potentially unsafe. Do you want to continue? |
|32593|Overwite existing table or query '_'? |
|32594|The Show Column History command is not available while you are working with a SharePoint list offline. Reconnect to the list and try again. |
|32595|Microsoft Access cannot delete the SharePoint list, because you don't have the necessary permissions to delete the list or the list has been renamed or no longer exists.|
|32596|The Microsoft Access Outlook Add-in is disabled in Microsoft Outlook. To collect data by using e-mail messages in Microsoft Access, verify that this add-in is installed and enabled from within Microsoft Outlook.|
|32597|This database contains links to lists on the selected SharePoint site. Do you want to be able to add shortcuts to each list's View menu? This will enable other people to open forms and reports from the SharePoint Web page.|
|32598|The name '_' is already in use. Enter a different name. |
|32599|Only XML files can be imported from an Internet address (http://, ftp://). Enter a path that points to a location on your computer or on the network. |
|32600|The list _1 is already used by the _2 link table. Select a different list.|
|32601|You are about to change sites. Any changes you have made to link tables will be discarded. Do you want to continue without saving these changes?|
|32602|The SetProperty action or method failed because there was no form or report context. To establish the correct context for SetProperty, call OpenForm or OpenReport before calling SetProperty, or call SetProperty from an event on a form or report. |
|32603|SharePoint List data will be cached in this database. If this database is shared, users who do not have permissions to view this data will be able to see it.|
|32604|There was an error publishing your database. Verify your publish URL and permissions, and then try again.|
|32605|Your data source contains more than 255 fields (columns). Access will import only the first 255 fields.|
|32606|Some field types, such as attachment fields and calculated fields, are not supported in the MDB or ADP database. Do you want to continue?|
|32607|Access 2016 [introduced a new error number 32607][2] related to the Large Number feature of Access 2016. The (almost certainly) unintended side effect is that **all the Access error numbers above 32607 are one number higher in Access 2016 than they were in Access 2013 and earlier.** |
|32607 (*32608*)|Field types such as attachment fields and calculated fields, are not supported in the MDB or ADP database formats. If any unsupported field types exist in the source database tables, those fields will not be imported. Do you want to continue?|
|32608 (*32609*)|This database file cannot be converted because it contains linked tables that refer to one or more files that cannot be found. To continue, delete these linked tables or repair the links. |
|32609 (*32610*)|_1 cannot load customization '_2'. This customization name was already loaded.|
|32610 (*32611*)|Automatic configuration of the current version of Microsoft Access has failed. Your database might not function correctly. This can occur if you do not have the necessary permissions to install Microsoft Access on this computer. |
|32611 (*32612*)|The file '_' already exists. Do you want to update the existing file? |
|32612 (*32613*)|Microsoft Access cannot open this file. This file was converted to runtime mode by changing its file name extension to .accdr. This file will open only when Access is in runtime mode. To open the file, either double-click it in Windows Explorer, open it by using a shortcut, or use the /runtime command-line switch. To modify the design of this database, rename it with an .accdb file name extension, and then open it in Access.|
|32613 (*32614*)|This feature requires the Microsoft Access 2010 program to be installed on your computer. The application you are running is powered by the Microsoft Access 2010 Runtime. For information about the differences between the Microsoft Access program and the Microsoft Access Runtime, in the status bar, click Powered by Microsoft Access.|
|32614 (*32615*)|Microsoft Access cannot read the data in _1. The minimum required version to read the data is _2.|
|32615 (*32616*)|Microsoft Access cannot update the data in _1. The minimum required version to update the data is _2.|
|32616 (*32617*)|Microsoft Access cannot change the design of _1. The minimum required version to change the design is _2.|
|32617 (*32618*)|The database you are trying to open requires a newer version of Microsoft Access.|
|32618 (*32619*)|Click here to learn more about this error.|
|32620 (*32621*)|An Access 2007 format (.accdb) database must be open before you use this command.|
|32621 (*32622*)|The file '_1' could not be created. Verify that you have the correct permissions and that the file is not locked or read-only, and then try again.|
|32622 (*32623*)|An error occurred when trying to save the database as a template because the object _1:[_2] could not be exported.|
|32623 (*32624*)|An error occurred when trying to save the database as a template because the preview image could not be saved.|
|32624 (*32625*)|An error occurred when trying to save the database as a template because the icon could not be saved.|
|32625 (*32626*)|An error occurred when trying to save the database as a template because Relationships failed to export.|
|32626 (*32627*)|An error occurred when trying to save the database as a template because the Navigation Pane failed to export.|
|32627 (*32628*)|The value for Name is required.|
|32628 (*32629*)|The value for Category is required.|
|32629 (*32630*)|The label for the lookup columnn is required.|
|32630 (*32631*)|The database must be in online mode to complete this operation. Do you want to switch to online mode now? |
|32631 (*32632*)|The Web application '_1/_2' already exists. Select a different site or Web application name and try again.|
|32632 (*32633*)|Access to '_' was denied. Please contact the server administrator to request permissions to publish Access Services databases.|
|32633 (*32634*)|While attempting to publish your database, the server reported the following error: _|
|32634 (*32635*)|An unknown error occurred while communicating with the server. Please verify that you are connected to the network, the full URL to the site you are creating is correct, and that the server is available.|
|32635 (*32636*)|Microsoft Access could not open the Web application from _1. Please verify that the server is available.|
|32636 (*32637*)|The server did not respond. Please verify that you are connected to the network and that the server is available.|
|32637 (*32638*)|The specified server does not support publishing Access Services applications.|
|32638 (*32639*)|You must specify a name to set or remove a local variable.|
|32639 (*32640*)|You must specify a value to set the local variable to.|
|32640 (*32641*)|The database object '_' must be closed in order to make this change. Please close the object and try again. |
|32641 (*32642*)|You cannot open the database object '_' in design mode with the current settings.|
|32642 (*32643*)|Save As action is not available for Web objects.|
|32643 (*32644*)|You must specify an object for the BrowseTo to open.|
|32644 (*32645*)|Some of the lists selected for import will only be available when the database application is opened in Microsoft Access. Do you want to continue?|
|32645 (*32646*)|Object name may not exceed _ characters. Please specify a shorter name.|
|32646 (*32647*)|There are no valid objects to package into a template. No template file was created.|
|32647 (*32648*)|You cannot run Web Expressions in a non-trusted database. If you trust the source of this database, you can enable it by using the Message bar.|
|32648 (*32649*)|This operation is only allowed on Custom Groups.|
|32649 (*32650*)|This operation is only allowed on Custom Categories.|
|32650 (*32651*)|This operation is only allowed on Object Shortcuts in Custom Groups.|
|32651 (*32652*)|This group does not allow this type of customization.|
|32652 (*32653*)|This category does not allow this kind of customization.|
|32653 (*32654*)|Invalid position.|
|32654 (*32655*)|The position of items in the Navigation Pane cannot be changed.|
|32655 (*32656*)|This object cannot be saved as a report because it contains a navigation control. |
|32656 (*32657*)|Microsoft Access has detected that this database contains both Microsoft Access 2007 compatible links and Access 2010 compatible links. Would you like to convert all links to use improved Access 2010 caching? This will mean the links cannot be opened in Microsoft Access 2007. |
|32657 (*32658*)|The compact and repair operation has been cancelled. You might not have adequate permissions to the folder that the database is located in. You need full permissions to the directory the database is located to compact and repair. Contact your system administrator for more information. |
|32658 (*32659*)|An error occurred while initializing the Access Services database.|
|32659 (*32660*)|Inserting a datatype template is only supported in an open table with a current selection.|
|32660 (*32661*)|Microsoft Access does not support this operation for Data Access Pages. To perform this operation, use Microsoft Office Access 2007 or earlier. |
|32661 (*32662*)|The ReplicationConflictFunction property is not set properly to resolve conflicts. Contact the author of this database application or go to http://support.microsoft.com/kb/158930 to see how to create such a function. |
|32662 (*32663*)|The form cannot be saved as a report because it contains a Web browser control.|
|32663 (*32664*)|Please provide a data source for the report.|
|32664 (*32665*)|There was an error opening the file '_'.|
|32665 (*32666*)|The object "_1" (_2) could not be found in the current Web application.|
|32666 (*32667*)|Invalid Table Name.|
|32667 (*32668*)|Access was unable to automatically restore your backup database. The backup database can be accessed here: |
|32668 (*32669*)|All open objects must be closed before instantiating this application part. Do you want Microsoft Access to close all open objects?|
|32669 (*32670*)|Microsoft Access encountered an error while checking the object '_' for Web compatibility. The conversion failed. |
|32670 (*32671*)|Access 2010 cannot contact the server. Check your network connection or contact the server administrator.|
|32671 (*32672*)|Network connectivity lost while updating links. Some tables may not have changed modes. You will be prompted to change modes again next time your database is closed and opened.|
|32672 (*32673*)|Microsoft Access could not create the linked table '_1' with the specified connection information. Please rename the link and check that the data source is available.|
|32673 (*32674*)|Microsoft Access does not have any Web service connection definitions.|
|32674 (*32675*)|The resource name '_' is already in use.|
|32675 (*32676*)|Microsoft Access could not add '_1' to your Image Gallery. _1 is not an image file, or is saved in an unrecognized image format. |
|32676 (*32677*)|Microsoft Access was unable to add the requested resource to the resources collection. Ensure that the file exists and is in a valid format.|
|32677 (*32678*)|A resource with the specified name and type already exists in the resource collection. Only one resource can exist for each combination of name and type.|
|32678 (*32679*)|There was an error adding an image from the file '_1'. Only graphic files supported by Microsoft Access may be added.|
|32679 (*32680*)|The file '_1' could not be opened. Please verify that the file exists and try again.|
|32680 (*32681*)|The resource name '_1' is invalid. Resource names must be between 1 and 64 characters long, and may not contain most punctuation.|
|32681 (*32682*)|You cannot delete themes directly. Themes are automatically deleted when no objects are using them.|
|32682 (*32683*)|The current database does not support resource collections. Resource collections are not supported on Access projects (ADP) or Access databases in the MDB format.|


  [1]: http://www.fmsinc.com/MicrosoftAccess/Errors/ErrorNumber_Description.html
  [2]: https://social.msdn.microsoft.com/Forums/en-US/d694e779-1b5b-4c50-b6e0-b4f488a46425/error-number-changed-between-access-2013-and-access-2016?forum=accessdev#622d6b95-173c-40a7-841b-206016f2c0f9 "Error Number Changed Between Access 2013 and Access 2016"

## Form.Error
Here is typical error handler for a subform as a table:

    Public Const cErrCodeValueRequierd = 3162
    Public Const cErrCodeDuplicateKey = 3022

    Private Sub Form_Error(DataErr As Integer, Response As Integer)
        Select Case DataErr
            Case cErrCodeDuplicateKey
                MsgBox "Duplicate records are not allowed!", vbExclamation, "Key violation"
                Response = acDataErrContinue
            Case cErrCodeValueRequired
                MsgBox "Field ""fieldName"" requires data!""
                Response = acDataErrContinue
            Case Else
                MsgBox "Other error. Error code: " & DataErr    
        End Select
    End Sub


## Trap All Errors
Basic error handling should be added to all procedures in production code, since otherwise an unexpected error will cause Access to crash or invite the end-user to enter debug mode. 

A typical pattern for a basic error handler that traps all errors is:  

<!-- language: lang-vb -->
    Sub Name()
    On Error GoTo errHandler
    
    '[some code...]
    
    exitHandler:
        'Clean up any objects
        Exit Sub

    errHandler:
        Select Case Err.Number
            'Any expected error cases go here
        Case Else
            MsgBox Err.Number & Err.Description
        End Select
        Resume exitHandler   'Make sure objects get cleaned up when code threw an error
    End Sub

For a more detailed discussion of error handling in Access VBA, see:

 - [Error Handling in VBA](http://allenbrowne.com/ser-23a.html), by Allen Browne
 - [Error Handling and Debugging Tips and Techniques for Microsoft Access, VBA, and Visual Basic 6 (VB6)](http://www.fmsinc.com/tpapers/vbacode/Debug.asp), by Luke Chung

## Catching and handling errors in user code
On error move to labelled code and see if there is a specific error that needs to be handled.

    Public Const cErrCodeNotNumber = 2262   ' This value must be a number.
    Public Const cErrCodeNumericOverflow = 2263   ' The number is too large.

    Private Sub MySub()
        Dim objConn As ADODB.Connection
        Dim objCmd As ADODB.Command
        Dim objRS As ADODB.Recordset
        'etc.

        On Error Goto ErrHandler
        [...My code goes here...]

    ExitSub:
        'Cleanup
        If objConn.State <> adStateOpen Then objConn.Close 'Closing connection to database, if it is still open
        If Not objRS Is Nothing Then objRS.Close
        objConn = Nothing
        ObjRS = Nothing
        'Do any other cleaning.
        Exit Sub

    ErrHandler:
        Select Case Err.Number
            Case cErrCodeNotNumber
                MsgBox "The value found is not a number. Execution stopped.", vbCritical
            Case cErrCodeNumericOverflow
                MsgBox "The value found is too big. [instructions how to resolve this]", vbCritical
            Case Else
                MsgBox "Error " & Err.Number & ". " & vbCrLf & Err.Source & "--->" & Err.Description, "Unrecoverable error", vbCritical
        End Select
        
        Goto ExitSub
    End Sub

