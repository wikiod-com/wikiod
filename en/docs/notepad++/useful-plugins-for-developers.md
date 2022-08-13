---
title: "Useful plugins for developers"
slug: "useful-plugins-for-developers"
draft: false
images: []
weight: 9907
type: docs
toc: true
---

## NppExec
NppExec [[sourceforge](https://sourceforge.net/projects/npp-plugins/files/NppExec/)] allows you to execute commands and scripts from a console window in Notepad++. It can be found in the menu bar at `Plugins -> NppExec` or just by simply hitting the <kbd>F6</kbd> key (the shortcut <kbd>Ctrl</kbd>+<kbd>F6</kbd> will run the latest command) .

Example: the following will
 - Set the console to `output_var: on`, meaning we can use the output of the console
 - run a SQL query from the file `C:\scripts\query.sql`
 - take the output from the console and put it into the active file at the cursor
 - close the console
 - clear the console


    NPE_CONSOLE v+
    sqlcmd -S 111.111.1.1 -U UserName -P "password" -i C:\scripts\query.sql
    sel_settext $(OUTPUT)
    NPP_CONSOLE 0
    cls

NppExec also allows you to save your scripts.  After saving them, you can go to `Plugins -> NppExec -> Advanced Options` and run it anytime Notepad++ starts, closes, or even add that script to the Plugin commands using Macros.  For example, by saving the above example as a "Run Query", I could use the bottom left fields in the Advanced Options to add it to the menu.

[![Add to Menu][1]][1]

The script will be available as a **macro** after a restart of Notepad++ as long as the "Place to the Macros submenu" box is checked.

Finally, a **shortcut** can be assigned to the macro/command by using `Settings -> Shortcut mapper -> Plugin commands`.

  [1]: http://i.stack.imgur.com/iWrrF.png

## JSON Viewer
JSON Viewer [SourceForge][1] is a plugin for JSON visualization and formatting. It is useful for indenting /formatting JSON documents and can be used to browse complex JSON file using a treeview tool.

The following image shows the commands offered by the plugin:

[![enter image description here][2]][2]

Starting from an unformatted JSON fragment (Example from http://www.json.org ):

> {"glossary": {"title": "example glossary","GlossDiv": {"title":
> "S","GlossList": {"GlossEntry": {"ID": "SGML","SortAs":
> "SGML","GlossTerm": "Standard Generalized Markup Language","Acronym":
> "SGML","Abbrev": "ISO 8879:1986","GlossDef": {"para": "A meta-markup
> language, used to create markup languages such as
> DocBook.","GlossSeeAlso": ["GML", "XML"]},"GlossSee": "markup"}}}}}

To format and indent the code:

- select all the json fragment
- click "Plugins"/"JSON Viewer"/"Format JSON" or use the shortcut <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>Shift</kbd> + <kbd>M</kbd>

This is the resulting formatted code:

[![enter image description here][3]][3]

The plugin can also show a treeview browsable version of the JSON fragment:

- select all the json fragment
- click "Plugins"/"JSON Viewer"/"Show JSON Viewer" or use the shortcut <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>Shift</kbd> + <kbd>J</kbd>

The following screenshot shows how the plugin renders the JSON structure:

[![enter image description here][4]][4]


  [1]: https://sourceforge.net/projects/nppjsonviewer/
  [2]: http://i.stack.imgur.com/Vji5D.png
  [3]: http://i.stack.imgur.com/V0dvY.png
  [4]: http://i.stack.imgur.com/pMjkC.png

## SourceCookifier
[SourceCookifier](https://sourceforge.net/projects/sourcecookifier/) parses the current source code for such components as class, function, and variable names and displays them in a tree view at a side panel. Navigation among these members is possible by double-clicking on the component name. The plugin supports a number of languages and customizations are possible for undetermined or user-defined languages. This is useful when working with large codes.

[![Example SourceCookifier][6]][6]


By default, **SourceCookifier** session mode is set to `Single file mode`. Other available modes are `N++ session mode` (all the files open in Notepad++) or `Cookie session mode`.

[![Cookie Session mode][1]][1]

To use `Cookie session mode`:    
 1. Drag & Drop your folders with source code to Source Cookifier window 
    
 2. Select the type of files you want to parse
 
[![Select file type][2]][2]


**Notes:** 

 - You can save and load the cookie sessions; moreover, Source Cookifier display the history of the latest sessions
 - Opening the file of a saved session in Notepad++ will automatically switch SourceCookifier into cookie session mode and load this session
 - Maintaining a keyboard modifier (<kbd>CTRL</kbd>, <kbd>SHIFT</kbd>, or <kbd>ALT</kbd> - they are
   all similar) while drag-dropping the folder will fasten the parsing by only adding INCLUDES and not tags

[![Drad&Drop with keyboard modifier][5]][5]


---

Another useful feature is the ability to **jump to the definition** of a symbol (e.g. function)

 1. Select the right "session mode": use *cookie mode* if the definition of the symbol is in another file not opened in Notepadd++, or *single file mode* if the definition is in the same file 
 2. In Notepad++, put the cursor in the function/type you want to get definition of, and press <kbd>CTRL</kbd>+<kbd>SHIFT</kbd>+<kbd>ENTER</kbd> to jump to the definition. If there are several definitions, you can choose the file you want to open

[![Select file][3]][3]
    
 3. You can press <kbd>ALT</kbd>+<kbd>LEFT</kbd> to come back to the previous location (and <kbd>ALT</kbd>+<kbd>RIGHT</kbd> to go back to the definition)

[![Navigate Backward][4]][4]

 


  [1]: http://i.stack.imgur.com/GLV2J.png
  [2]: http://i.stack.imgur.com/Tnpsj.png
  [3]: http://i.stack.imgur.com/aWfp9.png
  [4]: http://i.stack.imgur.com/O67Mu.png
  [5]: http://i.stack.imgur.com/7JnR1.png
  [6]: http://i.stack.imgur.com/uJL89.jpg

## TextFX
TextFX [[SourceForge][1]] is plugin for advanced character conversions (escaping characters etc.) and code formatting (HTML or C++ code).


  [1]: https://sourceforge.net/projects/npp-plugins/files/TextFX/

