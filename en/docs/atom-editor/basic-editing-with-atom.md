---
title: "Basic Editing With Atom"
slug: "basic-editing-with-atom"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Note, the icons used at the end of the **Opening Files and Directories** example are not part of Atom's standard styling, but are the result of the [file-icons](https://atom.io/packages/file-icons) styling package.

## Interactive File Tree
In order to keep track of your projects' file structure, Atom, like many text editors and IDEs, uses a file tree model. These trees show the locations and names of your files and directory. To toggle the tree between visible and hidden, the keys <kbd>Ctrl</kbd>+<kbd>\\</kbd> may be used (<kbd>⌘</kbd>+<kbd>\\</kbd> for Mac OS). This tree also includes many operations for both files and directories as shown below:

[![Atom file tree options context menu][1]][1]

| Operation | Description |
| --------- | ----------- |
|Split Up|Splits the editor into two panes with the selected file on the top|
|Split Down|Splits the editor into two panes with the selected file on the bottom|
|Split Left|Splits the editor into two panes with the selected file on the left|
|Split Right|Splits the editor into two panes with the selected file on the |
|Search in Directory|Opens the find and replace tool to search the selected file or directory|
|New File|Creates a new file in the scope of the directory where the click occurred|
|New Folder|Creates a new folder in the scope of the directory where the click occurred|
|Rename|Changes the name of the file or directory|
|Duplicate|Creates an exact copy of the file or directory|
|Delete|Removes the file or directory|
|Copy|Copies the file or directory to the clipboard|
|Paste|Pastes a copied file or directory from the clipboard|
|Add Project Folder|Allows you to select another directory to be included at the root of the tree|
|Copy Full Path|Copies the full system path to the selected file or directory onto the clipboard|
|Copy Project Path|Copies the path of the selected item relative to the project root to the clipboard|
|Open In New Window|Opens the file or directory as the root in a new window|
|Show in Finder|Opens the default file explorer of the OS to the selected file or directory|

Hidden files will (unless set otherwise in Atom's settings) show up with shaded filenames. A common example is GitHub's repository configuration data in the `.git` directory.

  [1]: https://i.stack.imgur.com/TytzW.png

## Opening Files and Directories
Along with other more advanced text editors, Atom allows developers to open a single file or a directory.

# Opening Files

To open files with Atom, either use **File > Open File...** in the menu as show below:

[![Open File option in the Atom menu][1]][1]

or use the faster keyboard shortcut: <kbd>Ctrl</kbd>+<kbd>O</kbd> (For Mac OS: <kbd>⌘</kbd>+<kbd>O</kbd>). This will open a file explorer (Finder for Mac OS) from which you can select a file to open, or to select multiple files,  use the <kbd>Ctrl</kbd> (For Mac <kbd>⌘</kbd>) key while clicking on other files or hold the <kbd>Shift</kbd> key while selecting other files to select a range. When you have selected the files you wish to open, press the `Open` button on the file explorer. Atom, as a text editor, only elects to handle files under 2 megabytes.

# Opening Directories

Especially for projects, Atom's directory opening feature can be quite useful. To do so, you may either use the option in Atom's file menu:

[![Open Folder option in the Atom menu][2]][2]

or use the keyboard shortcut <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>O</kbd> (For Mac OS: <kbd>⌘</kbd>+<kbd>Shift</kbd>+<kbd>O</kbd>). Opening directories will allow you to access other directories and files below the root directory:

[![Sample file tree of opened directory][3]][3]

  [1]: https://i.stack.imgur.com/jsgBs.png
  [2]: https://i.stack.imgur.com/TMARj.png
  [3]: https://i.stack.imgur.com/v1lNw.png

## Find and Replace
The find and replace feature in Atom works in two ways, one operates locally only on the file you are in, and the other on a set of files or directories.

To open the find and replace tool for a single file, press <kbd>Ctrl</kbd>+<kbd>F</kbd> (For Mac OS use <kbd>⌘</kbd>+<kbd>F</kbd>). Enter in the first blank the string you are searching for. Press the <kbd>Enter</kbd> key to find all instances of the string. To the right of the **Find** button are the regex, case sensitive, in selection, and whole word option buttons. The **Use Regex** button allows you to search for regex characters such as `\n`, `\t`, `\r` and regex statements `/^[a-z0-9_-]{3,16}$/`. The **Case Sensitive** button - when active - will only find strings with the same case (capitalizations). The **Only in Selection** option will only find instances of the string in highlighted sections of the file. The **Whole Word** option will only find delimited instances, not when the string is part of a larger portion. Clicking the **Replace** button will take the first instance found with the **Find** method and replace them with the contents of the replace field (even if it is empty). Clicking the **Replace All** button will replace all instances found with the **Find** method and replace them all at once with the contents of the replace field.

