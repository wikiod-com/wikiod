---
title: "Manage bookmarks within Emacs"
slug: "manage-bookmarks-within-emacs"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## How to bookmark frequently used files
Use the following commands to create bookmarks and access bookmarks from within Emacs.  

Let us say that you are editing a file called `foobar.org` and suppose that you visit this file frequently to edit / view contents.  

It would be convenient to access this file with couple of key strokes rather than navigate through the file structure (Dired) and visit the file.  

**Steps:**  

 1. Open `foobar.org` for once by navigating to the file (*visit the file* in Emacs lingo)
 2. While the file is open type <kbd>C-x</kbd><kbd>r</kbd><kbd>m</kbd> this will prompt you to provide the bookmark name for the file. Let us say `foobar` in this case.
 3. Close the file (<kbd>C-x</kbd><kbd>k</kbd> - kill buffer) - save if required
 4. Now to visit the file, just type <kbd>C-x</kbd><kbd>r</kbd><kbd>l</kbd> - this will populate a list which will contain `foobar`.
 5. Select `foobar` and hit <kbd>Enter</kbd> key
 6. Use <kbd>M-x</kbd>`bookmark-delete` to delete any unnecessary bookmark of a file.  

_Note:_ Deleting a bookmark is analogous to deleting a shortcut in your Windows desktop.  
The main file will be safe in its location and only the listing of the file from the bookmark menu will be removed.



