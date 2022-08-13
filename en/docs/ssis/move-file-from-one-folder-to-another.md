---
title: "Move file from one folder to another"
slug: "move-file-from-one-folder-to-another"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## File System Tasks in SSIS
From the Control Flow tab in your SSIS package, look in the SSIS Toolbox in the common section for the File System Task, drag this onto your design surface where you want the file move to happen in your package.

[![enter image description here][1]][1]

Once you've placed the task, double click to open it.

[![enter image description here][3]][3]

From here you'll want to first give the task a name. This helps later when you're reading logs looking for errors, you can recognize your file move task by name in those logs. In my case I named the task, Move To Complete.  

Next you have two options to define your source and destination files.  You can either define two file connections in your package.  And then choose False for IsDestinationPathVariable and IsSourcePathVariable.  You would then click the cell to the right of DestinationConnection and SourceConnection and choose the file connections from your package.  I find more often I'm moving tasks in a loop, so I have a need to move more than one file per package execution.

If you want to be able to handle multiple files, change the IsDestinationPathVariable and IsSourcePathVariable to true. Then your File System Task Editor will change to look like the below image.

[![enter image description here][2]][2]

You'll need two variables defined in your package to hold the **full** destination file path (directory structure and filename) and **full** source file path.  In my case I'm reading the variable XPR_ProcessingFileName for the source file and XPR_CompleteFileName for the destination file.

Finally, notice the Operation is "Rename file" rather than "Move file" since In my system, we append datestamps onto the end of the filenames to mark when they are processed by ETL.  You could also change this option to Move file if you'd like, but renaming a file from one filepath to another is a move. 

  [1]: http://i.stack.imgur.com/6bVzI.png
  [2]: http://i.stack.imgur.com/P9ClS.png
  [3]: http://i.stack.imgur.com/gMJqu.png

