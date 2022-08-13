---
title: "Using xcopy command"
slug: "using-xcopy-command"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Xcopy, copies files and directories, including subdirectories.

## Parameters
| Parameter   | Details   |
|---|---|
| /h | Copies files with hidden and system file attributes. By default, **xcopy** does not copy hidden or system files.| 
| /r | Copies read-only files.|
|/s | Copies directories and subdirectories, unless they are empty. If you omit /s, xcopy works within a single directory.  |
| /y  | Suppresses prompting to confirm that you want to overwrite an existing destination file.  |
| /D  | Orders CMD to only copy files that are newer than their destination opposite.  |



## Copying multiple files including tree structure
If you want to xcopy files with specific type to a new folder keeping the current folder structure you need only to do this

xcopy [SourcePath]*.mp3 [DestinationPath]  /sy




