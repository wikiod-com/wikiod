---
title: "Changing Directories and Listing their Contents"
slug: "changing-directories-and-listing-their-contents"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Syntax
 - echo %cd% - displays the current path of the directory
 - cd "C:\path\to\some\directory" -changes the path of the directory
 - cd "%variable_containing_directory_path%" - also changes the path of the directory
 - cd /d E: - change to E: drive from a different drive
 - cd/ - changes directory back to current drive

 - echo `%__CD__%` - displays the current path of the directory with trailing backslash (undocumented)
- echo %=C:% - The current directory of the C: drive (undocumented)
- echo %=D:% - The current directory of the D: drive if drive D: has been accessed in the current CMD session (undocumented)


Why is it important and what are they uses and advantages:

 - to open file or application in a directory using batch  
 - to create andwrite and read files in a directory using batch
 - to know and list out all folders
 - to know where your batch file is running


## To change the current directory (without changing drives)
Format:

    cd "<path>"

Example:

     cd "C:\Program Files (x86)\Microsoft Office"

`cd` is an abbreviation for `chdir` and the two commands behave in the exact same way. For the sake of consistency, `cd` will be used throughout this topic.

---
To navigate to the directory one level above the current directory, specify the system directory `..`.

    cd ..

---
To navigate to a directory that is inside of the current directory, simply `cd` to the folder name without typing the full path (wrapping the directory name in quotes if it contains spaces).


For example, to enter `C:\Program Files (x86)\Microsoft Office` while in the `C:\Program Files (x86)` directory, the following syntax may be used:

    cd "Microsoft Office"

or 

    cd "C:\Program Files (x86)\Microsoft Office"

## Navigating to a directory on a different drive
`cd` by itself will not allow a user to move between drives. To move to a different drive, the `/d` option must be specified.

e.g. Moving from `C:\Users\jdoe\Desktop` to `D:\Office Work`

    cd /d "D:\Office Work"

## Changing drive without CD /D
    Pushd "D:\Foo"
    Dir
    Popd
`Pushd` will change the directory to the directory following (in this case D:\Foo. `Popd` returns back to the original directory.

## To display the current directory
   Format and Usage:

     echo %cd%

`%cd%` is a system variable that contains the current directory path

## How to show all folders and in files in a directory
Usage to list all folders and files in the current directory: `dir`

A target directory can also be specified: `dir C:\TargetPath`

When specifying a path with spaces, it must be surrounded by quotes: `dir "C:\Path With Spaces"`






## To change the current directory to the root of the current drive
Format:

    cd/
`cd/` is set to change the current directory back to the root of the current drive

