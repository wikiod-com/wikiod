---
title: "Navigating directories"
slug: "navigating-directories"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Absolute vs relative directories
To change to an absolutely specified directory, use the entire name, starting with a backslash `\`, thus:

    cd /home/username/project/abc

If you want to change to a directory near your current on, you can specify a relative location. For example, if you are already in `/home/username/project`, you can enter the subdirectory `abc` thus:

    cd abc

If you want to go to the directory above the current directory, you can use the alias `..`. For example, if you were in `/home/username/project/abc` and wanted to go to `/home/username/project`, then you would do the following:

    cd ..

This may also be called going "up" a directory.

## Change to the last directory
For the current shell, this takes you to the previous directory that you were in, no matter where it was.

    cd -

Doing it multiple times effectively "toggles" you being in the current directory or the previous one. 

## Change to the home directory
The default directory is the home directory (`$HOME`, typically `/home/username`), so `cd` without any directory takes you there

    cd

Or you could be more explicit:

    cd $HOME 

A shortcut for the home directory is `~`, so that could be used as well.

    cd ~

## Change to the Directory of the Script
In general, there are two types of Bash **scripts**:

1. System tools which operate from the current working directory
2. Project tools which modify files relative to their own place in the files system

For the second type of scripts, it is useful to change to the directory where the script is stored. This can be done with the following command:

    cd "$(dirname "$(readlink -f "$0")")"

This command runs 3 commands:

1. `readlink -f "$0"` determines the path to the current script (`$0`)
2. `dirname` converts the path to script to the path to its directory
3. `cd` changes the current work directory to the directory it receives from `dirname`

