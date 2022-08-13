---
title: "Differences between Batch (Windows) and Terminal (Linux)"
slug: "differences-between-batch-windows-and-terminal-linux"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

Batch and bash are quite different. Batch flags are indicated with a `/`, while bash flags use a `-`. Capitalization matters in bash, but (almost) not at all in batch. Batch variable names can contain spaces, bash variable names can not.

Ultimately, both are ways of manipulating and interacting with the command line. Not surprisingly, there is a reasonably-sized amount of overlap between the capabilities of the two systems.

* `bitsadmin` is deprecated in favor of the PowerShell cmdlet BITS but still works as of Windows 10 version 1607
* `certutil` separates pairs of hexadecimal digits with a space, so `md5sum` will return an example value of `d41d8cd98f00b204e9800998ecf8427e`, while `certutil` displays the same value as `d4 1d 8c d9 8f 00 b2 04 e9 80 09 98 ec f8 42 7e`
* To `cd` to another drive (for example, from C: to D:) the `/d` flag must be used
* `del` can not delete folders, use `rm` instead
* `grep` is so much more powerful than `find` and `findstr`, it's almost not fair to compare them; `find` has no regex capabilities and `findstr` has extremely limited regex capabilities (`[a-z]{2}` is not valid syntax, but `[a-z][a-z]` is)
* `for` loops on the Windows command prompt can only use single-character variable names; this is the only time batch variable names are case-sensitive
* `for` loops on the command prompt also use the variable form `%A` instead of `%A%` - `for`loops in batch scripts use the variable form `%%A`
* `md` automatically creates any necessary parent directories, while `mkdir` needs the `-p` flag to do so
* `rem` may not be used as an inline comment character unless it is preceded by a `&`
* `::` may not be used as an inline comment at all, and should also not be used inside of a code block (set of parentheses)



- Note that some Windows command like `ping` still uses `-` as flags

## Batch Commands and Their Bash Equivalents
|Batch|Bash|Description|
|------|------|------|
|`command /?`|`man command`|Shows the help for _command_|
|`bitsadmin`|`wget` or `curl`|Downloads a remote file|
|`certutil -hashfile file_name MD5`|`md5sum file_name`|Gets the MD5 checksum of _file_name_|
|`cd`|`pwd`|Displays the current directory|
|`cd directory`|`cd directory`|Changes the current directory to the specified one|
|`cls`|`clear`|Clears the screen|
|`copy`|`cp`|Copies a file or files from a source path to a target path|
|`date`|`date`|Displays the date or sets it based on user input|
|`del`|`rm`|Deletes a file or files|
|`dir`|`ls`|displays a list of files and directories in the current directory|
|`echo`|`echo`|Displays text on the screen|
|`exit`|`return`|Exits a script or subroutine|
|`exit`|`logout`|Closes the command prompt or terminal|
|`fc`|`diff`|Compares the contents of two files|
|`find "string" file_name`|`grep "string" file_name`|Searches _file_name_ for _string_|
|`findstr "string" file_name`|`grep "string" file_name`|Searches _file_name_ for _string_|
|`for /F %A in (fileset*) do something`|`for item in fileset*; do; something; done`|Do something for every file in a set of files|
|`for /F %A in ('command') do something`|`` `command` ``|Returns the output of a command|
|`for /L %A in (first,increment,last) do something`|``for item in `seq first increment last`; do; something; done``|Starts at _first_ and counts by _increment_ until it reaches _last_|
|`forfiles`|`find`|Searches for files that match a certain criteria|
|`if "%variable%"=="value" (`|`if [ "variable"="value" ]; then`|Compares two values|
|`ipconfig`|`ifconfig`|Displays IP information|
|`md`|`mkdir`|Creates new folders|
|`mklink`|`ln -s`| Creates a symbolic link|
|`more`|`more`|Displays one screen of output at a time|
|`move`|`mv`|Moves a file or files from a source path to a target path|
|`pause`|`read -p "Press any key to continue"`|Pauses script execution until the user presses a button|
|`popd`|`popd`|Removes the top entry from the directory stack and goes to the new top directory|
|`pushd`|`pushd`|Adds the current directory to the directory stack and goes to the new top directory|
|`ren`|`mv`|Renames files|
|`rem` or `::`|`#`|Comments a line of code|
|`rd`|`rmdir`|Removes empty directories|
|`rd /s`|`rm -rf`|Removes directories regardlesss of whether or not they were empty|
|`set variable=value`|`variable=value`|Sets the value of _variable_ to _value_|
|`set /a variable=equation`|`variable=$((equation))`|Performs math (batch can only use 32-bit integers)|
|`set /p variable=promptstring`|`read -p "promptstring" variable`|Gets user input and stores it in _variable_|
|`shift`|`shift`|Shifts arguments by 1 (or n if provided)|
|`sort`|`sort`|Sorts output alphabetically by line|
|`tasklist`|`ps`|Shows a list of running processes|
|`taskkill /PID processid`|`kill processid`|Kills the process with PID _processid_|
|`time /t`|`date`|Displays the current time|
|`type`|`cat`|Displays the contents of a file|
|`where`|`which`|Searches the current directory and the PATH for a file or command|
|`whoami`|`id`|Displays the name and group of the current user|

## Batch Variables and Their Bash Equivalent
|Batch|Bash|Description|
|------|------|------|
|`%variable%`|`$variable`|A regular variable|
|`!variable!`|`$variable`|A variable inside of a code block when `setlocal enabledelayedexpansion` is on|
|`%errorlevel%` or `ERRORLEVEL`|`$?`|Returns the status of the previous command: 0 if successful, 1 (or something else) if not|
|`%1`, `%2`, `%3`, etc.|`$1`, `$2`, `$3`, etc.|The parameters given to a script|
|`%*`|`$*`|All parameters given to a script|

