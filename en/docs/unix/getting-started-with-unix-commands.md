---
title: "Getting Started with Unix Commands"
slug: "getting-started-with-unix-commands"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

This topic will provide a comprehensive coverage of basic Unix commands.

## A non exhaustive list of Unix commands
    $man <command>
Displays the on-line manual pages for the command

    $clear
Clears the terminal screen

    $pwd
Returns the working directory name

    $echo <string>
Writes the string to the standard output

    $printf <string>
Format and print the string
Example: print $PATH
    $printf “%s\n” $PATH

    $uptime
Show how long system has been running

    $which <program>
Locate a program file in the user’s path

    $whereis <program>
Checks the standard binary directories for the specified programs, printing out the
<br><br>

**FILES**

    $cd [directory] 
Change directory
Commonly used directory symbols:
 - .  : Current directory
 - .. : Parent directory
 - ~ : Home directory
 - / : Root directory


    $ls
 - $ls -a : Show hidden files
 - $ls -l : Show long list
 - $ls -1 : Show just the filename per line
 - $ls -h : Human readable format

    $file <file>
Determine file type (e.g. gzip)

**READING FILES**

    $more
Display content of a file one screen at a time
<br>spacebar : Scroll to next screen; b=previous screen
<br>enter : Scroll one line
<br>h : Help for more
<br>q : Quit help

    $less <file>
Less is a program similar to more, but which allows backward movement in the file as well as forward movement

    $cat <file>
Reads files sequentially, writing them to the standard output

    $head [-number] <file>
Display first lines of a file

    $tail [-number] <file>
Displays the contents of file or, by default, its standard input, to the standard output
 - $tail -f <file> : View changes in file in real-time


    $touch <file>
Sets the modification and access times of files.  If any file does not exist, it is created with default permissions

    $tee <file>
Copies standard input to standard output. Press ctrl-d to stop adding content
- $tee -a : Append the output to the files rather than overwriting them


    $mkdir <directory>
Create a directory

    $wc
Display the number of lines, words, and bytes contained in each input file, or standard input
 - wc -l : Count lines
 - wc -w : Count words
 - wc -m : Count characters


    $diff <file1> <file2>
Compare two files line by line. Will print only the different lines.

    $locate <file>
Locate files on disk
 - $locate -q : suppress errors


    $find <path> <expression> <action>
Search for files by name or content
 - $find -name : Find by filename
 - $find -size <+/-n>
Example: Find files in current directory that are larger than 10k
    $find . -size +10


    $rm <file or directory>
Delete \<file or directory>
 - rm -f : Skip confirmation
 - rm -i : Approve each deletion
 - rm -r : Recursive

Example: Delete the <directory> and it’s content
    $rm -r <directory>


    $mv <source_file> <target_file>
Renames a file

    $mv <source_file> <target_directory>
Moves a file
 - $mv -i : Don’t override exiting files
 - $mv -r : Recursive

Example: move directory up in hierarchy
    $mv <directory> ..


    $cp
Copy a file/directory within the same machine (Use scp command to copy to a remote machine)
 - $cp -i : Don’t override exiting files
 - $cp -r :recursive

Example: Copy and rename a file

    $cp <file_name> <new_file_name>

Example: Copy to directory

    $cp <file_name> <directory_name>

Example: Copy and rename a directory

    $cp -R <directory> <new_directory>

Example: Copy all files of specific type to a directory

    $cp *.txt <directory>
<br>

    $ln -s <file> <link name>
Create an alias (link) to a file
 - $ln -s : Create a soft link (A link that functionas across machines)


    $sort <file>
Sort the content of a file
     -r reverse sorts
     -n numeric sort

Example: sort the <file> and write the result to sorted.txt
    $sort <file> | uniq -u > sorted.txt


    $uniq [-ucd] filename(s)
Looks for duplicate lines. Data must be sorted first
 - $uniq -d : show only one copy of the duplicate lines
 - $uniq -u : Show only lines that are not duplicate
 - $uniq -c : Output each line preceded by a count of occurrences
Example: show users that are connected more than once
    $who | cut -d’ ‘ -f1 | sort | uniq -d


    $grep <pattern> <file_name> 
Prints lines that contain a match for a pattern.
 - $grep -i : Perform case insensitive matching
 - $grep -v : Prints all lines that don’t contain the regex
 - $grep -r <pattern> <directory> : Recursively search subdirectories listed and prints file names with occurrence of the pattern
 - $grep -I : Exclude binary files


    $tr “string1” [“string 2”]
Search and replace tool. tr only accepts its input from pipes and redirections. it doesn’t accept files as input.
 - tr -d : Delete all occurrences of all CHARACTERS in string1

Example: Print a.txt to screen after deleting all occurences of “;”
    $cat a.txt | tr -d “;”
 - tr -s : Replace occurrences with a single character

Example: 
    $echo “SSSS SS” | tr -s “S” “S”


    $tar
Creates and manipulates streaming archive files.  This implementation can extract from tar, pax, cpio, zip, jar, ar, and ISO images and can create tar, 
pax, cpio, ar, and shar archives.     

<br>

**DISK USAGE**

    $du [file or directory]
Display the file system block usage for each file or directory. If no file/directory is specified, the block usage of the current directory is displayed.
 - $du -a : Files & directories (default is directories only)
 - $du -h : Human readable format
Example: Display disk usage, ordered, only MB files
    $du -h <directory>| grep -i “m\t” | sort -n
Example: Find out top 10 largest file/directories
    $du -a /var | sort -n -r | head -n 10


    $df
Display free disk space
 - $df -h : Human readable format

<br>

**REDIRECTIONS & PIPES**

\> Redirect standard output. Dont overwrite file if it exists

\>! Redirect standard output. Overwrite file if it exists 

\>& Redirect standard output and standard error

Example: Redirect command output into a file

    $ls > result.txt

Use \> /dev/null file to dispose of errors message

Example: Find a file named my_file_name and print the result to ~/find.txt; Hide errors (e.g. “permissions denied”)

    $find / -name my_file_name.* > /dev/null > ~/find.txt

\< Redirect standard input

\>> Append standard output
\<command> >> <file> : Append <command> output to the end of an existing <file>

\<command> < <file> : Redirect input to a command from a file

\| Redirect standard output to another command (pipe)

Example: Show paginated details of running processes

    $ps -ex | more

<command1> | <command2> : Pipe output of command1 to be the input of command2 (If an output file is desired in the middle of a pipe use the tee command)

Example: Count the number of connected users

    $who | wc -l

<br><br>
**PROCESSES**

    $ps
Show active processes
 - $ps -e : Show information about the process
 - $ps -x : Show hidden processes

Example: Find processes by name
    $grep -l <process_name_regex>

    $kill [-signal] pid
Kill a process.

Some of the more commonly used signals:
 - 3 : QUIT (quit)
 - 9 : KILL (non-catchable, non-ignorable kill)
 - 15 : TERM (software termination signal)


    $top
Display and update sorted information about processes
See man pages for list of possible keys. common keys are:  cpu,  threads, ports
 - $top -o <key> 


    $htop
Display and update sorted information about processes
<br><br><br>
**USER & PERMISSIONS**

    $sudo <command>
Execute the command as a super user

    $su
(substitute user) opens a session as an admin

    $exit
Exit root

    $whoami
Display effective user id

    $who
Print all connected user names

    $passwd
Change password

    $chmod <who> <operation> <permissions> <file or directory name>
Change owner/group access to a file or directory<br>

Who: u user; g group; o other; a all above

Operation:  + add; - remove;  = set (meaning reset to nothing and set only what was specified)

Permissions: r w x

Example: Adds read/execute permissions to group

    $chmod g +rx <file>
Example:

    $chmod 743 <file>
Note that to $cd into a directory you need the x permissions


