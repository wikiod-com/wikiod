---
title: "tee command"
slug: "tee-command"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

tee - read from standard input and write to standard output and files.

The tee command is named after the T-splitter in plumbing, which splits water into two directions and is shaped like an uppercase T.

tee copies data from standard input to each FILE, and also to standard output. In effect, tee duplicates its input, routing it to multiple outputs at once.

## Syntax
 - tee [OPTION]... [FILE]...

## Parameters
| Options                 | Description                                 |
|-------------------------| ------------------------------------------- |
| -a, --append            | Append to the given FILEs. Do not overwrite.|
| -i, --ignore-interrupts | Ignore interrupt signals.                   |
| --help                  | Display a help message, and exit.           |
| --version               | Display version information, and exit.      |



   If a FILE is specified as a dash ("-"), tee writes again to standard output.

## Write output to stdout, and also to a file
The following command displays output only on the screen (stdout).

    $ ls 

The following command writes the output only to the file and not to the screen.

    $ ls > file

The following command (with the help of `tee` command) writes the output both to the screen (stdout) and to the file.

    $ ls | tee file


## Write output from the middle of a pipe chain to a file and pass it back to the pipe
You can also use `tee` command to store the output of a command in a file and redirect the same output to another command.

The following command will write current crontab entries to a file `crontab-backup.txt` and pass the crontab entries to `sed` command, which will do the substituion. After the substitution, it will be added as a new cron job.

    $ crontab -l | tee crontab-backup.txt | sed 's/old/new/' | crontab –


## write the output to multiple files
You can pipe your output to multiple files (including your terminal) by using `tee` like this:

    $ ls | tee file1 file2 file3

## Instruct tee command to append to the file
By default `tee` command overwrites the file. You can instruct `tee` to append to the file using the `–a` option as shown below.

    $ ls | tee –a file


