---
title: "Splitting Files"
slug: "splitting-files"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Sometimes it's useful to split a file into multiple separate files. If you have large files, it might be a good idea to break it into smaller chunks

## Split a file
Running the split command without any options will split a file into 1 or more separate files containing up to 1000 lines each.

    split file

This will create files named `xaa`, `xab`, `xac`, etc, each containing up to 1000 lines. As you can see, all of them are prefixed with the letter `x` by default. If the initial file was less than 1000 lines, only one such file would be created.

To change the prefix, add your desired prefix to the end of the command line

    split file customprefix

Now files named `customprefixaa`, `customprefixab`, `customprefixac` etc. will be created

To specify the number of lines to output per file, use the `-l` option. The following will split a file into a maximum of 5000 lines

    split -l5000 file

OR

    split --lines=5000 file

Alternatively, you can specify a maximum number of bytes instead of lines. This is done by using the `-b` or `--bytes` options. For example, to allow a maximum of 1MB

    split --bytes=1MB file

## We can use sed with w option to split a file into mutiple files. Files can be split by specifying line address or pattern.
Suppose we have this source file that we would like to split:

    cat -n sourcefile
 *1  On the Ning Nang Nong  
 2  Where the Cows go Bong!  
 3  and the monkeys all say BOO!  
 4  There's a Nong Nang Ning  
 5  Where the trees go Ping!  
 6  And the tea pots jibber jabber joo.  
 7  On the Nong Ning Nang*
     
Command to split the file by line number:

    sed '1,3w f1
    > 4,7w f2' sourcefile
This writes line1 to line3 into file f1 and line4 to line7 into file f2, from the sourcefile.

    cat -n f1
 *1  On the Ning Nang Nong  
 2  Where the Cows go Bong!  
 3  and the monkeys all say BOO!* 

    cat -n f2 
 1  There's a Nong Nang Ning  
 2  Where the trees go Ping!  
 3  And the tea pots jibber jabber joo.  
 4  On the Nong Ning Nang

Command to split the file by context/pattern:

    sed '/Ning/w file1
    > /Ping/w file2' sourcefile
This splits the sourcefile into file1 and file2.
file1 contains all lines that match Ning, file2 contains lines that match Ping.

    cat file1
*On the Ning Nang Nong  
There's a Nong Nang Ning  
On the Nong Ning Nang*  

    cat file2
*Where the trees go Ping!*


     







