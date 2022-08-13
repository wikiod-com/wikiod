---
title: "Hadoop commands"
slug: "hadoop-commands"
draft: false
images: []
weight: 9922
type: docs
toc: true
---

## Syntax
- Hadoop v1 commands:
  `hadoop fs -<command>`

- Hadoop v2 commands:
  `hdfs dfs -<command>`

## Hadoop v1 Commands
# 1. Print the Hadoop version

    hadoop version

# 2. List the contents of the root directory in HDFS
#

    hadoop fs -ls /

# 3. Report the amount of space used and
# available on currently mounted filesystem
#

    hadoop fs -df hdfs:/

# 4. Count the number of directories,files and bytes under
# the paths that match the specified file pattern
#

    hadoop fs -count hdfs:/


# 5. Run a DFS filesystem checking utility
#

    hadoop fsck – /

# 6. Run a cluster balancing utility
#

    hadoop balancer

# 7. Create a new directory named “hadoop” below the
# /user/training directory in HDFS. Since you’re
# currently logged in with the “training” user ID,
# /user/training is your home directory in HDFS.
#

    hadoop fs -mkdir /user/training/hadoop

# 8. Add a sample text file from the local directory
# named “data” to the new directory you created in HDFS
# during the previous step.
#

    hadoop fs -put data/sample.txt /user/training/hadoop

# 9. List the contents of this new directory in HDFS.
#

    hadoop fs -ls /user/training/hadoop

# 10. Add the entire local directory called “retail” to the
# /user/training directory in HDFS.
#

    hadoop fs -put data/retail /user/training/hadoop

# 11. Since /user/training is your home directory in HDFS,
# any command that does not have an absolute path is
# interpreted as relative to that directory. The next
# command will therefore list your home directory, and
# should show the items you’ve just added there.
#

    hadoop fs -ls

# 12. See how much space this directory occupies in HDFS.
#

    hadoop fs -du -s -h hadoop/retail

# 13. Delete a file ‘customers’ from the “retail” directory.
#

    hadoop fs -rm hadoop/retail/customers

# 14. Ensure this file is no longer in HDFS.
#

    hadoop fs -ls hadoop/retail/customers

# 15. Delete all files from the “retail” directory using a wildcard.
#

    hadoop fs -rm hadoop/retail/*

# 16. To empty the trash
#

    hadoop fs -expunge

# 17. Finally, remove the entire retail directory and all
# of its contents in HDFS.
#

    hadoop fs -rm -r hadoop/retail

# 18. List the hadoop directory again
#

    hadoop fs -ls hadoop

# 19. Add the purchases.txt file from the local directory
# named “/home/training/” to the hadoop directory you created in HDFS
#

    hadoop fs -copyFromLocal /home/training/purchases.txt hadoop/

# 20. To view the contents of your text file purchases.txt
# which is present in your hadoop directory.
#

    hadoop fs -cat hadoop/purchases.txt

# 21. Add the purchases.txt file from “hadoop” directory which is present in HDFS directory
# to the directory “data” which is present in your local directory
#

    hadoop fs -copyToLocal hadoop/purchases.txt /home/training/data

# 22. cp is used to copy files between directories present in HDFS
#

    hadoop fs -cp /user/training/*.txt /user/training/hadoop

# 23. ‘-get’ command can be used alternaively to ‘-copyToLocal’ command
#

    hadoop fs -get hadoop/sample.txt /home/training/

# 24. Display last kilobyte of the file “purchases.txt” to stdout.
#

    hadoop fs -tail hadoop/purchases.txt

# 25. Default file permissions are 666 in HDFS
# Use ‘-chmod’ command to change permissions of a file
#

    hadoop fs -ls hadoop/purchases.txt
    sudo -u hdfs hadoop fs -chmod 600 hadoop/purchases.txt

# 26. Default names of owner and group are training,training
# Use ‘-chown’ to change owner name and group name simultaneously
#

    hadoop fs -ls hadoop/purchases.txt
    sudo -u hdfs hadoop fs -chown root:root hadoop/purchases.txt

# 27. Default name of group is training
# Use ‘-chgrp’ command to change group name
#

    hadoop fs -ls hadoop/purchases.txt
    sudo -u hdfs hadoop fs -chgrp training hadoop/purchases.txt

# 28. Move a directory from one location to other
#

    hadoop fs -mv hadoop apache_hadoop

# 29. Default replication factor to a file is 3.
# Use ‘-setrep’ command to change replication factor of a file
#

    hadoop fs -setrep -w 2 apache_hadoop/sample.txt

# 30. Copy a directory from one node in the cluster to another
# Use ‘-distcp’ command to copy,
# -overwrite option to overwrite in an existing files
# -update command to synchronize both directories
#

    hadoop fs -distcp hdfs://namenodeA/apache_hadoop hdfs://namenodeB/hadoop

# 31. Command to make the name node leave safe mode
#

    hadoop fs -expunge
    sudo -u hdfs hdfs dfsadmin -safemode leave

# 32. List all the hadoop file system shell commands
#

    hadoop fs

# 33.  Get hdfs quota values and the current count of names and bytes in use.
#

    hadoop fs -count -q [-h] [-v] <directory>...<directory>

# 34. Last but not least, always ask for help!
#

    hadoop fs -help

## Hadoop v2 Commands
**appendToFile:** Append single src, or multiple srcs from local file system to the destination file system. Also reads input from stdin and appends to destination file system. Keep the <localfile> as `-` 

     hdfs dfs -appendToFile [localfile1 localfile2 ..] [/HDFS/FILE/PATH..]


**cat:** Copies source paths to stdout.

     hdfs dfs -cat URI [URI …]


**chgrp:** Changes the group association of files. With -R, makes the change recursively by way of the directory structure. The user must be the file owner or the superuser.

     hdfs dfs -chgrp [-R] GROUP URI [URI …]

**chmod:** Changes the permissions of files. With -R, makes the change recursively by way of the directory structure. The user must be the file owner or the superuser

     hdfs dfs -chmod [-R] <MODE[,MODE]... | OCTALMODE> URI [URI …]


**chown:** Changes the owner of files. With -R, makes the change recursively by way of the directory structure. The user must be the superuser.

     hdfs dfs -chown [-R] [OWNER][:[GROUP]] URI [URI ]


**copyFromLocal:** Works similarly to the put command, except that the source is restricted to a local file reference.

     hdfs dfs -copyFromLocal <localsrc> URI


**copyToLocal:** Works similarly to the get command, except that the destination is restricted to a local file reference.

     hdfs dfs -copyToLocal [-ignorecrc] [-crc] URI <localdst>


**count:** Counts the number of directories, files, and bytes under the paths that match the specified file pattern.

     hdfs dfs -count [-q] [-h] <paths>


**cp:** Copies one or more files from a specified source to a specified destination. If you specify multiple sources, the specified destination must be a directory.

     hdfs dfs -cp URI [URI …] <dest>


**du:** Displays the size of the specified file, or the sizes of files and directories that are contained in the specified directory. If you specify the -s option, displays an aggregate summary of file sizes rather than individual file sizes. If you specify the -h option, formats the file sizes in a "human-readable" way.

     hdfs dfs -du [-s] [-h] URI [URI …]


**dus:** Displays a summary of file sizes; equivalent to hdfs dfs -du –s.

     hdfs dfs -dus <args>

**expunge:** Empties the trash. When you delete a file, it isn't removed immediately from HDFS, but is renamed to a file in the /trash directory. As long as the file remains there, you can undelete it if you change your mind, though only the latest copy of the deleted file can be restored.

     hdfs dfs –expunge

**get:** Copies files to the local file system. Files that fail a cyclic redundancy check (CRC) can still be copied if you specify the -ignorecrc option. The CRC is a common technique for detecting data transmission errors. CRC checksum files have the .crc extension and are used to verify the data integrity of another file. These files are copied if you specify the -crc option.

     hdfs dfs -get [-ignorecrc] [-crc] <src> <localdst>


**getmerge:** Concatenates the files in src and writes the result to the specified local destination file. To add a newline character at the end of each file, specify the addnl option.

     hdfs dfs -getmerge <src> <localdst> [addnl]


**ls:** Returns statistics for the specified files or directories.

     hdfs dfs -ls <args>


**lsr:** Serves as the recursive version of ls; similar to the Unix command ls -R.

     hdfs dfs -lsr <args>


**mkdir:** Creates directories on one or more specified paths. Its behavior is similar to the Unix mkdir -p command, which creates all directories that lead up to the specified directory if they don't exist already.

     hdfs dfs -mkdir <paths>


**moveFromLocal:** Works similarly to the put command, except that the source is deleted after it is copied.

     hdfs dfs -moveFromLocal <localsrc> <dest>


**mv:** Moves one or more files from a specified source to a specified destination. If you specify multiple sources, the specified destination must be a directory. Moving files across file systems isn't permitted.

     hdfs dfs -mv URI [URI …] <dest>


**put:** Copies files from the local file system to the destination file system. This command can also read input from stdin and write to the destination file system.

     hdfs dfs -put <localsrc> ... <dest>


**rm:** Deletes one or more specified files. This command doesn't delete empty directories or files. To bypass the trash (if it's enabled) and delete the specified files immediately, specify the -skipTrash option.

     hdfs dfs -rm [-skipTrash] URI [URI …]


**rm r:** Serves as the recursive version of –rm.

     hdfs dfs -rm -r [-skipTrash] URI [URI …]


**setrep:** Changes the replication factor for a specified file or directory. With -R, makes the change recursively by way of the directory structure.

     hdfs dfs -setrep <rep> [-R] <path>


**stat:** Displays information about the specified path.

     hdfs dfs -stat URI [URI …]


**tail:** Displays the last kilobyte of a specified file to stdout. The syntax supports the Unix -f option, which enables the specified file to be monitored. As new lines are added to the file by another process, tail updates the display.

     hdfs dfs -tail [-f] URI


**test:** Returns attributes of the specified file or directory. Specifies -e to determine whether the file or directory exists; -z to determine whether the file or directory is empty; and -d to determine whether the URI is a directory.

     hdfs dfs -test -[ezd] URI


**text:** Outputs a specified source file in text format. Valid input file formats are zip and TextRecordInputStream.

     hdfs dfs -text <src>


**touchz:** Creates a new, empty file of size 0 in the specified path.

     hdfs dfs -touchz <path>


