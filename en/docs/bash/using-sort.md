---
title: "Using sort"
slug: "using-sort"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

sort is a Unix command to order data in file(s) in a sequence.

## Syntax
  - sort [option] filename

## Parameters
| Option | Meaning|
| ------ | ------ |
|  -u | Make each lines of output unique |

Full user manual of `sort` reading [online][1]


  [1]: http://man7.org/linux/man-pages/man1/sort.1.html

## Sort command output
`sort` command is used to sort a list of lines.

**Input from a file**

    sort file.txt  

**Input from a command**

You can sort any output command. In the example a list of file following a pattern.

    find * -name pattern | sort

## Make output unique
If each lines of the output need to be unique, add `-u` option.

To display owner of files in folder

    ls -l | awk '{print $3}' | sort -u



## Numeric sort
Suppose we have this file:

    test>>cat file
    10.Gryffindor
    4.Hogwarts
    2.Harry
    3.Dumbledore
    1.The sorting hat
To sort this file numerically, use sort with -n option:  

    test>>sort -n file  
This should sort the file as below:  

    1.The sorting hat  
    2.Harry  
    3.Dumbledore  
    4.Hogwarts  
    10.Gryffindor

Reversing sort order:
To reverse the order of the sort use the -r option

To reverse the sort order of the above file use:  

    sort -rn file

 This should sort the file as below:

    10.Gryffindor
    4.Hogwarts
    3.Dumbledore
    2.Harry
    1.The sorting hat

   

 



  





## Sort by keys
Suppose we have this file:

  

    test>>cat Hogwarts 
      Harry        Malfoy        Rowena        Helga
      Gryffindor   Slytherin     Ravenclaw     Hufflepuff
      Hermione     Goyle         Lockhart      Tonks
      Ron          Snape         Olivander     Newt
      Ron          Goyle         Flitwick      Sprout


To sort this file using a column as key use the k option:

  

    test>>sort -k 2 Hogwarts

This will sort the file with column 2 as the key:

  

      Ron        Goyle        Flitwick        Sprout
      Hermione   Goyle        Lockhart        Tonks
      Harry      Malfoy       Rowena          Helga
      Gryffindor Slytherin    Ravenclaw       Hufflepuff
      Ron        Snape        Olivander       Newt

Now if we have to sort the file with a secondary key along with the primary key use:

  

    sort -k 2,2 -k 1,1 Hogwarts 

This will first sort the file  with column 2 as primary key, and then sort the file with column 1 as secondary key: 

  

      Hermione        Goyle        Lockhart        Tonks
      Ron             Goyle        Flitwick        Sprout
      Harry           Malfoy       Rowena          Helga
      Gryffindor      Slytherin    Ravenclaw       Hufflepuff
      Ron             Snape        Olivander       Newt

  If we need to sort a file with more than 1 key , then for every -k option we need to specify where the sort ends. So -k1,1 means start the sort at the first column and end sort at first column.

**-t option**

In the previous example the file had the default delimeter - tab.
In case of sorting a file that has non-default delimeter we need the -t option to specify the delimeter.
Suppose we have the file as below:

    test>>cat file
    5.|Gryffindor
    4.|Hogwarts
    2.|Harry
    3.|Dumbledore
    1.|The sorting hat

To sort this file as per the second column, use:

    test>>sort -t "|" -k 2 file
This will sort the file as below:  

    3.|Dumbledore
    5.|Gryffindor
    2.|Harry
    4.|Hogwarts
    1.|The sorting hat




