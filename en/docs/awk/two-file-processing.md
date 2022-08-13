---
title: "Two-file processing"
slug: "two-file-processing"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Check matching fields in two files
Given these two CSV files:

    $ cat file1
    1,line1
    2,line2
    3,line3
    4,line4
    $ cat file2
    1,line3
    2,line4
    3,line5
    4,line6

To print those lines in `file2` whose second column occurs also in the first file we can say:

    $ awk -F, 'FNR==NR {lines[$2]; next} $2 in lines' file1 file2
    1,line3
    2,line4


Here, `lines[]` holds an array that gets populated when reading `file1` with the contents of the second field of each line.

Then, the condition `$2 in lines` checks, for every line in `file2`, if the 2nd field exists in the array. If so, the condition is True and `awk` performs its default action, consisting in printing the full line.

If just one field was needed to be printed, then this could be the expression:

    $ awk -F, 'FNR==NR {lines[$2]; next} $2 in lines {print $1}' file1 file2
    1
    2



## Print awk variables when reading two files
I hope this example will help everyone to understand how awk internal variables like NR, FNR etc change when awk is processing two files.

    awk '{print "NR:",NR,"FNR:",FNR,"fname:",FILENAME,"Field1:",$1}' file1 file2
    NR: 1 FNR: 1 fname: file1 Field1: f1d1
    NR: 2 FNR: 2 fname: file1 Field1: f1d5
    NR: 3 FNR: 3 fname: file1 Field1: f1d9
    NR: 4 FNR: 1 fname: file2 Field1: f2d1
    NR: 5 FNR: 2 fname: file2 Field1: f2d5
    NR: 6 FNR: 3 fname: file2 Field1: f2d9

Where file1 and file2 look like:

    $ cat file1
    f1d1 f1d2 f1d3 f1d4
    
    $ cat file2
    f2d1 f2d2 f2d3 f2d4

Notice how `NR` value keeps increasing among all files, while `FNR` resets on each file.
This is why the expression `NR==FNR` always refer to the first file fed to awk, since only in first file is possible to have `NR` equal to `FNR`.

