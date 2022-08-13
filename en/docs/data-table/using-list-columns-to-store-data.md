---
title: "Using list columns to store data"
slug: "using-list-columns-to-store-data"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Data.table supports column vectors belonging to R's `list` class.

In case it looks weird that we're talking about lists without using that word in the code, note that `.()` is an alias for `list()` when used inside a `DT[...]` call.

## Reading in many related files
Suppose we want to read and stack a bunch of similarly-formatted files. The quick solution is:

    rbindlist(lapply(list.files(patt="csv$"), fread), id=TRUE)

We might not be satisfied with this for a couple reasons:
- It might run into errors when reading with `fread` or when stacking with `rbindlist` due to inconsistent or buggy data formatting.
- We may want to keep track of metadata for each file, grabbed from the file name or perhaps from some header rows within the (not quite tabular) files.

One way to handle this is to make a "files table" and store the contents of each file as a list-column entry on the row associated with it.

# Example data

*Before making the example data below, make sure you're in an empty folder you can write to. Run `getwd()` and read `?setwd` if you need to change folders.*

    # example data
    set.seed(1)
    for (i in 1:3) 
      fwrite(data.table(id = 1:2, v = sample(letters, 2)), file = sprintf("file201%s.csv", i))

# Identify files and file metadata

This part is fairly straightforward:

    # First, identify the files you want:
    fileDT = data.table(fn = list.files(pattern="csv$"))

    # Next, optionally parse the names for metadata using regex:
    fileDT[, year := type.convert(sub(".*([0-9]{4}).*", "\\1", fn))]

    # Finally construct a string file-ID column:
    fileDT[, id := as.character(.I)]

    #              fn year id
    # 1: file2011.csv 2011  1
    # 2: file2012.csv 2012  2
    # 3: file2013.csv 2013  3

# Read in files

Read in the files as a list column:

    fileDT[, contents := .(lapply(fn, fread))]

    #              fn year id     contents
    # 1: file2011.csv 2011  1 <data.table>
    # 2: file2012.csv 2012  2 <data.table>
    # 3: file2013.csv 2013  3 <data.table>

If there's a snag in reading one of the files or you need to change the arguments to `fread` depending on the file's attributes, this step can easily be extended, looking like:

    fileDT[, contents := {
      cat(fn, "\n")

      dat = if (year %in% 2011:2012){
        fread(fn, some_args)
      } else {
        fread(fn)
      }

      .(.(dat))
    }, by=fn]

For details on options for reading in CSVs and similar files, see `?fread`.

# Stack data

From here, we want to stack the data:

    fileDT[, rbindlist(setNames(contents, id), idcol="file_id")]

    #    file_id id v
    # 1:       1  1 g
    # 2:       1  2 j
    # 3:       2  1 o
    # 4:       2  2 w
    # 5:       3  1 f
    # 6:       3  2 w

If some problem occurs in stacking (like column names or classes not matching), we can go back to the individual tables in `fileDT` to inspect where the problem originated. For example,

    fileDT[id == "2", contents[[1]]]
    #    id v
    # 1:  1 o
    # 2:  2 w

# Extensions

If the files are not in your current working dir, use

    my_dir = "whatever"
    fileDT = data.table(fn = list.files(my_dir, pattern="*.csv"))

    # and when reading
    fileDT[, contents := .(lapply(fn, function(n) fread(file.path(my_dir, n))))]




