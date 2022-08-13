---
title: "Extracting and Listing Files in Compressed Archives"
slug: "extracting-and-listing-files-in-compressed-archives"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Extracting files from a .zip archive
Unzipping a zip archive is done with `unzip` function from the `utils` package (which is included in base R). 

    unzip(zipfile = "bar.zip", exdir = "./foo")

This will extract all files in `"bar.zip"` to the `"foo"` directory, which will be created if necessary. Tilde expansion is done automatically from your working directory. Alternatively, you can pass the whole path name to the zipfile.

## Listing files in a .zip archive
Listing files in a zip archive is done with `unzip` function from the `utils` package (which is included in base R). 

    unzip(zipfile = "bar.zip", list = TRUE)

This will list all files in `"bar.zip"` and extract none. Tilde expansion is done automatically from your working directory. Alternatively, you can pass the whole path name to the zipfile.

## Listing files in a .tar archive
Listing files in a tar archive is done with `untar` function from the `utils` package (which is included in base R). 

    untar(zipfile = "bar.tar", list = TRUE)

This will list all files in `"bar.tar"` and extract none. Tilde expansion is done automatically from your working directory. Alternatively, you can pass the whole path name to the tarfile.

## Extracting files from a .tar archive
Extracting files from a tar archive is done with `untar` function from the `utils` package (which is included in base R). 

    untar(tarfile = "bar.tar", exdir = "./foo")

This will extract all files in `"bar.tar"` to the `"foo"` directory, which will be created if necessary. Tilde expansion is done automatically from your working directory. Alternatively, you can pass the whole path name to the tarfile.

## Extract all .zip archives in a directory
With a simple `for` loop, all zip archives in a directory can be extracted.

    for (i in dir(pattern=".zip$"))
        unzip(i)

The `dir` function produces a character vector of the names of the files in a directory matching the regex pattern specified by `pattern`. This vector is looped through with index `i`, using the `unzip` function to extract each zip archive.

