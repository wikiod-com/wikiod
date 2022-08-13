---
title: "File Transfer using scp"
slug: "file-transfer-using-scp"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
- scp /some/local/directory/file_name user_name@host_name:destination_file_path
- scp user_name@host_name:origin_file_path /some/local/directory

## scp transferring file
To transfer a file securely to another machine - type:

    scp file1.txt tom@server2:$HOME

This example presents transferring `file1.txt` from our host to `server2`'s user `tom`'s home directory.


## scp transferring multiple files
`scp` can also be used to transfer multiple files from one server to another. Below is example of transferring all files from `my_folder` directory with extension `.txt` to `server2`. In Below example all files will be transferred to user `tom` home directory.

    scp /my_folder/*.txt tom@server2:$HOME

## Downloading file using scp
To download a file from remote server to the local machine - type:

    scp tom@server2:$HOME/file.txt /local/machine/path/

This example shows how to download the file named `file.txt` from user `tom`'s home directory to our local machine's current directory.

