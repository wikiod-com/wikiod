---
title: "FTP server"
slug: "ftp-server"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Syntax
- My.Computer.Network.DownloadFile(serverFile As String, localFile As String)
- My.Computer.Network.DownloadFile(serverFile As String, localFile As String, user As String, password As String)
- My.Computer.Network.UploadFile(localFile As String, serverFile As String)
- My.Computer.Network.UploadFile(localFile As String, serverFile As String, user As String, password As String)



## Download file from FTP server
    My.Computer.Network.DownloadFile("ftp://server.my/myfile.txt", "donwloaded_file.txt")

This command download `myfile.txt` file from server named `server.my` and saves it as `donwloaded_file.txt` into working directory. You can specify absolute path for downloaded file.

## Download file from FTP server when login required
    My.Computer.Network.DownloadFile("ftp://srv.my/myfile.txt", "donwload.txt", "Peter", "1234")

This command download `myfile.txt` file from server named `srv.my` and saves it as `donwload.txt` into working directory. You can specify absolute path for downloaded file. File is download by user Peter with password 1234.

## Upload file to FTP server
    My.Computer.Network.UploadFile("example.txt", "ftp://server.my/server_example.txt")

This command upload `example.txt` file from working directory (you could specify absolute path if you want) to server named `server.my`. File stored on the server will be named `server_example.txt`.

## Upload file to FTP server when login required
    My.Computer.Network.UploadFile("doc.txt", "ftp://server.my/on_server.txt", "Peter", "1234")

This command upload `doc.txt` file from working directory (you could specify absolute path if you want) to server named `server.my`. File stored on the server will be named `server_example.txt`. Fill is send on the server by user Peter and password 1234.

