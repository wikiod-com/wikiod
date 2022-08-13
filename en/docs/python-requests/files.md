---
title: "Files"
slug: "files"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Parameters
| Parameters | Function |
| ------ | ------ |
| file   | JSON List of paths to the files.   |
| content_type   | MIME Types   |
| headers   | HTTP Headers   |

The `r` variable in the examples contains the full binary data of whatever file you're sending.

## Simple File Upload
    url = 'http://your_url'
    files = {'file': open('myfile.test', 'rb')}
    r = requests.post(url, files=files)



## File Upload w/ Manual Params
    url = 'http://httpbin.org/post'
    files = {'file': ('report.xls', open('report.xls', 'rb'), 'application/vnd.ms-excel', {'Expires': '0'})}
    r = requests.post(url, files=files)

## Sending Strings as FIles
    url = 'http://httpbin.org/post'
    files = {'file': ('report.csv', 'some,data,to,send\nanother,row,to,send\n')}
    r = requests.post(url, files=files)
    r.text

