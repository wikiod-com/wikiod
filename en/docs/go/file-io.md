---
title: "File IO"
slug: "file-io"
draft: false
images: []
weight: 9875
type: docs
toc: true
---

## Syntax
- file, err := os.Open(*name*) // Opens a file in read-only mode. A non-nil error is returned if the file could not be opened.
- file, err := os.Create(*name*) // Creates or opens a file if it already exists in write-only mode. The file is overwritten to if it already exists. A non-nil error is returned if the file could not be opened.
- file, err := os.OpenFile(*name*, *flags*, *perm*) // Opens a file in the mode specified by the flags. A non-nil error is returned if the file could not be opened.
- data, err := ioutil.ReadFile(*name*) // Reads the entire file and returns it. A non-nil error is returned if the entire file could not be read.
- err := ioutil.WriteFile(*name*, *data*, *perm*) // Creates or overwrites a file with the provided data and UNIX permission bits. A non-nil error is returned if the file failed to be written to.
- err := os.Remove(*name*) // Deletes a file. A non-nil error is returned if the file could not be deleted.
- err := os.RemoveAll(*name*) // Deletes a file or whole directory hierarchy. A non-nil error is returned if the file or directory could not be deleted.
- err := os.Rename(*oldName*, *newName*) // Renames or moves a file (can be across directories). A non-nil error is returned if the file could not be moved.

## Parameters
| Parameter | Details |
| ------ | ------ |
| name   | A filename or path of type string. For example: `"hello.txt"`.|
| err   | An `error`. If not `nil`, it represents an error that occurred when the function was called.|
| file   | A file handler of type `*os.File` returned by the `os` package file related functions. It implements an `io.ReadWriter`, meaning you can call `Read(data)` and `Write(data)` on it. Note that these functions may not be able to be called depending on the open flags of the file. |
| data   | A slice of bytes (`[]byte`) representing the raw data of a file.|
| perm   | The UNIX permission bits used to open a file with of type `os.FileMode`. Several constants are available to help with the use of permission bits.|
| flag   | File open flags that determine the methods that can be called on the file handler of type `int`. Several constants are available to help with the use of flags. They are: `os.O_RDONLY `, `os.O_WRONLY `, `os.O_RDWR `, `os.O_APPEND`, `os.O_CREATE`, `os.O_EXCL`, `os.O_SYNC`, and `os.O_TRUNC`.|

## Reading and writing to a file using ioutil
A simple program that writes "Hello, world!" to `test.txt`, reads back the data, and prints it out. Demonstrates simple file I/O operations.

    package main
    
    import (
        "fmt"
        "io/ioutil"
    )
    
    func main() {
        hello := []byte("Hello, world!")
    
        // Write `Hello, world!` to test.txt that can read/written by user and read by others 
        err := ioutil.WriteFile("test.txt", hello, 0644)
        if err != nil {
            panic(err)
        }
    
        // Read test.txt
        data, err := ioutil.ReadFile("test.txt")
        if err != nil {
            panic(err)
        }
    
        // Should output: `The file contains: Hello, world!`
        fmt.Println("The file contains: " + string(data))
    }

## Listing all the files and folders in the current directory
    package main
    
    import (
        "fmt"
        "io/ioutil"
    )
    
    func main() {
        files, err := ioutil.ReadDir(".")
        if err != nil {
            panic(err)
        }
    
        fmt.Println("Files and folders in the current directory:")
    
        for _, fileInfo := range files {
            fmt.Println(fileInfo.Name())
        }
    }


## Listing all folders in the current directory
```
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    files, err := ioutil.ReadDir(".")
    if err != nil {
        panic(err)
    }

    fmt.Println("Folders in the current directory:")

    for _, fileInfo := range files {
        if fileInfo.IsDir() {
            fmt.Println(fileInfo.Name())
        }
    }
}
```

