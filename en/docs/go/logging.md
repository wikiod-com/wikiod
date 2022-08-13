---
title: "Logging"
slug: "logging"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Basic Printing
Go has a built-in logging library known as `log` with a commonly use method `Print` and its variants. You can import the library then do some basic printing:

    package main

    import "log"

    func main() {

        log.Println("Hello, world!")
        // Prints 'Hello, world!' on a single line

        log.Print("Hello, again! \n")
        // Prints 'Hello, again!' but doesn't break at the end without \n

        hello := "Hello, Stackers!"
        log.Printf("The type of hello is: %T \n", hello)
        // Allows you to use standard string formatting. Prints the type 'string' for %T
        // 'The type of hello is: string
    }

## Logging to file
It is possible to specify log destination with something that statisfies io.Writer interface. With that we can log to file:

    package main
    
    import (
        "log"
        "os"
    )
    
    func main() {
        logfile, err := os.OpenFile("test.log", os.O_RDWR|os.O_CREATE|os.O_APPEND, 0666)
        if err != nil {
            log.Fatalln(err)
        }
        defer logfile.Close()
    
        log.SetOutput(logfile)
        log.Println("Log entry")
    }

Output:

    
    $ cat test.log
    2016/07/26 07:29:05 Log entry

## Logging to syslog
It is also possible to log to syslog with `log/syslog` like this:

    package main
    
    import (
        "log"
        "log/syslog"
    )
    
    func main() {
        syslogger, err := syslog.New(syslog.LOG_INFO, "syslog_example")
        if err != nil {
            log.Fatalln(err)
        }
    
        log.SetOutput(syslogger)
        log.Println("Log entry")
    }

After running we will be able to see that line in syslog:

    Jul 26 07:35:21 localhost syslog_example[18358]: 2016/07/26 07:35:21 Log entry

