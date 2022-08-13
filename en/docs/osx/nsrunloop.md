---
title: "NSRunLoop"
slug: "nsrunloop"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## simple daemon application
 A [daemon process][1] executes a program in the background, usually without user interaction. The example below shows how to create a daemon and register a listener, which monitors all open applications. The main part is the function call `NSRunLoop.mainRunLoop().run()`, which starts the daemon.

    class MyObserver: NSObject
    {
        override init() {
            super.init()

            // app listeners
            NSWorkspace.sharedWorkspace().notificationCenter.addObserver(self, selector: "SwitchedApp:", name: NSWorkspaceDidActivateApplicationNotification, object: nil)
        }

        func SwitchedApp(notification: NSNotification!)
        {
            print(notification)
        }
    }


    let observer = MyObserver()

    // simply to keep the command line tool alive - as a daemon process
    NSRunLoop.mainRunLoop().run()

  
You can also use this code as a basis for a server process.
 
 [1]: https://en.wikipedia.org/wiki/Daemon_(computing)


