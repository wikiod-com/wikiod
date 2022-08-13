---
title: "Protractor Debugger"
slug: "protractor-debugger"
draft: false
images: []
weight: 9902
type: docs
toc: true
---

## Syntax
 - browser.pause()
 - browser.debugger()




This section explains how we can debug protractor tests.

## Using browser.pause()
The `pause()` method is one of the easiest solution Protractor provides you to debug the code, in order to use it you have to add it in your code where you want to pause the execution.Once the execution is in paused state:

1. You can use `C` (type C) to move forward. Be careful while using it, you have to write this command without any delay as you might get timeout error from your assertion library if you delayed to press c.

2. Type `repl` to enter interactive mode. The interactive mode is used to send browser commands directly to open instance of browser. For example in interactive mode you can issue command like this:


    > element(by.css('#username')).getText() 
    > NoSuchElementError: No element found using locator: by.username("#username")

Notice output of above command appears directly over there, which lets you know correctness of your command.

*Note: If you have opened the Chrome Dev Tools, you must close them before continuing the test because ChromeDriver cannot operate when the Dev Tools are open.*

3. Exit debug mode using `CTRL+C`, you can take yourself out from debug mode using classical CTRL+C command.

    
     it('should pause when we use pause method', function () {
        browser.get('/index.html');
        
        var username = element(by.model('username'));
        username.sendKeys('username');
        browser.pause();
    
        var password = element(by.model('password'));
        password.sendKeys('password');
        browser.pause();
    });

4. Press d to continue to the next debugger statement

## Using browser.debugger()
You can use browser.debugger() to stop the execution. You can insert it any place in your code and it will stop the execution after that line until you don't command to continue.

Note: To run the tests in debugger mode you have to issue command like this:

        `protractor debug <configuration.file.js>` 
Enter `c` to start execution and continue after the breakpoint or enter `next` command.The next command steps to the next line in control flow.

The debugger used in Protractor uses [node debugger](https://nodejs.org/api/debugger.html) and it pause the execution in asynchronous way. For example, in below code the `browser.debugger()` will get called when `username.sendKeys('username')` has been executed. 

**Note:** Since these are asynchronous tasks, you would have to increase the default timeout of your specs else default timeout exception would be thrown!

    it('should pause when we use pause method', function () {
    browser.get('/index.html');
    
    var username = element(by.model('username'));
    username.sendKeys('username');
    browser.debugger();

    var password = element(by.model('password'));
    password.sendKeys('password');
    });

One can enter the `repl` mode by entering the command-
```
debug > repl
> element(by.model('abc')).sendKeys('xyz');
```
This will run the sendKeys command as the next task, then re-enter the debugger.

One can change the `Port no.` they want to debug their scripts by just passing the port to the debugger method-

```
browser.debugger(4545); //will start the debugger in port 4545
```

The `debugger()` method injects a client side from Protractor to browser and you can run few commands in browser console in order to fetch the elements. One of the example to use client side script is:

`window.clientSideScripts.findInputs('username');`

