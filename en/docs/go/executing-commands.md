---
title: "Executing Commands"
slug: "executing-commands"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Timing Out with Interrupt and then Kill
    c := exec.Command(name, arg...)
    b := &bytes.Buffer{}
    c.Stdout = b
    c.Stdin = stdin
    if err := c.Start(); err != nil {
        return nil, err
    }
    timedOut := false
    intTimer := time.AfterFunc(timeout, func() {
        log.Printf("Process taking too long. Interrupting: %s %s", name, strings.Join(arg, " "))
        c.Process.Signal(os.Interrupt)
        timedOut = true
    })
    killTimer := time.AfterFunc(timeout*2, func() {
        log.Printf("Process taking too long. Killing: %s %s", name, strings.Join(arg, " "))
        c.Process.Signal(os.Kill)
        timedOut = true
    })
    err := c.Wait()
    intTimer.Stop()
    killTimer.Stop()
    if timedOut {
        log.Print("the process timed out\n")
    }

## Executing a Command then Continue and Wait
    cmd := exec.Command("sleep", "5")

    // Does not wait for command to complete before returning
    err := cmd.Start()
    if err != nil {
        log.Fatal(err)
    }

    // Wait for cmd to Return
    err = cmd.Wait()
    log.Printf("Command finished with error: %v", err)

## Simple Command Execution
    // Execute a command a capture standard out. exec.Command creates the command
    // and then the chained Output method gets standard out. Use CombinedOutput() 
    // if you want both standard out and standerr output
    out, err := exec.Command("echo", "foo").Output()
    if err != nil {
        log.Fatal(err)
    }

## Running a Command twice
> A Cmd cannot be reused after calling its Run, Output or CombinedOutput methods

Running a command twice *will **not** work*:

    cmd := exec.Command("xte", "key XF86AudioPlay")
    _ := cmd.Run() // Play audio key press
    // .. do something else
    err := cmd.Run() // Pause audio key press, fails

> Error: exec: already started

Rather, one must use **two separate** `exec.Command`. You might also need some delay between commands.

    cmd := exec.Command("xte", "key XF86AudioPlay")
    _ := cmd.Run() // Play audio key press
    // .. wait a moment
    cmd := exec.Command("xte", "key XF86AudioPlay")
    _ := cmd.Run() // Pause audio key press

