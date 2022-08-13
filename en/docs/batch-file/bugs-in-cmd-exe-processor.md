---
title: "Bugs in cmd.exe processor"
slug: "bugs-in-cmdexe-processor"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

This topic will focus on errors caused by the processor bugs. Here are the things we would focus on the cause and the solution of the issue.


In the example [DEL File Extension][1], user X. Liu notices that this bug will **not** occurs when the file extension in the `DEL` command is less than 3 characters.


  [1]: https://www.wikiod.com/batch-file/bugs-in-cmdexe-processor#DEL File Extension

## Parentheses Confusion
From [this][1] website, the OP has noticed a problem.

---

# Cause

 Consider the following code snippet.

    if 1==1 (
        set /a result = 2*(3+4)
    )

At your first glance, you may think `CMD.exe` would process it like so:

 - The condition is true, execute code block
 - Set variable result's value to `14`
 - Continue
 
However, `CMD.exe` process like so:

 - The condition is true, execute code block
 - **Calculate `2*(3+4`, the `)` after 4 is processed at the end of `if` code block**
 - A random `)` has appeared!

The second step would return `Unbalanced parentheses` error.

---

# Solution

According to a German CMD.exe's `set /?`, we would need to quote arithmetic operations. Here's an example.

| Previous | Result |
| ----     | -----  | 
|`set /a result=2+5*4`|`set /a result="2+5*4"`


By the way, according to an English CMD.exe `set /?`, quotes are required if logical or modulus operators are present in the expression(although this is not a *must-do* step).



  [1]: https://www.pcreview.co.uk/threads/bug-in-cmd-exe-parentheses-confused.2916962/

## Improper Escape Character
[In this Stack Overflow question][1], user txtechhelp found an issue with the `^` character which could cause a security issue.

---

# Cause

    anyInvaildCommand ^

Note: Make sure the caret(`^`) is the last character! Any extra `CR\LF` won't work at all!

The caret looks for the next character to escape. However, the are no more character available to escape, so `cmd` loops infinitely, looking for a character to escape. In this "loop" process, `cmd.exe` will consume your computer memory. And gradually eating all memory, bringing the computer to knees.

This issue can lead to more serious security worries as one could just enter the code into the one's unlocked computer.

# Solutions

- Use codepage **UTF-16** could solve this problem. Only **UTF-8** or **ASCII** would cause the bug.

- Make sure there is an extra `CR\LF` in the file, or just simply don't use caret at the end of the file.

# Extra

This bug seems to be solved in Windows 10.

  [1]: https://stackoverflow.com/questions/23284131/cmd-exe-parsing-bug-leads-to-other-exploits

## DEL File Extension
This bug was reported by steve2916 from this [Microsoft Windows Forum thread.][1]

---

# Cause

Consider a folder with such files.
 
    TestA.doc
    TestB.doc
    TestC.docx
    TestD.docx

If we want to remove all `.doc` file in this directory, we usually would do:

    del *.doc

However, this command also removes the `.docx` files. The same happens on file extensions with this pattern.

    
| File A | File B |
| ------ | ------ |
| Anyname.**abc**   | AnotherName.**abc**d   |

As we can see, as long as the file extension string contains the string used in the `del` command, the file will be deleted. Note that this bug only occurs when the extension string used in the `del` command has at least three characters. For instance, `del *.do` doesn't delete `A.doc` or `A.docx`.

# Solution

In the thread, user MicroCompsUnltd noticed that using `Powershell` would solve the issue.

     


  [1]: https://answers.microsoft.com/en-us/windows/forum/windows8_1-performance/bug-report-serious-del-command-from-command-prompt/511a6883-87c1-47a0-9f6f-3b32f12927db

