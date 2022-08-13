---
title: "Understanding System ErrorExit Codes in Windows"
slug: "understanding-system-errorexit-codes-in-windows"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

For this process to work, the original error/exit code should start with `0x8007` which generally is an indication it originated from a valid Win32 process.  

However, should no message appear, then it probably didn't originate from a Windows process and therefore, will need to be examined further outside the steps mentioned above.



## Converting exit codes into meaningful messages
Being able to understand Error/Exit codes is a fundamental skill for developers on Window's machine. Yet for many, the cryptic hexadecimal code that can be produced on an application exiting with on error can prove to be time consuming and painstaking process for the developer to track down and isolate.

For instance, on SO, there are several thousand questions all asking about the meaning of what a particular error/exit code means... and as an example, below is one such exit code

> The program '[4432] program.exe' has exited with code -2147023895
> (0x800703e9).

Therefore, in order to identify the cause of the issue we need to convert the exit/error code into something more meaningful and we can do this by undertaking the following process.

 1. From the error code `0x800703e9`, take the last 4 characters `03e9`
 2. Using a [Hexadecimal to Decimal Converter](http://www.binaryhexconverter.com/hex-to-decimal-converter), convert `03e9` to its decimal counterpart, which in this case is `1001`
 3. Using `cmd`, type `net helpmsg 1001` or whatever decimal value is returned from step 3.
 4. A friendly error message should appear that can help identify the cause of the issue, which in this instance, the error returned was `Recursion too deep; the stack overflowed`.

