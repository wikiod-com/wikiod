---
title : C++CLI Tutorial
slug : c++cli-tutorial
weight : 9980
draft : false
images : []
type : docs
---

C++/CLI is a Microsoft-specific dialect of C++ which interoperates well with .NET.  Originally, it was envisioned as the "most powerful" of .NET languages and included designer support for WinForms applications.  However, new development on C++/CLI has all but ceased, and Microsoft now intends that the language only be used in "interop" scenarios:
   - allowing .NET code to use existing (legacy) C or C++ code, or
   - providing a mechanism for existing C or C++ code to call into (presumably, newer) .NET code.

Interop scenarios that might be complex with P/Invoke can be considerably easier with C++/CLI.

