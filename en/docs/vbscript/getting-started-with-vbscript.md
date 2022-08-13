---
title: "Getting started with vbscript"
slug: "getting-started-with-vbscript"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World message using cscript and wscript
    WScript.Echo "Hello world!"

This displays a message on the console if run with `cscript.exe` (the console host) or in a message box if run with `wscript.exe` (the GUI host).

If you're using VBScript as the server-side scripting language for a web page (for classic ASP, for example),

    Response.Write "Hello world!"

puts the message into the HTML send to the client (browser).

If you want to displays a message in the message box, you can use:

     Msgbox "Hello World!"

