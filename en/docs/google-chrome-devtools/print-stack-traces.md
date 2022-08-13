---
title: "Print stack traces"
slug: "print-stack-traces"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Error.stack
Each Error object has a string property named stack that contains the stack trace:[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/2sWn4.jpg

## console.trace()
Instrument your code with [console.trace()][1] calls that print current JavaScript call stacks:
[![enter image description here][2]][2]


  [1]: https://developers.google.com/web/tools/chrome-devtools/debug/console/console-reference#consoletraceobject
  [2]: http://i.stack.imgur.com/0XwSy.jpg

## console.assert()
Place assertions in your JavaScript code by calling [console.assert()][1] with the error condition as the first parameter. When this expression evaluates to false, you will see a corresponding console record:[![console.assert() example][2]][2]


  [1]: https://developers.google.com/web/tools/chrome-devtools/debug/console/console-reference#consoleassertexpression-object
  [2]: http://i.stack.imgur.com/9Xiwb.jpg

