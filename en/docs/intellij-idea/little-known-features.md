---
title: "Little-known features"
slug: "little-known-features"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Quick Preview
<h2>[Check screencast video on YouTube about this feature](https://youtu.be/D6YlYZVprCw)</h2>

IntelliJ provides a quick-preview feature called [Viewing Definition](https://www.jetbrains.com/help/idea/2016.1/viewing-definition.html).
Using this feature allows a user to quickly see the contents of a method/class without navigating into the class itself

* OS X - (<kbd>⌘</kbd>+<kbd>Y</kbd>) or (<kbd>⌥</kbd>+<kbd>Space</kbd>)
* Unix / Windows - <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>I</kbd>

Example: Looking into `Arrays.copyOfRange()`:
[![enter image description here][1]][1]

Verifying if you've selected the right class in `Search`:

[![enter image description here][2]][2]

Or a quick look at some project files:

<img src="http://i.stack.imgur.com/g7Z2P.png" width="426" height="333"/>




  [1]: http://i.stack.imgur.com/HKQC6.png
  [2]: http://i.stack.imgur.com/iWPKj.png

## Language injection
If you want to write strings containing other languages (JSON, regexes), it's hard to keep up with escaping symbols, and it would be nice to get some code assist.

1. Put your cursor inside an empty string
2. `ALT + ENTER`
3. Pick "Inect language or reference"

[![Pick "Inject language or reference"][1]][1]

4. Pick the desirable language (`RegExp` in my case) from the pop-up

[![enter image description here][2]][2]

5. Again use `ALT + ENTER` and pick `Edit regex fragment`

[![enter image description here][3]][3]

6. In the new tool window enter the regex - note how it's automatically mapped to a properly escaped Java string. Similarly for JSON the indents will be placed properly.


  [1]: http://i.stack.imgur.com/F2RpR.png
  [2]: http://i.stack.imgur.com/wh5oN.png
  [3]: http://i.stack.imgur.com/lDbOq.png

