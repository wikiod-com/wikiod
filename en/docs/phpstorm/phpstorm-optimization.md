---
title: "PhpStorm optimization"
slug: "phpstorm-optimization"
draft: false
images: []
weight: 9879
type: docs
toc: true
---

## Speed increase using OpenGL
PhpStorm can be very slow in large files as its performing so many inspections. One quick and easy way to speed up PhpStorm is to render using OpenGL. Previously in a 5000 line file it would give the 'eye' symbol in the top right for a long time before changing to a tick (or red/yellow box). After OpenGL it does this almost immediately.

**To enable OpenGL:**

Open: `path-to-phpstorm\bin\PhpStorm64.exe.vmoptions`

Then add these two lines below the others: 

    -Dawt.useSystemAAFontSettings=lcd 
    -Dawt.java2d.opengl=true

## Tuning PhpStorm performance by editing custom VM options
Its possible to change `*.vmoptions` and `idea.properties` files without editing them in the PhpStorm installation folder.

Follow the steps below:

**Step 1:**

Run `Help` - `Edit Custom VM Options...`

[![enter image description here][1]][1]

**Step 2:**

Confirm the creation of the configuration file, if prompted

[![enter image description here][2]][2]

**Step 3:**

Add following lines if you want to use OpenGL

    # This line could already be there depending on your PHPStorm version
    -Dawt.useSystemAAFontSettings=lcd 

    -Dawt.java2d.opengl=true

Add the following lines if you want to increase memory allocated to PhpStorm (improves performance in large projects)

    # This line could already be there depending on your PHPStorm version
    -Xmx750m

[![enter image description here][3]][3]

Save the file and restart program.

  [1]: http://i.stack.imgur.com/V9zqI.png
  [2]: http://i.stack.imgur.com/wxF0x.png
  [3]: http://i.stack.imgur.com/5C347.png

## Power Save Mode
If you're in a hurry and want to quickly get a speed boost PLUS save some power, go into `Power Save Mode`. It's in the File menu, bottom-most option in that menu. 

`File -> Power Save Mode`

Note: this will disable some powerful features like syntax highlighting, code analysis, auto-completing things. It will also stop indexing. 

