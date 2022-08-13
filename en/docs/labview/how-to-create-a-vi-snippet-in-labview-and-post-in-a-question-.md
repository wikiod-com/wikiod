---
title: "How to create a VI Snippet in LabVIEW and post in a question."
slug: "how-to-create-a-vi-snippet-in-labview-and-post-in-a-question"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Parameters
| **Parameter** |                        **Details**|
|:---------:|:---------------------------------------------------------:|
|  Number 1 |  Numeric Control |
|  Number 2 |  Numeric Control |
|    Sum    | Numeric Indicator|             |


  [1]: http://i.stack.imgur.com/0Yr4R.png
  [3]: http://i.stack.imgur.com/QOvuD.png

## Create VI snippet from Block Diagram
VI snippets are like Screenshots of a Block Diagram, with one important difference. They can be opened in LabVIEW to reconstruct the orginal program. They are saved in the common `.PNG` format so they can be used like normal pictures e.g. in forums and on StackOverflow.

To create a VI snippet mark the important part of your Block Diagram and hit **"Edit → Create VI Snippet from Selection"**. Then choose where you want to save it. The VI Snippet has a border and a icon to indicate that you can drag them into LabVIEW to add the code to your Block Diagram.

This is what a simple VI snippet looks like:

[![Simple VI Snippet][1]][1]


  [1]: http://i.stack.imgur.com/9dcIG.png

## Snippet VI for summation of two numbers
Creating summation is quite easy in **3 steps**

 1. On ```front panel```, Add **2 numeric controls** (number 1 and Number 2) and **1 numeric indicator** (sum).

[![enter image description here][1]][1]

 1. Switch to ```block diagram```(using **CTRL+E**) and connect **Add block** from **numeric palette**. Wire it to both numeric controls and sum indicator created in **step 1**. Eventually select all blocks by mouse (or by using keyboard **CTRL+A** .
[![enter image description here][2]][2]
 3. Go to **Edit menu**, select "**create VI snippet from selection**". Save yoursnippetname.png to any location.

[![enter image description here][3]][3]
[![enter image description here][4]][4]

 4. You can use snippet by **dragging the image** to ```block diagram```

[![enter image description here][5]][5]


  [1]: http://i.stack.imgur.com/02wJX.png
  [2]: http://i.stack.imgur.com/um6hv.png
  [3]: http://i.stack.imgur.com/GGQ28.png
  [4]: http://i.stack.imgur.com/hoPtx.png
  [5]: http://i.stack.imgur.com/QcOgg.png

