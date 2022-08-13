---
title: "Using axWindowsMediaPlayer in VB.Net"
slug: "using-axwindowsmediaplayer-in-vbnet"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

axWindowsMediaPlayer is the control for the playing multimedia files like videos and music.

## Adding the axWindowsMediaPlayer

 - Right-click on the Toolbox, then click "Choose Items". 
 - Select the COM Components tab, and then check Windows Media Player.
 - axWindowsMediaPlayer will be added to Toolbox.

Select this checkbox to use axWindowsMediaPlayer
[![Select this checkbox!][1]][1]


Then you can use axWindowsMediaPlayer :)
[![Enjoy yourself!][2]][2]




  [1]: https://i.stack.imgur.com/7DyHx.png
  [2]: https://i.stack.imgur.com/BeFqD.png

## Play a Multimedia File
    AxWindowsMediaPlayer1.URL = "C:\My Files\Movies\Avatar.mp4"
    AxWindowsMediaPlayer1.Ctlcontrols.play()

This code will play Avatar in the axWindowsMediaPlayer.

