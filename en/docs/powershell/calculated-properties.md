---
title: "Calculated Properties"
slug: "calculated-properties"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Calculated Properties in Powershell are custom derived(Calculated) properties. It lets the user to format a certain property in a way he want it to be. The calculation(expression) can be a quite possibly anything. 

## Display file size in KB - Calculated Properties
Let's consider the below snippet,

    Get-ChildItem -Path C:\MyFolder | Select-Object Name, CreationTime, Length

It simply output the folder content with the selected properties. Something like, 

[![Plain Properties][1]][1]

What if I want to display the file size in KB ? This is where calcualted properties comes handy. 

    Get-ChildItem C:\MyFolder | Select-Object Name, @{Name="Size_In_KB";Expression={$_.Length / 1Kb}}

Which produces,

[![enter image description here][2]][2]



The `Expression` is what holds the calculation for calculated property. And yes, it can be anything! 


  [1]: https://i.stack.imgur.com/4IJGG.png
  [2]: https://i.stack.imgur.com/KPeVM.png

