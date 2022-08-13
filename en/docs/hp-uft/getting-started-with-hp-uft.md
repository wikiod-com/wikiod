---
title: "Getting started with hp-uft"
slug: "getting-started-with-hp-uft"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
1. Once the binaries are extracted, right-click on setup.exe and click on "Run as Administrator"

2. The window above will appear. Click on Unified Functional Testing Setup. If any prerequisite appears just press "OK"
[![setup window][1]][1]


3. Wait the Setup prepare the installation. After a few minutes click on Next

[![Setup Wizard][2]][2]


4. Accept the terms in the License Agreement.

[![enter image description here][3]][3]


5. On Custom Setup screen you can choose which Add-ins to install. To choose a specific Add-in click on the dropdown and select "Entire feature will be installed on local hard drive". And then click on "Next".

[![enter image description here][4]][4]


6. Verify all the options except “Allow running UFT remotely from Automation Scripts”

    [![enter image description here][5]][5]


  [1]: https://i.stack.imgur.com/9fh2s.png
  [2]: https://i.stack.imgur.com/92OHT.png
  [3]: https://i.stack.imgur.com/LsF99.png`
  [4]: https://i.stack.imgur.com/ghxGR.png
  [5]: https://i.stack.imgur.com/3iX5l.png

7. Click on "Finish" after the installation is completed. Go to desktop and search for the icon of the program to confirm the installation was successful.

## Hello World
   HP UFT use VBScript, so you can create a message box to display to the user the output results 
    
     MsgBox "Hello World!"

   Or you can simple use the Output tab to see the output results of the script

    print "Hello World!"

