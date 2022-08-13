---
title: "Create a Custom framework in iOS"
slug: "create-a-custom-framework-in-ios"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Create Framework in Swift
follow these step to creating Custom Framework in Swift-IOS:

 1. Create a new project. In Xcode
 2. Choose iOS/Framework & Library/Cocoa Touch Framework to create a new framework
 3. click next and set the productName
 4. click next and choose directory to create Project there
 5. add code and resources to created project

Framework created successfully  

to add created framework to another project, first you should create a workspace  
add "target project" and "framework project" to workspace, then :

 1. go to the general tab of target project
 2. drag the "*.framework" file in product folder of framework project to "Embedded Binaries" section
 3. to use in any ViewController or class just import framework at each file



