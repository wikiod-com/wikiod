---
title: "Core SpotLight in iOS"
slug: "core-spotlight-in-ios"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Core-Spotlight
**Objective-C** 

1. Create a new iOS project and add *CoreSpotlight* and *MobileCoreServices* framework to your project.
![enter image description here][1]

 2. Create the actual CSSearchableItem and associating the uniqueIdentifier, domainIdentifier and the attributeSet. Finally index the CSSearchableItem using [[CSSearchableIndex defaultSearchableIndex]...] as show below.
![enter image description here][2]

 3. OK!Test the index!    
![enter image description here][3]


  [1]: http://i.stack.imgur.com/7HUAH.png
  [2]: http://i.stack.imgur.com/Glctb.png
  [3]: http://i.stack.imgur.com/LTPGF.png

