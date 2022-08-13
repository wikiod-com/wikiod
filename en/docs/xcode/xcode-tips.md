---
title: "Xcode Tips"
slug: "xcode-tips"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Reuse code snippets in Xcode
You can save your code snippets for use later simply by drag and drop. For eg: if you have an NSLog statement that used for so many places somewhere else in the project, then you can save the NSLog statements to code snippets library.

[![enter image description here][1]][1]


Drag the NSLog statement to code snippet library.

[![enter image description here][2]][2]

Now you can simply reuse the code snippet anywhere else on project. Also you can customise the code snippet and can add placeholder texts by give statements bw <# and #>.

[![enter image description here][3]][3]

Drag and drop this code snippet gives an NSLog with placeholder text.

[![enter image description here][4]][4]


  [1]: http://i.stack.imgur.com/MfU7t.jpg
  [2]: http://i.stack.imgur.com/MZqut.jpg
  [3]: http://i.stack.imgur.com/9hgDG.jpg
  [4]: http://i.stack.imgur.com/7aUUI.jpg

## Hide strange unwanted and extra Xcode 8 logs.

 1. From Xcode menu open: Product > Scheme > Edit Scheme.
 2.  On your Environment Variables set OS_ACTIVITY_MODE = disable


[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/7SFvn.gif

## Install Plugins on Xcode 7
Xcode by itself has quite a few good tools built in, but sometimes you just want to change a specific behavior or create a convenience shortcut.
That's why there's [Alcatraz](http://alcatraz.io).

# Installation
```
curl -fsSL https://raw.githubusercontent.com/supermarin/Alcatraz/deploy/Scripts/install.sh | sh
```
Throw this in a terminal, restart Xcode and you're good to go.

# Recommendations
A few popular ones include:

- [`VVDocumenter`](https://github.com/onevcat/VVDocumenter-Xcode) - Type three `/` above any method, class, ... declaration to add documentation
- [`XcodeColors`](https://github.com/robbiehanson/XcodeColors) - Colored console logs, e. g. using [CocoaLumberjack](https://github.com/CocoaLumberjack/CocoaLumberjack)
- [`FuzzyAutocomplete`](https://github.com/FuzzyAutocomplete/FuzzyAutocompletePlugin) - Type "NSog" and still get `NSLog` autocompleted
- [`BuildTimeAnalyzer`](https://github.com/RobertGummesson/BuildTimeAnalyzer-for-Xcode) - Set `-Xfrontend -debug-time-function-bodies` under `Other Swift flags` in the build settings and [optimize your Swift build time](https://medium.com/@RobertGummesson/regarding-swift-build-time-optimizations-fc92cdd91e31#.hov5rzn7v)

Of course there are many more and some are so good, Apple already implemented them into Xcode 8 (FuzzyAutocomplete and VVDocumenter for example)


# Usage
[![menu][1]][1]
Hit `⌘ + ⇧ + 9` or use this menu to open up the Package manager.

[![package manager][2]][2]

Click install on any package you want installed and afterwards restart Xcode again.


  [1]: http://i.stack.imgur.com/IVr2H.png
  [2]: http://i.stack.imgur.com/NUBwk.png

