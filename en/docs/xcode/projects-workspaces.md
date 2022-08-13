---
title: "Projects & Workspaces"
slug: "projects--workspaces"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Build, Run, Profile, Test, and Analyze your project
Click the Run button in the toolbar (or press ⌘R) to build & run your project. Click Stop (or press ⌘.) to stop execution.

<img src="http://i.stack.imgur.com/hqUBJ.png" width="380" alt="Build & Run button">

&nbsp;

Click & hold to see the other actions, Test (⌘U), Profile (⌘I), and Analyze (⇧⌘B). Hold down modifier keys <kbd>⌥ option</kbd>, <kbd>⇧ shift</kbd>, and <kbd>⌃ control</kbd> for more variants.

<img src="http://i.stack.imgur.com/CB9tw.png" width="372" alt="Actions dropdown">

&nbsp;

The same actions are available in the Product menu:

<img src="http://i.stack.imgur.com/jFyxR.jpg" width="506">



## Projects overview
Xcode [**projects**](https://developer.apple.com/library/ios/featuredarticles/XcodeConcepts/Concept-Projects.html#//apple_ref/doc/uid/TP40009328-CH5-SW1) are used to organize source files, library dependencies, and other resources, as well as the settings and steps required to build the project's products. [**Workspaces**](https://developer.apple.com/library/ios/featuredarticles/XcodeConcepts/Concept-Workspace.html) are groups of projects and other files.

## Create a project

You can create a **New Project** (⇧⌘N) from a number of built-in templates:

[![screenshot of project templates][1]][1]


  [1]: http://i.stack.imgur.com/EZhSi.png

## Working with projects

An Xcode project window includes:

1. **Toolbar** (top)
2. **Navigator** (left)
3. **Editor** (center)
4. **Inspector** (right)
5. **Variables View** (lower-middle left)
6. **Console output** (lower-middle right)
7. **Library** (lower right)

[![screenshot of project window][2]][2]


  [2]: http://i.stack.imgur.com/X4c4F.png

## Adjust workspace to your needs and freely navigate it
One of the things that can really boost your productivity while writing the code is effectively navigating the workspace. This also means making it comfortable for the moment. It's possible to achieve this by adjusting which areas of workspaces you see.

The buttons on the top of the navigation and assistant areas are not that big and are a bit hard to click with the mouse pointer. That's why there are helpful and easy to remember shortcuts that let you switch between different tabs or hide the area altogether.

You can switch between different panels in the navigation area by holding the <kbd>⌘</kbd> (Command) button and pressing on different digit keys from 1 to 8 or 0. The <kbd>0</kbd> key toggles the presence of the navigator. Here's a list of shortcuts for your convenience:

 1. <kbd>⌘</kbd>+<kbd>1</kbd> - File navigator;
 2. <kbd>⌘</kbd>+<kbd>2</kbd> - Symbol navigator;
 3. <kbd>⌘</kbd>+<kbd>2</kbd> - Search (also reachable through <kbd>⇧</kbd>+<kbd>⌘</kbd>+<kbd>F</kbd>);
 4. <kbd>⌘</kbd>+<kbd>4</kbd> - Warnings, errors and static analysis messages;
 5. <kbd>⌘</kbd>+<kbd>5</kbd> - Tests available in the project;
 6. <kbd>⌘</kbd>+<kbd>6</kbd> - Debug session panel;
 7. <kbd>⌘</kbd>+<kbd>7</kbd> - Breakpoints;
 8. <kbd>⌘</kbd>+<kbd>8</kbd> - Report/action history navigator;
 9. <kbd>⌘</kbd>+<kbd>0</kbd> - Show/Hide the navigator panel;

You can switch between different panels in the inspector area by holding <kbd>⌘</kbd> and <kbd>⌥</kbd> and pressing different digit keys from 1 to 6 (depending on the currently active editor: whether it's code, storyboard, xib or other type of resource). Pressing <kbd>0</kbd> while holding these two buttons will hide the inspector area.

So if you are editing a storyboard and need more visible space just tap <kbd>⌘</kbd> + <kbd>0</kbd> and <kbd>⌘</kbd>+<kbd>⌥</kbd>+<kbd>0</kbd> to get extra pixels for the canvas.

While switching panels on either side mostly depend on the combination of <kbd>⌘</kbd> or <kbd>⌘</kbd> and <kbd>⌥</kbd>, the bottom search fields are activated by holding <kbd>⌘</kbd> and <kbd>⌥</kbd> and pressing either <kbd>j</kbd> for navigation area search bar or <kbd>k</kbd> for library area search bar.

Activating the navigation search area can help you narrow the list of items displayed in navigator area depending on which navigator is active (filter files in the file navigator, simbols in the symbol navigator, tests in test navigator, etc).

Activating the inspector panel will help you filter the list of either file templates, code snippets, objects or media resources. Try using this search field when you have storyboard open and you quickly need to find a `UINavigationItem` or `UITableViewCell` components!

Speaking of library, you can switch between library panels (File templates, code snippets, object and media libraries) <kbd>⌃</kbd>+<kbd>⌘</kbd>+<kbd>⌥</kbd> and respective digit keys: 1 through 4.

