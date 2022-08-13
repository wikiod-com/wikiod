---
title: "Getting started with Xcode"
slug: "getting-started-with-xcode"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Use multiple versions of Xcode
You can have multiple versions of Xcode installed at the same time (including beta versions). Simply rename the application in Finder to avoid conflicts.

<img src="http://i.stack.imgur.com/FcEoU.png" width="232" alt="renaming Xcode in Finder">

**Note:** Installing Xcode from the App Store will tend to **overwrite** an existing version on your machine. You can also install Xcode from a [direct download](https://developer.apple.com/downloads/) to get more control over which versions you have.

Each copy of Xcode includes command line tools (`clang`, `xcodebuild`, etc.). You can choose which ones are invoked by the commands in `/usr/bin`.

In Xcode's preferences, under the Locations tab, choose a version of Xcode:

[![Locations preferences][1]][1]

Or you can manage versions from the command line using [`xcode-select`](https://developer.apple.com/library/mac/documentation/Darwin/Reference/ManPages/man1/xcode-select.1.html):

<!-- language: lang-bash -->
<pre><code># Print the currently selected version
$ <b>xcode-select --print-path</b>
/Applications/Xcode.app/Contents/Developer

$ <b>clang --version</b>
Apple LLVM version 7.3.0 (clang-703.0.29)
Target: x86_64-apple-darwin15.4.0
Thread model: posix
InstalledDir: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin

# Find all installed versions using Spotlight
$ <b>mdfind 'kMDItemCFBundleIdentifier = "com.apple.dt.Xcode"'</b>
/Applications/Xcode.app
/Applications/Xcode72.app

# Check their version numbers
$ <b>mdfind 'kMDItemCFBundleIdentifier = "com.apple.dt.Xcode"' | xargs mdls -name kMDItemVersion</b>
kMDItemVersion = "7.3"
kMDItemVersion = "7.2.1"

# Switch to a different version
$ <b>sudo xcode-select --switch /Applications/Xcode72.app</b>

$ <b>clang --version</b>
Apple LLVM version 7.0.2 (clang-700.1.81)
Target: x86_64-apple-darwin15.4.0
Thread model: posix</code></pre>
  [1]: http://i.stack.imgur.com/ls5PC.png

## Changing The Color Scheme
Many developers like to customize the font, text, and background color of their IDE's.  You can do this in Xcode by opening the app preference pane, either by going to XCODE->Preferences, or by pressing '⌘,'

  [![Getting to Preferences][1]][1]

With the preference pane open you can click on the 'Fonts and Colors' tab.

[![Editing colors and fonts][2]][2]

From here you can change the source AND console background and font colors. There are many pre-made color and font schemes provided with Xcode.  You choose these from the list on the left (Basic, Chalkboard, etc).  You can find and download more online (like [here][3] for example).

To further customize any theme, you can customize any of the types listed in the right pane (Plain Text, Comments, Documentation Markup, etc).  For example, say I really want my 'Numbers' to show up in my code.  So I change the font to 'American Typewriter' at 24 px, the color to a greenish color, and set the line highlighting to red:

[![Bold Numbers, Yay!][4]][4] 

Now in my text editing, I can really see my numbers:

[![Cool colors in action!][5]][5]

Now you can customize the look and feel of the 'Source Editor' and 'Console' to your hearts delight!

Pro Tip
-------
Many developers like to theme their IDS dark (light text, dark background).  In Xcode, you can only do this for the 'Source Editor' and the 'Console'.  However, the Navigation (left side), Debug (bottom), and Utility (far right) sections are non-customizable.  There are two work arounds to this.  First (kind of tricky, is to leave the IDE light themed (Light background, dark text) then invert the screen colors all together.  This will make everything dark, but colors in the simulator and in the rest of the system are now wonky.  The second work around is to hide The Navigation, Debug, and Utility areas when not in use.  You can toggle these areas quickly using the following commands:

Navigator : ⌘0

Debug Area : ⇧⌘Y

Utility : ⌥⌘0



  [1]: http://i.stack.imgur.com/oBOn5.png
  [2]: http://i.stack.imgur.com/g1xI9.png
  [3]: https://github.com/hdoria/xcode-themes
  [4]: http://i.stack.imgur.com/qR3ap.png
  [5]: http://i.stack.imgur.com/WXNjG.png

## Get Started
- [Download Xcode](https://developer.apple.com/xcode/download/) from the Mac App Store.

- Click to create a new project or playground:
![Xcode welcome window](http://i.stack.imgur.com/yBkHu.png)

