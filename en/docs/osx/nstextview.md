---
title: "NSTextView"
slug: "nstextview"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

NSTextView is Apple's main handler of AppKit's text system. It contains everything you need to create a text viewer/editor space in OS X (renamed macOS) applications.

## Creating an NSTextView
Graphically
===========
In XCode a simple NSTextView can be created by dragging and dropping one from the Object Library.

[![Dragging an NSTextView object from the Object Library to a window in Interface Builder.][1]][1]


This NSTextView sits inside an **NSScrollView** that is automatically set to expand vertically with the text view. Make sure when option(⌥)-dragging you make connections to the text view and not the scroll view.

[![Showing contents of NSScrollView to show hierarchy location of NSTextView.][2]][2]

Programmatically
================
Creating an NSTextView programmatically allows for greater control and customization. It is slightly more difficult and requires knowledge of the text system to use it to its full potential. More info about the text system and more is available [here][3]. The basics required to make a text view programmatically are as follows:

 - Three other objects are required for a fully functioning NSTextView to work:
    1. An NSLayoutManager - performs glyph/character layout.
    2. An NSTextContainer - controls graphical space that glyphs/characters can inhabit.
    3. An NSTextStorage - holds the actual string data that NSTextView displays.
    
 - An NSTextStorage can have many NSLayoutManager's, but an NSLayoutManager can only have one NSTextStorage. This is useful if you wish to show the same data in different ways at the same time.

 - NSLayoutManager's can have many NSTextContainer's. Useful for paginated text.

 - NSTextView's can only have one NSTextContainer at a time.

 - Certain things built into NSTextView are off limits at the time of writing. For example built-in Find-and-Replace functions are not able to be customized, but can be overridden with custom functions.

More information on ways to use the text system can be found [here][4].

Now for the code. This code will create a simple NSTextView, with not even scrolling. Such things like scrolling and pagination will be in another example.
## Objective-C ##
    // This code resides in an NSDocument object's windowControllerDidLoadNib:(NSWindowController *)windowController method.
    // This is done simply because it is easy and automatically gets called upon.
    
    // This method is also where the following NSRect variable gets size information. We need this information for this example.
    NSRect windowFrame = windowController.window.contentView.frame;
    NSTextStorage *textStorage = [[NSTextStorage alloc] initWithString:@"Example text!"];
    NSLayoutManager *manager = [[NSLayoutManager alloc] init];
    NSTextContainer *container = [[NSTextContainer alloc] initWithContainerSize:NSMakeSize(windowFrame.size.width, windowFrame.size.height)];
    NSTextView *textView = [[NSTextView alloc] initWithFrame:windowFrame textContainer:container];
    
    [textStorage addLayoutManager:manager];
    [manager addTextContainer:container];
    [windowController.window setContentView:textView];

Congratulations! You have made an NSTextView programmatically!

[![enter image description here][5]][5]


  [1]: https://i.stack.imgur.com/e11Rf.png
  [2]: https://i.stack.imgur.com/hhBpy.png
  [3]: https://developer.apple.com/library/content/documentation/TextFonts/Conceptual/CocoaTextArchitecture/Introduction/Introduction.html#//apple_ref/doc/uid/TP40009459 "About the Cocoa Text System"
  [4]: https://developer.apple.com/library/content/documentation/TextFonts/Conceptual/CocoaTextArchitecture/TextSystemArchitecture/ArchitectureOverview.html#//apple_ref/doc/uid/TP40009459-CH7-CJBJHGAG "Text System Organization"
  [5]: https://i.stack.imgur.com/yJI81.png

