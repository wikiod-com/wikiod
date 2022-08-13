---
title: "UIFont"
slug: "uifont"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

[UIFont](https://developer.apple.com/reference/uikit/uifont) is a class that is used for getting and setting font-related information. It inherits from `NSObject` and conforms to `Hashable`, `Equatable`, `CVarArg` and `NSCopying`.

## Declaring and initializing UIFont
You can declare a `UIFont` as follows:

    var font: UIFont!

`UIFont` has more `init()` methods:

 - `UIFont.init(descriptor: UIFontDescriptor, size: CGFloat)`
 - `UIFont.init(name: String, size: CGFloat)` 

Therefore, you can initialize a `UIFont` like this:
     
    let font = UIFont(name: "Helvetica Neue", size: 15)
 

The default font is `System`, size `17`.

## Changing the font of a label
To change a label's text font, you need to access its `font` property:

    label.font = UIFont(name:"Helvetica Neue", size: 15)

The code above will change the font of the label to `Helvetica Neue`, size `15`. Beware that you must spell the font name correctly, otherwise it will throw this error, because the initialized value above is an Optional, and thus can be nil:

> Unexpectedly found nil while unwrapping an Optional value

