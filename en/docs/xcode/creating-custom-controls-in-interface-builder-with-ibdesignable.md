---
title: "Creating Custom Controls in Interface Builder with @IBDesignable"
slug: "creating-custom-controls-in-interface-builder-with-ibdesignable"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

It became much easier to create custom controls in Interface Builder with the introduction of the `@IBDesignable` and `@IBInspectable` directives in Swift. Developers can now build rich, complex, fully animated controls using just a few extra lines of code. I'm surprised by how many developers have yet to fully embrace this feature, and I frequently find that adding just a few of lines of code to existing classes can make them so much easier to work with.

*Note that these features are also available in Objective-C and are a great way of breathing life into old classes. The syntactic equivalents in Objective-C are IB_DESIGNABLE and IBInspectable, but for now I'll be concentrating on examples in Swift.*

## A Live-Rendered Rounded View
This is such a common requirement in iOS development, and it was always something that had to be done purely in code (or using images - yuck!). Now it's incredibly easy to preview his kind of thing in Interface Builder, there's absolutely no excuse for not using it.

Here's the code:-

    import UIKit
    
    @IBDesignable
    class MyRoundedView: UIView {
    
        @IBInspectable var radius: CGFloat = 8 {
            didSet {
                self.layer.cornerRadius = radius
            }
        }
        
        override func awakeFromNib() {
            self.layer.cornerRadius = self.radius
            self.layer.masksToBounds = true
        }
    }

To use this class, add it to your project and then open the storyboard in IB and create a normal UIView of a decent size. Give it a background colour so you can see it, then navigate to the Identity Inspector in the right-hand Utilities panel and change the class type in the drop-down to `MyRoundedView`. 

[![Setting the class type to MyRoundedView][1]][1]

When you do this you should see a third label appear beneath "Class" and "Module" that says "Designables", and it should say "Updating" for a moment before changing to "Up to date". This means that Xcode has recompiled your code for `MyRoundedView` successfully.

Now you can open the Attributes Inspector and you should see (maybe after a short pause) a new section at the top of the pane with the heading "My Rounded View" and a new attribute labelled "Radius" with the value 8 (because that is the initial value we set in the code). This has appeared in the Attributes Inspector because we marked it as `@IBInspectable`.

You can now change this to another number and you should see the rounded view's corner radius update in real-time!

[![Updating the inspectable property][2]][2]


  [1]: http://i.stack.imgur.com/MXBYU.jpg
  [2]: http://i.stack.imgur.com/Sx0pz.jpg

