---
title: "Size Classes and Adaptivity"
slug: "size-classes-and-adaptivity"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

For more details ( Size Classes and Adaptivity through Storyboard) of using auto layout for adaptivity in iOS, we can follow the [apple developer site link][1].

We can also add constraints **Programatically** using **Visual Format Language** as described [here at apple developer site][2].


  [1]: https://developer.apple.com/library/ios/documentation/UserExperience/Conceptual/AutolayoutPG/
  [2]: https://developer.apple.com/library/prerelease/content/documentation/UserExperience/Conceptual/AutolayoutPG/ProgrammaticallyCreatingConstraints.html

## Size Classes and Adaptivity through Storyboard
We can add adaptivity to any subclass of `UIView` which we add on view controller in nib file.  
Lets take an example of adding adaptivity using size classes to a view.

1. Add a view on view controller as:

[![enter image description here][1]][1]

2. Now we need to pin this view to it's superview for fixing it's size and position using **constraints** as:

[![enter image description here][2]][2]

3. We can see the added constraints as:

[![enter image description here][3]][3]

These constraints defines that the added view will be placed in it's superview as

    CGRect(20, 0, superview.width - 20, superview.height - 20)

4. To see the preview on screen of these added constraints we can use **Assistant Editor** as;

[![enter image description here][4]][4]


5. We can add more screen to see preview like:

[![enter image description here][5]][5]

We can also see the preview with landscape mode by moving mouse on the name of device and clicking the rotation button as:

[![enter image description here][6]][6]


  [1]: http://i.stack.imgur.com/Wvis9.png
  [2]: http://i.stack.imgur.com/ocSWe.png
  [3]: http://i.stack.imgur.com/ttiC2.png
  [4]: http://i.stack.imgur.com/l7W4L.png
  [5]: http://i.stack.imgur.com/vEbj2.png
  [6]: http://i.stack.imgur.com/XCRdo.png

