---
title: "SWRevealViewController"
slug: "swrevealviewcontroller"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

Using the SWRevealViewController class as the main navigation might not always result in the best user experience. If the sidebar contains only 5 or less entries (or the content can be compressed into 5 or less entries), you should consider using the default tab bar.

The tab bar is intuitive and allows the user to quickly change between views/contexts. On the other hand, the sidebar navigation can perform more actions than switching the view/context and uses less space when collapsed.

For more information check out Apple's [iOS Human Interface Guidelines](https://developer.apple.com/ios/human-interface-guidelines/).

## Setting up a basic app with SWRevealViewController
Create a basic application with single view application template with swift as language

Add `SWRevealViewController.h` and `SWRevealViewController.m` 

then click on Create Bridging Header button 

[![enter image description here][1]][1]

and add 

    #import "SWRevealViewController.h"

on the Bridging header

Then select viewController on storyboard and change class to `SWRevealViewController`

[![enter image description here][2]][2]

Then rename the viewController on files to MainViewController and add new ViewController with RightViewController name

[![enter image description here][3]][3]

then we add two segues from SWRevealViewController to MainViewController and from SWRevealViewController to RightViewController, then we need to select the first (from SWRevealViewController to MainViewController) and edit properties 

on identifier set `sw_front`
on Class set `SWRevealViewControllerSegueSetController`

[![enter image description here][4]][4]

after this we need to do the same with the segue (from SWRevealViewController to RightViewController)

on identifier set `sw_rear`
on Class set `SWRevealViewControllerSegueSetController`

[![enter image description here][5]][5]

then on MainViewController add this line on `viewDidLoad` method

    self.view.addGestureRecognizer(self.revealViewController().panGestureRecognizer());

And this is all, you have a basic app with SWRevealViewController integrated, you can swipe to right to show `RightViewController` as lateral menu

  [1]: http://i.stack.imgur.com/nqQZM.png
  [2]: http://i.stack.imgur.com/vP10G.png
  [3]: http://i.stack.imgur.com/4wiPd.png
  [4]: http://i.stack.imgur.com/019Uz.png
  [5]: http://i.stack.imgur.com/lBSc6.png

