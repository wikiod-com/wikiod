---
title: "ModelPresentationStyles"
slug: "modelpresentationstyles"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Modal Presentation styles are used when you are transitioning from one view controller to another. There are 2 ways of achieving this customization. One is through code and another through Interface Builder(using segues). This effect is achieved by setting`modalPresentationStyle` variable to an instance of `UIModalPresentationStyle` enum. `modalPresentationStyle` property is a class variable of `UIViewController` and is used to specify how a `ViewController` is presented on screen.

Always remember the following mention from Apple.
> In a horizontally compact environment, modal view controllers are always presented full-screen. In a horizontally regular environment, there are several different presentation options.

## Exploring ModalPresentationStyle using Interface Builder
This will be a very basic app which will illustrate different `ModalpresentationStyle` in iOS. According to documentation found [here][1], There are 9 different values for `UIModalPresentationStyle` which are as follows,

 1. `fullScreen`
 2. `pageSheet`
 3. `formSheet`
 4. `currentContext`
 5. `custom`
 6. `overFullScreen`
 7. `overCurrentContext`
 8. `popover`
 9. `none`

To setup a project, just create a normal iOS project and add 2 `ViewControllers`. Put a `UIButton` in you initial `ViewController` and connect it to 2nd `ViewController` via a `Target -> Action` mechanism. To distinguish both `ViewControllers`, set background property of `UIView` in `ViewController` some other color. If all goes well, your Interface Builder should look something this,
[![Initial Interface builder][2]][2] 

Make sure you build this project and run it on **iPad** (For details on why iPad, refer to Remarks section). Once you are done setting up your project, select the segue and go to the `attributes inspector`. You should be able to see something like this,
[![enter image description here][3]][3]

Set the kind property to `Present Modally`.

Now, we won't see all of the effects in this example as some of them requires little bit of code.

Let's start with `fullscreen`. This effect is selected by default when you select `Present Modally` in `Kind` tab. When you build and run, the 2nd `ViewController` would occupy the full screen of you iPad.

[![enter image description here][4]][4]

Next is `pageSheet`. You can select this option from `Presentation` tab. In this option, when device is in portrait mode, the 2nd `ViewController` is similar to full screen but in landscape mode, 2nd `ViewController` is much narrow the device width. Also, any content not covered by 2nd `ViewController` will be dimmed.

[![enter image description here][5]][5]

For `formSheet` style, the 2nd `ViewController` is placed in center of device and the size is smaller to that of device. Also when device is in landscape mode and keyboard is visible position of view is adjusted upwards to show the `ViewController`.

[![enter image description here][6]][6]

Last style which we are going to try is `popover`. To select this style, select `Present as Popover` in `Kind` tab. The 2nd `ViewController` is presented as a small popover(size can be set). The background content is dimmed. Any tap outside the popover would dismiss the popover. Your `Attributes Inspector` should look something like this,

[![enter image description here][7]][7]

`Anchor` is the UI element to which you want your popover arrow to point.
`Directions` are the directions you allow your popover `Anchor` to point in.

[![enter image description here][8]][8]

There more then these basic Modal Presentation Styles but they are little complicated to achieve and require some code. More details can be found in the Apple Documentation.


  [1]: https://developer.apple.com/reference/uikit/uimodalpresentationstyle
  [2]: https://i.stack.imgur.com/u3T2N.png
  [3]: https://i.stack.imgur.com/dkaNm.png
  [4]: https://i.stack.imgur.com/nImSM.png
  [5]: https://i.stack.imgur.com/guMIT.png
  [6]: https://i.stack.imgur.com/ZKu3t.png
  [7]: https://i.stack.imgur.com/XTOSj.png
  [8]: https://i.stack.imgur.com/5DKN1.png

