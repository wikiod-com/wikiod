---
title: "NSMenuItem"
slug: "nsmenuitem"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

See Apple's Documentation here: https://developer.apple.com/library/mac/documentation/Cocoa/Reference/ApplicationKit/Classes/NSMenuItem_Class/

## Enabling menu items

# Manually enabling menu items  

To manually control the enabled state of a menu items the menu that contains it must disable automatic enabling of its items

Menus can turn off automatic enabling in one of two ways:

1. In the Interface Builder 

[![Interface Builder assistant editor screenshot showing Auto Enables Items disabled][1]][1]

2. In code

`menu.autoenablesItems = false`

Both of the mechanisms set the [autoenablesItems](https://developer.apple.com/reference/appkit/nsmenu/1518227-autoenablesitems) property on [NSMenu](https://developer.apple.com/reference/appkit/nsmenu).

Once the menu is menu is no longer enabling and disabling menu items the menu items can be programmatically set in one of two ways

1. In Interface Builder
2. In code

```swift
menuItem.enabled = true
```

# Automatically enabling menu items

Menu items can be automatically enabled by connecting menu items actions to the first responder and implementing the delivered action on an object in the responder chain as described by Apple's [docs](https://developer.apple.com/library/content/documentation/Cocoa/Conceptual/MenuList/Articles/EnablingMenuItems.html).

  [1]: http://i.stack.imgur.com/DsyJn.png

## Supporting default menu actions
Menus act like all standard control items. They have an [action](https://developer.apple.com/reference/appkit/nscontrol/1428956-action) which is the function to be called and a [target](https://developer.apple.com/reference/appkit/nscontrol/1428885-target) which is the object to send the function to. If the target is set to an object then when a user selects a menu item it the action method will be sent to the target object. If the menu item has an action, but not a target then the target will be dynamically selected from the first object from the following that responds to the action:

1. The first responder 
2. The view hierarchy 
3. Window 
4. Window controller
5. `NSApplication`
6. `NSApplication.delegate`
7. `NSApplication.nextResponder`

Implementing the default Open (⌘O) menu item can be accomplished by implementing the `openDocument` method on any object in the above list.

```Objective-C

- (IBAction)openDocument:(id)sender {

}
```

## Adding and removing items to a menu


```swift
// add an item to a menu 
menu.addItem(item)

// remove and item from a menu
menu.removeItem(item)
```


