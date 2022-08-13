---
title: "UIButton"
slug: "uibutton"
draft: false
images: []
weight: 9911
type: docs
toc: true
---

[UIButton](https://developer.apple.com/reference/uikit/uibutton) : [UIControl](https://developer.apple.com/reference/uikit/uicontrol) intercepts touch events and sends an action message to a target object when it's tapped. You can set the title, image, and other appearance properties of a button. In addition, you can specify a different appearance for each button state.

# **Button Types**

 *A button’s type defines its basic appearance and behavior. After creating a button, you cannot change its type. The most commonly used button types are the Custom and System types, but use the other types when appropriate*

 * UIButtonTypeCustom 

       No button style.

 * UIButtonTypeSystem

       A system style button, such as those shown in navigation bars and toolbars.

 * UIButtonTypeDetailDisclosure

       A detail disclosure button.

 * UIButtonTypeInfoLight

       An information button that has a light background.

 * UIButtonTypeInfoDark

       An information button that has a dark background.

 * UIButtonTypeContactAdd

       A contact add button.


When creating a custom button—that is a button with the type custom—the frame of the button is set to (0, 0, 0, 0) initially. Before adding the button to your interface, you should update the frame to a more appropriate value.


## Attaching a Method to a Button
To add a method to a button, first create an action method:

**Objective-C**

    -(void) someButtonAction{
        NSLog(@"Button is tapped");
    
    }

**Swift**

    func someButtonAction() {
            print("Button is tapped")
        }

Now to add this action method to your button, you have to write following line of code:

**Objective C**

    [yourButtonInstance addTarget:self action:@selector(someButtonAction) forControlEvents:UIControlEventTouchUpInside];

**Swift**

    yourButtonInstance.addTarget(self, action: #selector(someButtonAction), forControlEvents: .touchUpInside)


For ControlEvents, all members of `ENUM` [UIControlEvents][1] are valid. 


  [1]: https://developer.apple.com/library/tvos/documentation/UIKit/Reference/UIControl_Class/#//apple_ref/c/tdef/UIControlEvents

## Creating a UIButton
UIButtons can be initialized in a frame:

**Swift**

    let button = UIButton(frame: CGRect(x: x, y: y, width: width, height: height)

**Objective C**

    UIButton *button = [[UIButton alloc] initWithFrame:CGRectMake(x, y, width, height)];

A specific type of UIButton can be created like this:

**Swift**

    let button = UIButton(type: .Custom) 

**Objective C**

    UIButton *button = [UIButton buttonWithType:UIButtonTypeCustom];

where `type` is a `UIButtonType`:

    enum UIButtonType : Int {
        case Custom
        case System
        case DetailDisclosure
        case InfoLight
        case InfoDark
        case ContactAdd
        static var RoundedRect: UIButtonType { get }
    }


## Set title
    
**Swift**

    button.setTitle(titleString, forState: controlState)

**Objective C**

    [button setTitle:(NSString *) forState:(UIControlState)];

To set the default title to "Hello, World!"

**Swift**

    button.setTitle("Hello, World!", forState: .normal)

**Objective C**

    [button setTitle:@"Hello, World!" forControlState:UIControlStateNormal];

## Set title color
    //Swift
    button.setTitleColor(color, forControlState: controlState)

    //Objective-C
    [button setTitleColor:(nullable UIColor *) forState:(UIControlState)];

To set the title color to blue

    //Swift
    button.setTitleColor(.blue, for: .normal)

    //Objective-C
    [button setTitleColor:[UIColor blueColor] forState:UIControlStateNormal]

## Horizontally aligning contents

**Swift**

    //Align contents to the left of the frame
    button.contentHorizontalAlignment = .left

    //Align contents to the right of the frame
    button.contentHorizontalAlignment = .right

    //Align contents to the center of the frame
    button.contentHorizontalAlignment = .center

    //Make contents fill the frame
    button.contentHorizontalAlignment = .fill

**Objective C**

    //Align contents to the left
    button.contentHorizontalAlignment = UIControlContentHorizontalAlignmentLeft;

    //Align contents to the right
    button.contentHorizontalAlignment = UIControlContentHorizontalAlignmentRight;

    //Align contents to the center
    button.contentHorizontalAlignment = UIControlContentHorizontalAlignmentCenter;

    //Align contents to fill the frame
    button.contentHorizontalAlignment = UIControlContentHorizontalAlignmentFill;

## Getting the title label
The underlying title label, if one exists, can be fetched using

**Swift**

    var label: UILabel? = button.titleLabel

**Objective C**

    UILabel *label = button.titleLabel;

This can be used to set the font of the title label, for example

**Swift**

    button.titleLabel?.font = UIFont.boldSystemFontOfSize(12)

**Objective C**

    button.titleLabel.font = [UIFont boldSystemFontOfSize:12];

## Disabling a UIButton
A button can be disabled by

**Swift**

    myButton.isEnabled = false

**Objective-C:**

    myButton.enabled = NO;

The button will become gray:

[![enter image description here][1]][1]

If you don't want the button appearance to change when disabled set `adjustsImageWhenDisabled` to `false` / `NO`

  [1]: http://i.stack.imgur.com/uuJPz.png

## Adding an action to an UIButton via Code (programmatically)
To add a method to a button, first create an action method:
 
 **Objective-C**
 

    -(void)someButtonAction:(id)sender {
      // sender is the object that was tapped, in this case its the button.
        NSLog(@"Button is tapped"); 
    }

 
 **Swift**
 

    func someButtonAction() {
        print("Button is tapped")
    }

 
 Now to add this action method to your button, you have to write following line of code:
 
 **Objective C**
 

    [yourButtonInstance addTarget:self action:@selector(someButtonAction) forControlEvents:UIControlEventTouchUpInside];

 
 **Swift**
 

    yourButtonInstance.addTarget(self, action: #selector(someButtonAction), forControlEvents: .TouchUpInside)

 
 
 For ControlEvents parameter, all members of `ENUM` [UIControlEvents][1] are valid.
 
 
 [1]: https://developer.apple.com/library/tvos/documentation/UIKit/Reference/UIControl_Class/#//apple_ref/c/tdef/UIControlEvents

## Setting Font
**Swift**

    myButton.titleLabel?.font =  UIFont(name: "YourFontName", size: 20)

**Objective C**

    myButton.titleLabel.font = [UIFont fontWithName:@"YourFontName" size:20];

## Get UIButton's size strictly based on its text and font
To get the the exact size of a UIButton's text based on its font, use the function `intrinsicContentSize`.

**Swift**

    button.intrinsicContentSize.width

**Objective-C**

    button.intrinsicContentSize.width;



## Set Image
## Swift

    button.setImage(UIImage(named:"test-image"), forState: .normal)

## Objective C

    [self.button setImage:[UIImage imageNamed:@"test-image"] forState:UIControlStateNormal];



----------

<h1>Multiple Control States</h1>

You can also set an image for multiple `UIControlStates`, for example to set the same image for the `Selected` and `Highlighted` state:

## Swift

    button.setImage(UIImage(named:"test-image"), forState:[.selected, .highlighted])


## Objective C

    [self.button setImage:[UIImage imageNamed:@"test-image"] forState:UIControlStateSelected|UIControlStateHighlighted];

