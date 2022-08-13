---
title: "UISwitch"
slug: "uiswitch"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

## Syntax
- (instancetype)initWithFrame:(CGRect)frame;
- (void)setOn:(BOOL)on animated:(BOOL)animated;
- (nullable instancetype)initWithCoder:(NSCoder *)aDecoder;

<h2> 1. UISwitch Reference : [Apple Documentation][1] </h2>

[![enter image description here][2]][2]

<h2> 2. Another Reference given by : [Enoch Huang][3] </h2>

[![enter image description here][4]][4]


  [1]: https://developer.apple.com/library/ios/documentation/UIKit/Reference/UISwitch_Class/index.html#//apple_ref/occ/cl/UISwitch
  [2]: http://i.stack.imgur.com/ZqTs0.png
  [3]: http://studyswift.blogspot.in/2016/05/create-uiswitch-programmatically.html
  [4]: http://i.stack.imgur.com/zYobJ.gif

## Set On / Off
**Objective-C**

    [mySwitch setOn:YES];
    //or
    [mySwitch setOn:YES animated:YES];

**Swift**

    mySwitch.setOn(false)
    //or
    mySwitch.setOn(false, animated: false)

## Set Image for On/Off state
**Objective-C**

    //set off-image
    mySwitch.offImage = [UIImage imageNamed:@"off_image"];
    [mySwitch setOffImage:[UIImage imageNamed:@"off_image"]];
    
    //set on-image
    mySwitch.onImage = [UIImage imageNamed:@"on_image"];
    [mySwitch setOnImage:[UIImage imageNamed:@"on_image"]];

**Swift**

    //set off-image
    mySwitch.offImage = UIImage(named: "off_image")
    
    //set on-image
    mySwitch.onImage = UIImage(named: "on_image")

## Set Background Color
**Objective-C**

    mySwitch.backgroundColor = [UIColor yellowColor];
    [mySwitch setBackgroundColor: [UIColor yellowColor]];
    mySwitch.backgroundColor =[UIColor colorWithRed:255/255.0 green:0/255.0 blue:0/255.0 alpha:1.0];
    mySwitch.backgroundColor= [UIColor colorWithWhite: 0.5 alpha: 1.0];
    mySwitch.backgroundColor=[UIColor colorWithHue: 0.4 saturation: 0.3 brightness:0.7 alpha: 1.0];

**Swift**

    mySwitch.backgroundColor = UIColor.yellow
    mySwitch.backgroundColor = UIColor(red: 255.0/255, green: 0.0/255, blue: 0.0/255, alpha: 1.0)
    mySwitch.backgroundColor = UIColor(white: 0.5, alpha: 1.0)
    mySwitch.backgroundColor = UIColor(hue: 0.4,saturation: 0.3,brightness: 0.7,alpha: 1.0)

## Set Tint Color
**Objective-C**

    //for off-state
    mySwitch.tintColor = [UIColor blueColor];
    [mySwitch setTintColor: [UIColor blueColor]];

    //for on-state
    mySwitch.onTintColor = [UIColor cyanColor];
    [mySwitch setOnTintColor: [UIColor cyanColor]];

**Swift**

    //for off-state
    mySwitch.tintColor = UIColor.blueColor()
    
    //for on-state
    mySwitch.onTintColor = UIColor.cyanColor()

