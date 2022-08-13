---
title: "UIAppearance"
slug: "uiappearance"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## Set appearance of all instances of the class
To customize appearance of all instances of a class, access appearance proxy of the desired class. For example:

**Set UIButton tint color**

*Swift:*

    UIButton.appearance().tintColor = UIColor.greenColor()

*Objective-C:*

    [UIButton appearance].tintColor = [UIColor greenColor];

**Set UIButton background color**

*Swift:*

    UIButton.appearance().backgroundColor = UIColor.blueColor()

*Objective-C:*

    [UIButton appearance].backgroundColor = [UIColor blueColor];

**Set UILabel text color**

*Swift:*

    UILabel.appearance().textColor = UIColor.redColor()

*Objective-C:*

    [UILabel appearance].textColor = [UIColor redColor];

**Set UILabel background color**

*Swift:*

    UILabel.appearance().backgroundColor = UIColor.greenColor()

*Objective-C:*

    [UILabel appearance].backgroundColor = [UIColor greenColor];

**Set UINavigationBar tint color**

*Swift:*

    UINavigationBar.appearance().tintColor = UIColor.cyanColor()

*Objective-C:*

    [UINavigationBar appearance].tintColor = [UIColor cyanColor];

**Set UINavigationBar background color**

*Swift:*

    UINavigationBar.appearance().backgroundColor = UIColor.redColor()

*Objective-C:*

    [UINavigationBar appearance].backgroundColor = [UIColor redColor];

        











## Appearance for class when contained in container class
Use `appearanceWhenContainedInInstancesOfClasses:` to customize the appearance for instance of a class when contained within an instance of container class. For example customization of `UILabel `'s `textColor ` and `backgroundColor ` within `ViewController` class will look like this:

**Set UILabel text color**

*Swift:*
    
    UILabel.appearanceWhenContainedInInstancesOfClasses([ViewController.self]).textColor = UIColor.whiteColor()

*Objective-C:*

    [UILabel appearanceWhenContainedInInstancesOfClasses:@[[ViewController class]]].textColor = [UIColor whiteColor];

**Set UILabel background color**

*Swift:*

    UILabel.appearanceWhenContainedInInstancesOfClasses([ViewController.self]).backgroundColor = UIColor.blueColor()

*Objective-C:*

    [UILabel appearanceWhenContainedInInstancesOfClasses:@[[ViewController class]]].backgroundColor = [UIColor blueColor];
    
       

