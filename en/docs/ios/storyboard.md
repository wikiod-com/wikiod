---
title: "Storyboard"
slug: "storyboard"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

Normally, view controllers in a storyboard are instantiated and created automatically in response to actions defined within the storyboard itself. However, you can use a storyboard object to instantiate the initial view controller in a storyboard file or instantiate other view controllers that you want to present programmatically. Below you will find examples of both use cases.

## Initialize
    //Swift    
    let storyboard = UIStoryboard(name: "Main", bundle: NSBundle.mainBundle()) 

    //Objective-c
    UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:[NSBundle mainBundle]];
    

## Fetch Initial ViewController 
    //Swift
    let initialScreen = storyboard.instantiateInitialViewController()
    
    //Objective-c
    UIViewController *initailScreen = [storyboard instantiateInitialViewController];

## Fetch ViewController
    //Swift    
    let viewController = storyboard.instantiateViewControllerWithIdentifier("identifier")

    //Objective-c
    UIViewController *viewController = [storyboard instantiateViewControllerWithIdentifier:@"identifier"];

