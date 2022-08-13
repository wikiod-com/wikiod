---
title: "UINavigationController"
slug: "uinavigationcontroller"
draft: false
images: []
weight: 9935
type: docs
toc: true
---

From the [documentation][1]: 
>The UINavigationController class implements a specialized view controller that manages the navigation of hierarchical content. This navigation interface makes it possible to present your data efficiently and makes it easier for the user to navigate that content. You generally use this class as-is but you may also subclass to customize the class behavior.


  [1]: https://developer.apple.com/library/ios/documentation/UIKit/Reference/UINavigationController_Class/

## Popping in a Navigation Controller


# To previous view controller

To pop back to the previous page you can do this:

Swift

    navigationController?.popViewControllerAnimated(true)

Objective-C

    [self.navigationController popViewControllerAnimated:YES];

---
# To root view controller

To pop to the root of the navigation stack, you can do this:

Swift

    navigationController?.popToRootViewControllerAnimated(true)

Objective C

    [self.navigationController popToRootViewControllerAnimated:YES];

## Embed a view controller in a navigation controller programmatically
Swift

    //Swift
    let viewController = UIViewController()
    let navigationController = UINavigationController(rootViewController: viewController)

    //Objective-C
    UIViewController *viewController = [[UIViewController alloc] init];
    UINavigationController *navigationController = [[UINavigationController alloc] initWithRootViewController:viewController];
    

## Purpose
`UINavigationController` is used to form a tree-like hierarchy of view controllers, which is known as a `navigation stack`.


**From developers perspective:** 

You can connect independently made controller and get all the benefits of a free hierarchy manager and common UI presenter gratis. `UINavigationController` animates the transition to new controllers and provides the back functionality for you automatically. `UINavigationController`also gives access to all the other controllers in the `navigation stack` which can help access to some functionality or data. 

**From user's perspective:**

`UINavigationController` helps to remember where user is at the moment (navigation bar title) and how he can go back (embedded back button) to one of the previous screens.

## Creating a NavigationController
In your storyboard select the ViewController that you want to embed into a Navigation Controller.

Then navigate to Editor > Embed In > Navigation Controller

[![enter image description here][1]][1]

And that will create your navigation controller

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/JKS8j.png
  [2]: http://i.stack.imgur.com/0zk9n.png

## Pushing a view controller onto the navigation stack
    //Swift
    let fooViewController = UIViewController()
    navigationController?.pushViewController(fooViewController, animated: true)

    //Objective-C
    UIViewController *fooViewController = [[UIViewController alloc] init];
    [navigationController pushViewController:fooViewController animated:YES];

