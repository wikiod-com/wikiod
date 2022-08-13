---
title: "UIViewController"
slug: "uiviewcontroller"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Subclassing


## Access the container view controller
When the view controller is presented within a tab bar controller, you can access the tab bar controller like this:

**Swift**

    let tabBarController = viewController.tabBarController

**Objective-C**

    UITabBarController *tabBarController = self.tabBarController;

When the view controller is part on an navigation stack, you can access the navigation controller like this:

**Swift**

    let navigationController = viewController.navigationController

**Objective-C**

    UINavigationController *navigationController = self.navigationController;

## Create an instance
**Swift**

    let viewController = UIViewController()

**Objective-C**

    UIViewController *viewController = [UIViewController new];

## Set the view programmatically
Swift

    class FooViewController: UIViewController {

      override func loadView() {
        view = FooView()
      }

    }

## Instantiate from a Storyboard
    
    UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:nil];
    
**With an Identifier**:

Give the scene a Storyboard ID within the identity inspector of the storyboard.
 
[![enter image description here][1]][1]
     
Instantiate in code:

    UIViewController *controller = [storyboard instantiateViewControllerWithIdentifier:@"myIdentifier"];

**Instantiate an initial viewcontroller**:

Within the storyboard select the view controller, then select the attribute inspector, check the "Is Initial View Controller" box.

[![enter image description here][2]][2]
     
    UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:nil];
    UIViewController *controller = [storyboard instantiateInitialViewController];


  [1]: http://i.stack.imgur.com/aZyXo.png
  [2]: http://i.stack.imgur.com/rc7Wi.png

## Adding/removing a child view controller
To add a child view controller:

    - (void)displayContentController:(UIViewController *)vc {
       [self addChildViewController:vc];
       vc.view.frame = self.view.frame;
       [self.view addSubview:vc.view];
       [vc didMoveToParentViewController:self];
    }

To remove a child view controller:

    - (void)hideContentController:(UIViewController *)vc {
       [vc willMoveToParentViewController:nil];
       [vc.view removeFromSuperview];
       [vc removeFromParentViewController];
    }



