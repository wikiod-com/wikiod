---
title: "Change Status Bar Color"
slug: "change-status-bar-color"
draft: false
images: []
weight: 9746
type: docs
toc: true
---

## For non-UINavigationBar status bars
1. In info.plist set `View controller-based status bar appearance` to `YES`
2. In view controllers not contained by `UINavigationController` implement this method.

**In Objective-C:**

    - (UIStatusBarStyle)preferredStatusBarStyle
    { 
        return UIStatusBarStyleLightContent; 
    }

**In Swift:**

    override func preferredStatusBarStyle() -> UIStatusBarStyle {
        return UIStatusBarStyle.LightContent
    }

## For UINavigationBar status bars
Subclass UINavigationController and then override these methods:

In Objective-C:

    - (UIStatusBarStyle)preferredStatusBarStyle
    { 
        return UIStatusBarStyleLightContent; 
    }

In Swift:

    override func preferredStatusBarStyle() -> UIStatusBarStyle {
        return .lightContent
    }


Alternatively, you can set `barStyle` on the `UINavigationBar` instance:

Objective C:
   
    // e.g. in your view controller's viewDidLoad method:
    self.navigationController.navigationBar.barStyle = UIBarStyleBlack;  // this will give you a white status bar

Swift

    // e.g. in your view controller's viewDidLoad method:
    navigationController?.navigationBar.barStyle = .black // this will give you a white status bar

`UIBarStyle` options are `default`, `black`, `blackOpaque`, `blackTranslucent`. The latter 3 should all give you a status bar with white text, just the last two specify the opacity of the bar.

Note: you can still change the appearance of your navigation bar as you like.

## If you cannot change ViewController's code
If you are using library that contains (for example) AwesomeViewController with a wrong status bar color you can try this:

      let awesomeViewController = AwesomeViewController()
      awesomeViewController.navigationBar.barStyle = .blackTranslucent // or other style


## For ViewController containment
If you are using `UIViewControllerContainment` there are a few other methods that are worth looking at.

When you want a child viewController to control the presentation of the status bar (i.e. if the child is positioned at the top of the screen

in Swift

    class RootViewController: UIViewController {

        private let messageBarViewController = MessageBarViewController()        

        override func childViewControllerForStatusBarStyle() -> UIViewController? {
            return messageBarViewController
        }

        override func viewDidLoad() {
            super.viewDidLoad()
            
            //add child vc code here...

            setNeedsStatusBarAppearanceUpdate()
        }
    }
    
    class MessageBarViewController: UIViewController {
        
        override func preferredStatusBarStyle() -> UIStatusBarStyle {
            return .Default 
        }
    }

  

## Changing the status bar style for the entire application
<br>**SWIFT:**
-----
<br>

Step 1:
---

In your **Info.plist** add the following attribute:

    View controller-based status bar appearance

and set its value to

    NO

as described in the image below: 

[![enter image description here][1]][1]<br><br>

Step 2:
--

In your **AppDelegate.swift** file, in `didFinishLaunchingWithOptions` method, add this code:

    UIApplication.shared.statusBarStyle = .lightContent

**or**

    UIApplication.shared.statusBarStyle = .default

 - The **.lightContent** option will set the colour of the **statusBar**
   to white, for the entire app.
   
 - The **.default** option will set the colour of the **statusBar** to
   the original black colour, for the entire app.<br><br>


----------


**OBJECTIVE-C:**
-----

<br> Follow the first step from the **SWIFT** Section. 
Then add this code to the **AppDelegate.m** file:

    [[UIApplication sharedApplication] setStatusBarStyle:UIStatusBarStyleLightContent];

**or**

    [[UIApplication sharedApplication] setStatusBarStyle:UIStatusBarStyleDefault];

  [1]: https://i.stack.imgur.com/4EF5C.png

