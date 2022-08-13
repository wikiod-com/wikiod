---
title: "UITabBarController"
slug: "uitabbarcontroller"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Create an instance
A 'tab bar' is commonly found in most iOS apps and is used to present distinct views in each tab. 

To create a tab bar controller using the interface builder, drag a tab bar Controller from the Object Library into the canvas.

[![tab bar controller][1]][1]

By default a tab bar controller comes with two views. To add additional views, control drag from the tab bar controller to the new view and select 'view controllers' in the segue-drop down. 

[![tab bar with views][2]][2]


  [1]: http://i.stack.imgur.com/IEtwo.png
  [2]: http://i.stack.imgur.com/u1dHd.png

## Navigation Controller with TabBar
Navigation controller can be embed in each tabs using storyboard it self. It can be like in the screenshot added. 

To add a Navigation Controller to a View Controller connecting from Tab Bar Controller, here are the flow

 - Select the view controller for which we need to add navigation controller. Here let it be the Search View Controller as the selection display.
 - From the **Editor** menu of the Xcode, select **Embed In -> Navigation Controller** option 

  [![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/QKRCZ.png

## Changing Tab Bar Item Title and Icon

**Using the Story Board:**

Select the tab bar item from the corresponding view controller and go to the attributes inspector

If you want a built-in icon and title, set the 'System  Item' to the corresponding value.

For a custom icon, add the required images to the assets folder and set the 'System Item' from earlier to 'custom'.

Now, set the icon to be shown when the tab is selected from the 'selected image' drop down and the default tab icon from the 'image' drop down. Add the corresponding title in the 'title' field.

[![Tab Bar Item Attributes Inspector][1]][1]

**Programmatically:**

In the `viewDidLoad()` method of the view controller, add the following code: 

## Objective-C:

    self.title = @"item";

    self.tabBarItem.image = [UIImage imageNamed:@"item"];
    self.tabBarItem.selectedImage = [UIImage imageNamed:@"item_selected"];


## Swift:

    self.title = "item"
    self.tabBarItem.image = UIImage(named: "item")
    self.tabBarItem.selectedImage = UIImage(named: "item_selected")


  [1]: http://i.stack.imgur.com/tzPs8.png

## Tab Bar color customizing
    [[UITabBar appearance] setTintColor:[UIColor whiteColor]];
    [[UITabBar appearance] setBarTintColor:[UIColor tabBarBackgroundColor]];
    [[UITabBar appearance] setBackgroundColor:[UIColor tabBarInactiveColor]];
    [[UINavigationBar appearance] setBarTintColor:[UIColor appBlueColor]];
    [[UINavigationBar appearance] setTintColor:[UIColor whiteColor]];
    [[UINavigationBar appearance] setBarStyle:UIBarStyleBlack];
    


## UITabBarController with custom color selection
UITabBarController building in `Swift 3` Change image color and title according to selection with changing selected tab color.

    import UIKit
    
    class TabbarController: UITabBarController {
    
        override func viewDidLoad() {
            super.viewDidLoad()
    
            self.navigationController?.isNavigationBarHidden = true
            
            UITabBar.appearance().tintColor = UIColor.purple
            
            // set red as selected background color
            let numberOfItems = CGFloat(tabBar.items!.count)
            let tabBarItemSize = CGSize(width: tabBar.frame.width / numberOfItems, height: tabBar.frame.height)
            tabBar.selectionIndicatorImage = UIImage.imageWithColor(UIColor.lightText.withAlphaComponent(0.5), size: tabBarItemSize).resizableImage(withCapInsets: UIEdgeInsets.zero)
            
            // remove default border
            tabBar.frame.size.width = self.view.frame.width + 4
            tabBar.frame.origin.x = -2
            
        }
        
        override func viewWillAppear(_ animated: Bool) {
            // For Images
            let firstViewController:UIViewController = NotificationVC()
            // The following statement is what you need
            let customTabBarItem:UITabBarItem = UITabBarItem(title: nil, image: UIImage(named: "notification@2x")?.withRenderingMode(UIImageRenderingMode.alwaysOriginal), selectedImage: UIImage(named: "notification_sel@2x"))
            firstViewController.tabBarItem = customTabBarItem
            
            for item in self.tabBar.items! {
                let unselectedItem = [NSForegroundColorAttributeName: UIColor.white]
                let selectedItem = [NSForegroundColorAttributeName: UIColor.purple]
                
                item.setTitleTextAttributes(unselectedItem, for: .normal)
                item.setTitleTextAttributes(selectedItem, for: .selected)
            }
        }
    
    }
    
    
    extension UIImage {
        class func imageWithColor(_ color: UIColor, size: CGSize) -> UIImage {
            let rect: CGRect = CGRect(origin: CGPoint(x: 0,y :0), size: CGSize(width: size.width, height: size.height))
            UIGraphicsBeginImageContextWithOptions(size, false, 0)
            color.setFill()
            UIRectFill(rect)
            let image: UIImage = UIGraphicsGetImageFromCurrentImageContext()!
            UIGraphicsEndImageContext()
            return image
        }
        
    }

# Choosing image for tab bar and set the tab title here

[![create custom tab][1]][1]

[![Selection of tab][2]][2]

# Selection another tab

[![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/bLLV5.png
  [2]: https://i.stack.imgur.com/ybNLF.png
  [3]: https://i.stack.imgur.com/RCSf7.png

## Create Tab Bar controller programmatically without Storyboard
    class AppDelegate: UIResponder, UIApplicationDelegate {
    
        var window: UIWindow?
        
        var firstTabNavigationController : UINavigationController!
        var secondTabNavigationControoller : UINavigationController!
        var thirdTabNavigationController : UINavigationController!
        var fourthTabNavigationControoller : UINavigationController!
        var fifthTabNavigationController : UINavigationController!
        
    
    
        func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]?) -> Bool {
            // Override point for customization after application launch.
            Fabric.with([Crashlytics.self])
            
           window = UIWindow(frame: UIScreen.main.bounds)
            
            
            window?.backgroundColor = UIColor.black
                
            
            let tabBarController = UITabBarController()
            
            firstTabNavigationController = UINavigationController.init(rootViewController: FirstViewController())
            secondTabNavigationControoller = UINavigationController.init(rootViewController: SecondViewController())
            thirdTabNavigationController = UINavigationController.init(rootViewController: ThirdViewController())
            fourthTabNavigationControoller = UINavigationController.init(rootViewController: FourthViewController())
            fifthTabNavigationController = UINavigationController.init(rootViewController: FifthViewController())
            
            tabBarController.viewControllers = [firstTabNavigationController, secondTabNavigationControoller, thirdTabNavigationController, fourthTabNavigationControoller, fifthTabNavigationController]
            
            
            let item1 = UITabBarItem(title: "Home", image: UIImage(named: "ico-home"), tag: 0)
            let item2 = UITabBarItem(title: "Contest", image:  UIImage(named: "ico-contest"), tag: 1)
            let item3 = UITabBarItem(title: "Post a Picture", image:  UIImage(named: "ico-photo"), tag: 2)
            let item4 = UITabBarItem(title: "Prizes", image:  UIImage(named: "ico-prizes"), tag: 3)
            let item5 = UITabBarItem(title: "Profile", image:  UIImage(named: "ico-profile"), tag: 4)

            firstTabNavigationController.tabBarItem = item1
            secondTabNavigationControoller.tabBarItem = item2
            thirdTabNavigationController.tabBarItem = item3
            fourthTabNavigationControoller.tabBarItem = item4
            fifthTabNavigationController.tabBarItem = item5
                    
            UITabBar.appearance().tintColor = UIColor(red: 0/255.0, green: 146/255.0, blue: 248/255.0, alpha: 1.0)
            
            self.window?.rootViewController = tabBarController
            
            window?.makeKeyAndVisible()
 
            return true
        }



