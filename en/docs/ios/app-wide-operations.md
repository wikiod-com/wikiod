---
title: "App wide operations"
slug: "app-wide-operations"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Get the top most UIViewController
A common approach to get the top most `UIViewController` is to get the RootViewController of your active `UIWindow`. I wrote an extension for this:

    extension UIApplication {
    
    func topViewController(_ base: UIViewController? = UIApplication.shared.keyWindow?.rootViewController) -> UIViewController {
        
        if let nav = base as? UINavigationController {
            return topViewController(nav.visibleViewController)
        }
        
        if let tab = base as? UITabBarController {
            if let selected = tab.selectedViewController {
                return topViewController(selected)
            }
        }
        
        if let presented = base?.presentedViewController {
            return topViewController(presented)
        }
        
        return base!
    }

## Intercept System Events
Using the NotificationCenter of iOS, which can be very powerful, you are able to intercept certain app-wide events:

    NotificationCenter.default.addObserver(
            self,
            selector: #selector(ViewController.do(_:)),
            name: NSNotification.Name.UIApplicationDidBecomeActive,
            object: nil)

You can register for a lot of more events, just take a look at https://developer.apple.com/reference/foundation/nsnotification.name.

