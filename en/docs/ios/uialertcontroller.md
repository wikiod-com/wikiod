---
title: "UIAlertController"
slug: "uialertcontroller"
draft: false
images: []
weight: 8957
type: docs
toc: true
---

> A `UIAlertController` object displays an alert message to the user.
> This class replaces the `UIActionSheet` and `UIAlertView` classes for
> displaying alerts. After configuring the alert controller with the
> actions and style you want, present it using the
> `presentViewController:animated:completion:` method.

From [Apple's Documentation][1]

[UIAlertController in Swift][2]


  [1]: https://developer.apple.com/library/ios/documentation/UIKit/Reference/UIAlertController_class/
  [2]: https://iosdevcenters.blogspot.com/2016/03/uialertcontroller-in-swift.html

## AlertViews with UIAlertController
`UIAlertView` and `UIActionSheet` are Deprecated in `iOS 8` and Later. So Apple introduced a new controller for `AlertView` and `ActionSheet` called `UIAlertController` , changing the `preferredStyle`, you can switch between `AlertView` and `ActionSheet`. There is no delegate method for it because all button events are handled in their blocks.

**Simple AlertView**

***Swift:***

    let alert = UIAlertController(title: "Simple", message: "Simple alertView demo with Cancel and OK.", preferredStyle: .alert)
    
    alert.addAction(UIAlertAction(title: "Cancel", style: .cancel) { _ in
            print("Cancel")
    })
    alert.addAction(UIAlertAction(title: "OK", style: .default) { _ in
            print("OK")
    })
    
    present(alert, animated: true)

***Objective-C:***

     UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Simple" message:@"Simple alertView demo with Cancel and OK." preferredStyle:UIAlertControllerStyleAlert];
        UIAlertAction *cancelAction = [UIAlertAction actionWithTitle:@"Cancel" style:UIAlertActionStyleCancel handler:^(UIAlertAction * action) {
            NSLog(@"Cancel");
        }];
        UIAlertAction *okAction = [UIAlertAction actionWithTitle:@"OK" style:UIAlertActionStyleDefault handler:^(UIAlertAction * action) {
            NSLog(@"OK");
        }];
        
        [alertController addAction:cancelAction];
        [alertController addAction:okAction];
        [self presentViewController:alertController animated: YES completion: nil];


[![enter image description here][1]][1]

**Destructive AlertView**

***Swift:***

    let alert = UIAlertController(title: "Simple", message: "Simple alertView demo with Cancel and OK.", preferredStyle: .alert)
    
    alert.addAction(UIAlertAction(title: "Destructive", style: .destructive) { _ in
            print("Destructive")
    })
    alert.addAction(UIAlertAction(title: "OK", style: .default) { _ in
            print("OK")
    })
    
    present(alert, animated: true)

***Objective-C:***

    UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Destructive" message:@"Simple alertView demo with Destructive and OK." preferredStyle:UIAlertControllerStyleAlert];
        UIAlertAction *destructiveAction = [UIAlertAction actionWithTitle:@"Cancel" style:UIAlertActionStyleDestructive handler:^(UIAlertAction * action) {
            NSLog(@"Destructive");
        }];
        UIAlertAction *okAction = [UIAlertAction actionWithTitle:@"OK" style:UIAlertActionStyleDefault handler:^(UIAlertAction * action) {
            NSLog(@"OK");
        }];
        
        [alertController addAction:destructiveAction];
        [alertController addAction:okAction];
        [self presentViewController:alertController animated: YES completion: nil];

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/Pyu1U.png
  [2]: http://i.stack.imgur.com/nQDRl.png

## Action Sheets with UIAlertController
With `UIAlertController`, action sheets like the deprecated `UIActionSheet` are created with the same API as you use for AlertViews.

# Simple Action Sheet with two buttons

## Swift

    let alertController = UIAlertController(title: "Demo", message: "A demo with two buttons", preferredStyle: UIAlertControllerStyle.actionSheet)

## Objective-C

    UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Demo" message:@"A demo with two buttons" preferredStyle:UIAlertControllerStyleActionSheet];

Create the buttons "Cancel" and "Okay"

## Swift

    let cancelAction = UIAlertAction(title: "Cancel", style: .cancel) { (result : UIAlertAction) -> Void in
        //action when pressed button
    }
    let okAction = UIAlertAction(title: "Okay", style: .default) { (result : UIAlertAction) -> Void in
        //action when pressed button
    }

## Objective-C

    UIAlertAction *cancelAction = [UIAlertAction actionWithTitle:@"Cancel" style:UIAlertActionStyleCancel handler:^(UIAlertAction * action) {
            //action when pressed button
        }];
    
    UIAlertAction * okAction = [UIAlertAction actionWithTitle:@"Okay" style:UIAlertActionStyleDefault handler:^(UIAlertAction * action) {
            //action when pressed button
        }];

And add them to the action sheet:

## Swift

    alertController.addAction(cancelAction)
    alertController.addAction(okAction)

## Objective-C

    [alertController addAction:cancelAction];
    [alertController addAction:okAction];

Now present the `UIAlertController`:

## Swift

    self.present(alertController, animated: true, completion: nil)

## Objective-C

    [self presentViewController:alertController animated: YES completion: nil];

This should be the result:

[![UIAlertController Action Sheet example][1]][1]

# Action Sheet with destructive button

Using the `UIAlertActionStyle` `.destructive` for an `UIAlertAction` will create a button with red tint color.

[![Destructive button][2]][2]

For this example, the `okAction` from above was replaced by this `UIAlertAction`:

## Swift

    let destructiveAction = UIAlertAction(title: "Delete", style: .destructive) { (result : UIAlertAction) -> Void in
        //action when pressed button
    }

## Objective-C

    UIAlertAction * destructiveAction = [UIAlertAction actionWithTitle:@"Delete" style:UIAlertActionStyleDestructive handler:^(UIAlertAction * action) {
                //action when pressed button
            }];


  [1]: http://i.stack.imgur.com/M0uT4.png
  [2]: http://i.stack.imgur.com/yfT3b.png

## Adding Text Field in  UIAlertController like a prompt Box
## Swift
    
    let alert = UIAlertController(title: "Hello",
                                  message: "Welcome to the world of iOS",
                                  preferredStyle: UIAlertControllerStyle.alert)
    let defaultAction = UIAlertAction(title: "OK", style: UIAlertActionStyle.default) { (action) in
            
    }
    defaultAction.isEnabled = false
    alert.addAction(defaultAction)
        
    alert.addTextFieldWithConfigurationHandler { (textField) in
         textField.delegate = self
    }
        
    present(alert, animated: true, completion: nil)


## Objective-C

    UIAlertController* alert = [UIAlertController alertControllerWithTitle:@"Hello"
                                                                   message:@"Welcome to the world of iOS"
                                                            preferredStyle:UIAlertControllerStyleAlert];
        
    UIAlertAction* defaultAction = [UIAlertAction actionWithTitle:@"OK" 
                                                            style:UIAlertActionStyleDefault
                                                          handler:^(UIAlertAction * action) {}];
        
    defaultAction.enabled = NO;
    [alert addAction:defaultAction];    
        
    [alert addTextFieldWithConfigurationHandler:^(UITextField *textField) {
         textField.delegate = self;
    }];        
 
    [self presentViewController:alert animated:YES completion:nil];

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/ga7am.png

## Highlighting an action button
Alert controller has a property which is used to put emphases on an action added in the alert controller. This property can be used to highlight a particular action for user attention.For objective C;

    @property(nonatomic, strong) UIAlertAction *preferredAction

An action **which is already added in alert controller** can be assigned to this property.The Alert Controller will highlight this action.

**This property can only be used with UIAlertControllerStyleAlert.**

Following example shows how to use it.

    UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Cancel edit" message:@"Are you really want to cancel your edit?" preferredStyle:UIAlertControllerStyleAlert];
 
    UIAlertAction *cancel = [UIAlertAction actionWithTitle:@"Cancel" style:UIAlertActionStyleCancel handler:^(UIAlertAction * action) {
        NSLog(@"Cancel");
    }];
    
    UIAlertAction *no = [UIAlertAction actionWithTitle:@"NO" style:UIAlertActionStyleDefault handler:^(UIAlertAction * action) {
        NSLog(@"Highlighted button is pressed.");
    }];

    [alertController addAction:cancel];
    [alertController addAction:no];
    
    //add no action to preffered action.
    //Note
    //the action should already be added to alert controller
    alertController.preferredAction = no;
    
    [self presentViewController:alertController animated: YES completion: nil];

Alert Controller with **preferred action set**.The **NO** button is highlighted.

[![enter image description here][1]][1]


Alert Controller with **preferred action not set**.The **NO** button is not highlighted.

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/C7LVT.png
  [2]: http://i.stack.imgur.com/WLLu0.png

## Displaying and handling alerts
# One Button

[![enter image description here][1]][1]

## Swift

    class ViewController: UIViewController {
    
        @IBAction func showAlertButtonTapped(sender: UIButton) {
            
            // create the alert
            let alert = UIAlertController(title: "My Title", message: "This is my message.", preferredStyle: UIAlertControllerStyle.Alert)
            
            // add an action (button)
            alert.addAction(UIAlertAction(title: "OK", style: UIAlertActionStyle.Default, handler: nil))

            // show the alert
            self.presentViewController(alert, animated: true, completion: nil)
        }
    }

# Two Buttons

[![enter image description here][2]][2]

## Swift

    class ViewController: UIViewController {
    
        @IBAction func showAlertButtonTapped(sender: UIButton) {
            
            // create the alert
            let alert = UIAlertController(title: "UIAlertController", message: "Would you like to continue learning how to use iOS alerts?", preferredStyle: UIAlertControllerStyle.Alert)
            
            // add the actions (buttons)
            alert.addAction(UIAlertAction(title: "Continue", style: UIAlertActionStyle.Default, handler: nil))
            alert.addAction(UIAlertAction(title: "Cancel", style: UIAlertActionStyle.Cancel, handler: nil))
            
            // show the alert
            self.presentViewController(alert, animated: true, completion: nil)
        }
    }

# Three Buttons

[![enter image description here][3]][3]

## Swift

    class ViewController: UIViewController {
    
        @IBAction func showAlertButtonTapped(sender: UIButton) {
            
            // create the alert
            let alert = UIAlertController(title: "Notice", message: "Lauching this missile will destroy the entire universe. Is this what you intended to do?", preferredStyle: UIAlertControllerStyle.Alert)
            
            // add the actions (buttons)
            alert.addAction(UIAlertAction(title: "Remind Me Tomorrow", style: UIAlertActionStyle.Default, handler: nil))
            alert.addAction(UIAlertAction(title: "Cancel", style: UIAlertActionStyle.Cancel, handler: nil))
            alert.addAction(UIAlertAction(title: "Launch the Missile", style: UIAlertActionStyle.Destructive, handler: nil))
            
            // show the alert
            self.presentViewController(alert, animated: true, completion: nil)
        }
    }

# Handling Button Taps

The `handler` was `nil` in the above examples. You can replace `nil` with a [closure][4] to do something when the user taps a button, like the example below:

## Swift

    alert.addAction(UIAlertAction(title: "Launch the Missile", style: UIAlertActionStyle.Destructive, handler: { action in
        
        // do something like...
        self.launchMissile()
        
    }))

# Notes

- Multiple buttons do not necessarily need to use different `UIAlertActionStyle` types. They could all be `.Default`.
- For more than three buttons consider using an Action Sheet. The setup is very similar. [Here is an example.][5]


  [1]: http://i.stack.imgur.com/x5V0q.png
  [2]: http://i.stack.imgur.com/3Rq2U.png
  [3]: http://i.stack.imgur.com/IG91U.png
  [4]: https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Closures.html
  [5]: http://stackoverflow.com/a/32991999/3681880

## Temporary toast-like pop up
Good for quick notifications that don't require interaction.

## Swift

    let alert = UIAlertController(title: "Toast", message: "Hello World", preferredStyle: .Alert)

    presentViewController(alert, animated: true) {
         let delay_s:Double = 2
         let delayTime = dispatch_time(DISPATCH_TIME_NOW, Int64(delay_s * Double(NSEC_PER_SEC)))
         dispatch_after(delayTime, dispatch_get_main_queue()) {
              alert.dismissViewControllerAnimated(true, completion: nil)
         }
    }

