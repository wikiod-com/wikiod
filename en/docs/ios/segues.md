---
title: "Segues"
slug: "segues"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Using Segues to navigate backwards in the navigation stack
----------
**Unwind Segues**

> Unwind Segues give you a way to “unwind” the navigation stack and specify a destination to go back to. 
The signature of this function is key to Interface Builder recognizing it. 
**It must have a return value of IBAction and take one parameter of UIStoryboardSegue**. The name of the function does not matter. In fact, the function does not even have to do anything. It’s just there as a marker of which UIViewController is the destination of the Unwind Segue. [source][1]

Required signature of an unwind segue

Objective C:

    -(IBAction)prepareForUnwind:(UIStoryboardSegue *)segue {
    }

Swift:

    @IBAction func prepareForUnwind(segue: UIStoryboardSegue) {
    }

## An Overview
From the Apple documentation: 

> A UIStoryboardSegue object is responsible for **performing the visual transition between two view controllers**. In addition, segue objects are used to prepare for the transition from one view controller to another. **Segue objects contain information about the view controllers involved in a transition**. When a segue is triggered, but before the visual transition occurs, the storyboard runtime calls the current view controller’s prepareForSegue:sender: method so that it can pass any needed data to the view controller that is about to be displayed. 


----------


**Attributes**

Swift

    sourceViewController: UIViewController {get}
    destinationViewController: UIViewController {get}
    identifier: String? {get}



----------


References:

 - [UIViewController Class Reference][2]
 - [UIStoryBoardSegue Class Reference][3]


  [1]: http://Unwind%20Segues%20give%20you%20a%20way%20to%20%E2%80%9Cunwind%E2%80%9D%20the%20navigation%20stack%20and%20specify%20a%20destination%20to%20go%20back%20to
  [2]: https://developer.apple.com/library/ios/documentation/UIKit/Reference/UIViewController_Class/index.html#//apple_ref/occ/instm/UIViewController/shouldPerformSegueWithIdentifier:sender:
  [3]: https://developer.apple.com/library/ios/documentation/UIKit/Reference/UIStoryboardSegue_Class/index.html#//apple_ref/doc/uid/TP40010911-CH1-SW8

## Preparing your view controller before a triggering a Segue

*PrepareForSegue*:
==================================

     func prepareForSegue(_ segue:UIStoryboardSegue, sender sender:AnyObject?) 


> Notifies the view controller that a segue is about to be performed 

Parameters
----------


*segue*: The segue object.

*sender*: The object that initialized the segue.

Example in Swift
----------------


Perform a task if the identifier of the segue is "SomeSpecificIdentifier"

    override func prepareForSegue(segue: UIStoryboardSegue, sender: AnyObject?) {
        if segue.identifier == "SomeSpecificIdentifier" {
            //- Do specific task
        }
    }



  

## Deciding if an invoked Segue should be performed.
*ShouldPerformSegueWithIdentifier*:
==================================

     func shouldPerformSegueWithIdentifier(_ identifier:String, sender sender:AnyObject?) -> Bool 


> Determines whether the segue with the specified identifier should be performed. 

Parameters
----------


*Identifier*: String that identifies the triggered segue

*Sender*: The object that initialized the segue.

Example in Swift
----------------


Only perform segue if the identifier is "SomeSpecificIdentifier"

    override func shouldPerformSegueWithIdentifier(identifier:String, sender:AnyObject?) -> Bool {
        if identifier == "SomeSpecificIdentifier" {
            return true
        }
        return false
    }
  

## Trigger Segue Programmatically

PerformSegueWithIdentifier:
--------------------------

     func performSegueWithIdentifier(_ identifier:String, sender sender:AnyObject?)


> Initiates the segue with the specified identifier from the current view controller's storyboard file 

Parameters
----------


*Identifier*: String that identifies the triggered segue

*Sender*: The object that will initiate the segue.

Example in Swift
----------------


Performing a segue with identifier "SomeSpecificIdentifier" from a table view row selection:

    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        performSegueWithIdentifier("SomeSpecificIdentifier", sender: indexPath.item)
    }

