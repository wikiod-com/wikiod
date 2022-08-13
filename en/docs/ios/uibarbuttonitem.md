---
title: "UIBarButtonItem"
slug: "uibarbuttonitem"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Parameters
| Parameter | Description |
| ------ | ------ |
| title   | The UIBarButtonItem title   |
| style   | The style of the UIBarButtonItem   |
| target   | The object to receive the UIBarButtonItem action   |
| action   | The selector (method) to be performed when the UIBarButtonItem is pressed   |

Referencing `self.navigationItem` assumes that the UIViewController is embedded inside a UINavigationController.

## Creating a UIBarButtonItem
        
    //Swift
    let barButtonItem = UIBarButtonItem(title: "Greetings!", style: .Plain, target: self, action: #selector(barButtonTapped))
    self.navigationItem.rightBarButtonItem = barButtonItem

    //Objective-C
    UIBarButtonItem *barButtonItem = [[UIBarButtonItem alloc] initWithTitle:@"Greetings!" style:UIBarButtonItemStylePlain target:self action:@selector(barButtonTaped)];
    self.navigationItem.rightBarButtonItem = barButtonItem;

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/500Wr.png

## Creating a UIBarButtonItem in the Interface Builder
The example below shows how to add a navigation bar button (called a `UIBarButtonItem`) in the Interface Builder.

# Add a Navigation Controller to your Storyboard

Select your View Controller and then in the Xcode menu choose **Editor > Embed In > Navigation Controller**.

[![navigation controller screenshot][1]][1]

Alternatively, you could add a `UINavigationBar` from the Object Library.

# Add a Bar Button Item

Drag a `UIBarButtonItem` from the Object Library to the top navigation bar.

[![UIBarButtonItem in the object library screenshot][2]][2]

It should look like this:

[![UIBarButtonItem placed on the storyboard screenshot][3]][3]

# Set the Attributes

You could double-click "Item" to change the text to something like "Refresh", but there is an actual icon for *Refresh* that you can use. Just select the Attributes Inspector for the `UIBarButtonItem` and for **System Item** choose **Refresh**.

[![enter image description here][4]][4]

That will give you the default Refresh icon.

[![enter image description here][5]][5]

# Add an IB Action

Control drag from the `UIBarButtonItem` to the View Controller to add an `@IBAction`.

    class ViewController: UIViewController {
    
        @IBAction func refreshBarButtonItemTap(sender: UIBarButtonItem) {
            
            print("How refreshing!")
        }
        
    }

That's it. 

# Notes

- This example originally comes from [this Stack Overflow answer][6].


  [1]: http://i.stack.imgur.com/zWgLB.png
  [2]: http://i.stack.imgur.com/PsbYO.png
  [3]: http://i.stack.imgur.com/Th0kS.png
  [4]: http://i.stack.imgur.com/HWLVq.png
  [5]: http://i.stack.imgur.com/wRDNf.png
  [6]: http://stackoverflow.com/a/33670242/3681880

## Bar Button Item Original Image with no Tint Color


