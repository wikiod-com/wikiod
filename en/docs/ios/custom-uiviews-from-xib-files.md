---
title: "Custom UIViews from XIB files"
slug: "custom-uiviews-from-xib-files"
draft: false
images: []
weight: 7035
type: docs
toc: true
---

[From Apple: Creating a Custom View That Renders in Interface Builder][1]


  [1]: https://developer.apple.com/library/ios/recipes/xcode_help-IB_objects_media/Chapters/CreatingaLiveViewofaCustomObject.html

• Note: Keep in mind that if you'd use fancy 'custom'  fonts in your XIB elements (such UILabel, UITextField etc) then the initial loading time of your XIB will be longer depending on the font chosen and system version.

## Wiring elements
> Create a XIB file

Xcode Menu Bar > File > New > File.  
Select iOS, User Interface and then "View":

[![First step][1]][1]

Give your XIB a name (yes, we are doing a Pokemon example 👾).  
**Remember to check your target** and hit "Create".

[![Second step][2]][2]

> Design your view

To make things easier, set:
 
 - Size: Freeform
 - Status Bar: None
 - Top Bar: None
 - Bottom Bar: None

[![Third step][3]][3]

Click on the Size Inspector and resize the view.  
For this example we'll be using width 321 and height 256.

[![Fourth step][4]][4] 

Drop some elements into your XIB file like shown below.  
Here we'll be adding an **Image View** (256x256) and a **Switch**.

[![Fifth step][5]][5]

Add Auto-Layout constraints by clicking on "Resolve Auto Layout Issues" (bottom-right) and selecting "Add Missing Constraints" under "All Views".

[![Sixth step][6]][6]

Preview the changes you made by clicking on "Show the Assistant Editor" (top-right), then "Preview".  
You can add iPhone screens by clicking on the "Plus" button.  
The preview should look like this:

[![Seventh step][7]][7]

> Subclass UIView

Create the class that is going to manage the XIB file.  
Xcode Menu Bar > File > New > File.  
Select iOS / Source / Cocoa Touch Class. Hit "Next".

[![Eighth step][8]][8]

Give the class a name, which must be the same name as the XIB file (Pokemon).  
Select UIView as the subclass type, then hit "Next".

[![Nineth step][9]][9]

On the next window, select your target and hit "Create".

[![Tenth step][10]][10]

> Connect Pokemon.xib to Pokemon.swift via "File’s Owner" attribute

Click on the Pokemon.xib file in Xcode.  
Click on the "File's Owner" outlet.  
On the "Identity inspector" (top-right), set the Class to our recently created Pokemon.swift file.

[![Eleventh step][11]][11]

> POKEMONS!!!

Yes! Drag and drop some Pokemons into your project to finish up our "infrastructure".  
Here we are adding two PGN files, 256x256, transparent.

[![Twelfth step][12]][12]

> Show me code already.

All right, all right.  
Time to add some code to our Pokemon.swift class.

It's actually pretty simple:

 1. Implement required initializers
 2. Load the XIB file
 3. Configure the view that will display the XIB file
 4. Show the above view

Add the following code to the Pokemon.swift class:

    import UIKit
    
    class Pokemon: UIView {
        
        // MARK: - Initializers
        
        override init(frame: CGRect) {
            super.init(frame: frame)
            setupView()
        }
        
        required init?(coder aDecoder: NSCoder) {
            super.init(coder: aDecoder)
            setupView()
        }
        
        // MARK: - Private Helper Methods
        
        // Performs the initial setup.
        private func setupView() {
            let view = viewFromNibForClass()
            view.frame = bounds

            // Auto-layout stuff.
            view.autoresizingMask = [
                UIViewAutoresizing.flexibleWidth,
                UIViewAutoresizing.flexibleHeight
            ]

            // Show the view.
            addSubview(view)
        }
        
        // Loads a XIB file into a view and returns this view.
        private func viewFromNibForClass() -> UIView {
            
            let bundle = Bundle(for: type(of: self))
            let nib = UINib(nibName: String(describing: type(of: self)), bundle: bundle)
            let view = nib.instantiate(withOwner: self, options: nil).first as! UIView
            
            /* Usage for swift < 3.x
            let bundle = NSBundle(forClass: self.dynamicType)
            let nib = UINib(nibName: String(self.dynamicType), bundle: bundle)
            let view = nib.instantiateWithOwner(self, options: nil)[0] as! UIView
            */

            return view
        }
    }

> @IBDesignable and @IBInspectable

By adding `@IBDesignable` to your class, you make possible for it to live-render in Interface Builder.  
By adding `@IBInspectable` to the properties of your class, you can see your custom views changing in Interface Builder as soon as you modify those properties.  
  
Let's make the `Image View` of our custom view "Inspectable".  

First, hook up the `Image View` from the Pokemon.xib file to the Pokemon.swift class.

[![Thirteenth step][13]][13]

Call the outlet `imageView` and then add the following code (notice the `@IBDesignable` before the class name):

    @IBDesignable class Pokemon: UIView {
        
        // MARK: - Properties
        
        @IBOutlet weak var imageView: UIImageView!
        
        @IBInspectable var image: UIImage? {
            get {
                return imageView.image
            }
            set(image) {
                imageView.image = image
            }
        }

        // MARK: - Initializers
        ...

> Using your Custom Views

Got to your Main storyboard file, drag a UIView into it.  
Resize the view to, say 200x200. Centralize.  
Go to the Identity inspector (top-right) and set the Class to Pokemon.

[![Fourteenth steps][14]][14]

To select a Pokemon, go to the Attribute Inspector (top-right) and select one of the Pokemon images you previously added using the awesome `@IBInspectable` image property.

[![Fifteenth step][15]][15]

Now duplicate your custom Pokemon view.  
Give it a different size, say 150x150.  
Choose another Pokemon image, observe:

[![Sixteenth step][16]][16]

Now we are going to add more logic to that self-containing custom UI element.  
The button will allow Pokemons to be enabled/disabled.

Create an `IBAction` from the Switch button to the Pokemon.swift class.  
Call the action something like `switchTapped`.  
Add the following code to it:
    
    // MARK: - Actions
    
    @IBAction func switchTapped(sender: UISwitch) {
        imageView.alpha = sender.on ? 1.0 : 0.2
    }
    
    // MARK: - Initializers
    ...

Final result:

[![Final][18]][18]

You are done!  
Now you can create complex custom views and reuse them anywhere you want.  
This will increase productivity while isolating code into self-contained UI elements.

[The final project can be cloned in Github.][17]  
(**Updated to Swift 3.1**)

  [1]: http://i.stack.imgur.com/RSkzu.png
  [2]: http://i.stack.imgur.com/oJ1s1.png
  [3]: http://i.stack.imgur.com/Gy0KD.png
  [4]: http://i.stack.imgur.com/PNIek.png
  [5]: http://i.stack.imgur.com/6u3pd.png
  [6]: http://i.stack.imgur.com/esZhQ.png
  [7]: http://i.stack.imgur.com/D3rac.png
  [8]: http://i.stack.imgur.com/0EyHy.png
  [9]: http://i.stack.imgur.com/qbo3s.png
  [10]: http://i.stack.imgur.com/Ks9Tu.png
  [11]: http://i.stack.imgur.com/4YT3i.png
  [12]: http://i.stack.imgur.com/ibqxZ.png
  [13]: http://i.stack.imgur.com/Dxyv2.png
  [14]: http://i.stack.imgur.com/Iah3Y.png
  [15]: http://i.stack.imgur.com/727mm.png
  [16]: http://i.stack.imgur.com/xrTXw.png
  [17]: https://github.com/singledev/custom-uiviews-from-xib
  [18]: http://i.stack.imgur.com/DQJvO.gif

## How to make custom reusable UIView using XIB
Following example shows steps involved in initializing a view from XIB.

This is not a complex operation but exact steps need to be followed in order to do it right way first time, avoiding exceptions.

[How does loadNibNamed Works][1]

Main steps are:

 1. Create XIB
 2. Create class .h and .m
 3. Define outlets in .h
 4. Connect outlets between .h and XIB

See attached screenshot:

[![Label Object connected to myLabel IBOutlet UILabel variable][2]][2]

 5. Invoke loadNibNamed inside initWithCoder function of .m file. This is needed to ensure you can directly place UIView object into storyboard / Parent UIView XIB file and define it as your custom view. No other initialization code is needed once you load the storyboard / parent XIB. Your custom view can be added to other views just like other built-in Objective C view objects given in XCode.


  [1]: http://stackoverflow.com/questions/20323393/how-does-loadnibnamed-work-uiview-outlets-not-initializing-using-loadnibnamed
  [2]: http://i.stack.imgur.com/rSBw6.png

