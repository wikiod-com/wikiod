---
title: "Custom fonts"
slug: "custom-fonts"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Embedding custom fonts

> **Custom Font Support**<br>
Applications that want to use custom fonts can now include those fonts in their application bundle and register those fonts with the system by including the UIAppFonts key in their Info.plist file. The value of this key is an array of strings identifying the font files in the application’s bundle. When the system sees the key, it loads the specified fonts and makes them available to the application.


Once the fonts have been set in the `Info.plist`, you can use your custom fonts as any other font in IB or programatically.

1) Drag and drop your font to Xcode Supporting Files folder. Don't forget to mark your app at "Add to targets" section. From this moment you can use this font in IB and choose it from font pallet.

[![enter image description here][1]][1]

2) To make this font available on the device, open `Info.plist` and add `Fonts provided by application key` (UIAppFonts). Add font name as the value to the Item 0 key. Note: Font name can vary from your font file name.
[![enter image description here][2]][2]


3) Get the custom added font name using below snippet

[*Swift 3*]

    for family in UIFont.familyNames {
                print("\(family)")
    
                for name in UIFont.fontNames(forFamilyName: family) {
                    print("   \(name)")
                }
            }

[*Objective - C*]

    for (NSString *familyName in [UIFont familyNames]){
            NSLog(@"Family name: %@", familyName);
            for (NSString *fontName in [UIFont fontNamesForFamilyName:familyName]) {
                NSLog(@"--Font name: %@", fontName);
            }
        }


  [1]: http://i.stack.imgur.com/d9Vun.png
  [2]: http://i.stack.imgur.com/nN47U.png

## Custom Fonts with Storyboard
Custom Fonts for UI components from storyboard can be easily achieved with [User Defined Runtime Attributes][1] in storyboard and [Categories][2].

The advantages are like,

 - No need to define outlets for the ui element
 - No need to set font for elements programatically.

> **Steps to follow**
> 
>  1. **Font File:** Add the Font file (.ttf) to the application bundle and add the entry for the font in Info.plist under ***Font provided by
> application*** as in this [documentation][3] of custom fonts.
> 
>  2. **Define Categories:** Add a file like ***UIKit+IBExtensions*** and add the categories for UI elements like UILabel, UIButton etc. for 
> which you want to set custom font. All the categories will be having a
> custom property say **fontName**. This will be using from the
> storyboard later for setting custom font (as in step 4).

   

# UIKit+IBExtensions.h
    
    #import <UIKit/UIKit.h>
    
    //Category extension for UILabel
    @interface UILabel (IBExtensions)
    
    @property (nonatomic, copy) NSString *fontName;
    @end
    
    // Category extension for UITextField
    @interface UITextField (IBExtensions)
    
    @property (nonatomic, copy) NSString *fontName;
    @end
    
    // Category extension for UIButton
    @interface UIButton (IBExtensions)
    
    @property (nonatomic, copy) NSString *fontName;
    @end


 

> 3. **Getters and Setters:** Define getters and setters for the fontName property towards each category added.

# UIKit+IBExtensions.m

    #import "UIKit+IBExtensions.h"
    
    @implementation UILabel (IBExtensions)
    
    - (NSString *)fontName {
        return self.font.fontName;
    }
    
    - (void)setFontName:(NSString *)fontName {
        self.font = [UIFont fontWithName:fontName size:self.font.pointSize];
    }
    @end
    
    @implementation UITextField (IBExtensions)
    
    - (NSString *)fontName {
        return self.font.fontName;
    }
    
    - (void)setFontName:(NSString *)fontName {
        self.font = [UIFont fontWithName:fontName size:self.font.pointSize];
    }
    @end
    
    @implementation UIButton (IBExtensions)
    
    - (NSString *)fontName {
        return self.titleLabel.font.fontName;
    }
    
    - (void)setFontName:(NSString *)fontName{
        self.titleLabel.font = [UIFont fontWithName:fontName size:self.titleLabel.font.pointSize];
    }
    @end

 

> 4. **Setting font in storyboard:**  Add an entry in User Defined Runtime Attributes with ***fontName*** as keyPath and your ***Custom
> Font's Name*** as value with type as String as shown.


   [![enter image description here][4]][4]
        

*This will set your custom font while running the app.*

Notes: 

 - Lato-Regular is the custom font I have used. 
 - Same name in the ***.ttf*** file added in bundle should be used without extension in storyboard. 
 - Font size will be same as it is defined in the UI element's attribute inspector. 


  [1]: https://developer.apple.com/library/ios/recipes/xcode_help-interface_builder/Chapters/AddUserDefinedRuntimeAttributes.html
  [2]: https://developer.apple.com/library/ios/documentation/General/Conceptual/DevPedia-CocoaCore/Category.html
  [3]: https://www.wikiod.com/ios
  [4]: http://i.stack.imgur.com/uG7Kh.png

## Applying custom fonts  to controls within a Storyboard
The following example shows how to apply custom fonts to a Navigation Bar and includes fixes for some quirky behaviors found in Xcode. One also may apply the custom fonts to **any other UIControls** such as **UILabels**, **UIButtons**, and more by using the attributes inspector after the custom font is added to the project. Please note the external links to working samples and videos near the bottom.

1. Select Your Navigation Bar within your Navigation Controller
---------------------------------------------------------------

[![navbar][1]][1]

2. Change the Title Font in the Attributes Inspector
----------------------------------------------------
[![title-font][2]][2]

*(You will likely need to toggle the Bar Tint for the Navigation Bar before Xcode picks up the new font)*

Notes (Caveats)
---------------

Verified that this does work on Xcode 7.1.1+. (**See the Samples below**)

 1. You do need to toggle the nav bar tint before the font takes effect (seems like a bug in Xcode; you can switch it back to default and font will stick)
 2. If you choose a system font ~ Be sure to make sure the size is not
    0.0 (Otherwise the new font will be ignored)

[![size][3]][3]

 3. Seems like this works with no problem when only one NavBar is in the view
    hierarchy. It appears that secondary NavBars in the same stack are ignored. (Note that if you show the master navigation controller's navBar all the other custom navBar settings are ignored).

Gotchas (deux)
-------------

*Some of these are repeated which means they are very likely worth noting.*

 1. Sometimes the storyboard xml gets corrupt. This requires you to
    review the structure in Storyboard as Source Code mode (right click
    the storyboard file > Open As ...)
 2. In some cases the navigationItem tag associated with user defined runtime attribute was set as an xml 
    child of the view tag instead of the view controller tag. If so
    remove it from between the <view></view> tags for proper operation.
 3. Toggle the NavBar Tint to ensure the custom font is
    used.
 4. Verify the size parameter of the font unless using a dynamic font
    style
 5. View hierarchy will override the settings. It appears that one font
    per stack is possible.

Result
------

[![navbar-italic][4]][4]

Samples
-------

 - [Video Showing Multiple Fonts In Advanced Project][5]
 - [Simple Source Download][6]
 - [Advanced Project Download ~ Shows Multiple NavBar Fonts & Custom Font Workaround][7]
 - [Video Showing Multiple Fonts & Custom Fonts][8]



Handling Custom Fonts
---------------------
*Note ~ A [nice checklist][9] can be found from the Code With Chris website and you can see the sample download project.*

If you have your own font and want to use that in your storyboard, then there is a decent set of answers on the following [SO Question][10]. One answer identifies these steps.

 1. Get you custom font file(.ttf,.ttc)
 2. Import the font files to your Xcode project
 3. In the app-info.plist,add a key named Fonts provided by
    application.It's an array type , add all your font file names to the
    array,note:including the file extension.
 4. In the storyboard , on the NavigationBar go to the Attribute
    Inspector,click the right icon button of the Font select area.In the
    popup panel , choose Font to Custom, and choose the Family of you
    embeded font name.

Custom Font Workaround
----------------------

So Xcode naturally looks like it can handle custom fonts on UINavigationItem but that feature is just not updating properly (The font selected is ignored).

[![UINavigationItem][11]][11]

> **To workaround this:** 
> 
> One way is to fix using the storyboard and adding a line of
> code: First add a UIView (UIButton, UILabel, or some other UIView
> subclass) to the View Controller (Not the Navigation Item...Xcode is not currently allowing one to do that). After you add the control
> you can modify the font in the storyboard and add a reference as an
> outlet to your View Controller. Just assign that view to the
> UINavigationItem.titleView. You could also set the text name in code
> if necessary. Reported Bug (23600285).

    @IBOutlet var customFontTitleView: UIButton!
    
    //Sometime later...    
    self.navigationItem.titleView = customFontTitleView

Note - This example is derived from an answer I posted on SO ([here][12]).


  [1]: http://i.stack.imgur.com/kTgul.png
  [2]: http://i.stack.imgur.com/FQyiW.png
  [3]: http://i.stack.imgur.com/33nJ9.png
  [4]: http://s28.postimg.org/gvgs0lxwd/NHg_Ex.png
  [5]: https://googledrive.com/host/0B8D3kbf8rViMS3MxQ3prcXRXUHM
  [6]: https://bitbucket.org/mingsai/navbarfontupdate/get/b796b40e1ec5.zip
  [7]: https://bitbucket.org/mingsai/multiplenavstackfonttest/get/bfd5fcb34be9.zip
  [8]: https://googledrive.com/host/0B8D3kbf8rViMUHdUSE1BbW4zUnM
  [9]: http://codewithchris.com/common-mistakes-with-adding-custom-fonts-to-your-ios-app/
  [10]: http://stackoverflow.com/questions/9090745/custom-font-in-a-storyboard#15155081
  [11]: http://i.stack.imgur.com/1SvP0.png
  [12]: http://stackoverflow.com/questions/19791762/ios-change-navigation-bar-title-font-and-color/33761674#33761674

