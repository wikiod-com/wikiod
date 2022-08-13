---
title: "Custom Keyboard"
slug: "custom-keyboard"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Custom KeyBoard Example
**Objective-C and Xib**

Add a target to an existing XCode project 

[![enter image description here][1]][1]

In the Add Target select Custom KeyBoard[![enter image description here][2]][2]

Add the target like this:

[![enter image description here][3]][3]

Your project file directory should look something like this 

[![enter image description here][4]][4]

Here myKeyBoard is the name of the added Target

Add new Cocoatouch file of type of type UIView and add an interface file 

[![enter image description here][5]][5]



Finally your project directory should look like this 

[![enter image description here][6]][6]


make the `keyBoardView.xib` a subclass of `keyBoardView`

[![enter image description here][7]][7]

Make interface in the `keyBoardView.xib` file 

[![enter image description here][8]][8]

Make connections to from the `keyBoardView.xib` to `keyBoardView.h` file 

`keyBoardView.h` should look like


    #import <UIKit/UIKit.h>
    
    @interface keyBoardView : UIView
    
    @property (weak, nonatomic) IBOutlet UIButton *deleteKey;
    //IBOutlet for the delete Key
    @property (weak, nonatomic) IBOutlet UIButton *globe;
    //Outlet for the key with title globe which changes the keyboard type
    @property (strong, nonatomic) IBOutletCollection(UIButton) NSArray *keys;
    //Contains a colloection of all the keys '0 to 9' '+' '-' and '.'
    
    @end

In the `keyBoardViewController.h` file import `#import "keyBoardView.h"` 

Declare a property for keyboard  `@property (strong, nonatomic)keyBoardView *keyboard;`

Comment out the 

    @property (nonatomic, strong) UIButton *nextKeyboardButton and all the code associated with it

The KeyboardViewController.m file's viewDidLoad() function should look like this 

    - (void)viewDidLoad {
        [super viewDidLoad];
        self.keyboard=[[[NSBundle mainBundle]loadNibNamed:@"keyBoardView" owner:nil options:nil]objectAtIndex:0];
          self.inputView=self.keyboard;
        [self addGestureToKeyboard];
    
        // Perform custom UI setup here
    //    self.nextKeyboardButton = [UIButton buttonWithType:UIButtonTypeSystem];
    //    
    //    [self.nextKeyboardButton setTitle:NSLocalizedString(@"Next Keyboard", @"Title for 'Next Keyboard' button") forState:UIControlStateNormal];
    //    [self.nextKeyboardButton sizeToFit];
    //    self.nextKeyboardButton.translatesAutoresizingMaskIntoConstraints = NO;
    //    
    //    [self.nextKeyboardButton addTarget:self action:@selector(advanceToNextInputMode) forControlEvents:UIControlEventTouchUpInside];
    //    
    //    [self.view addSubview:self.nextKeyboardButton];
    //    
    //    [self.nextKeyboardButton.leftAnchor constraintEqualToAnchor:self.view.leftAnchor].active = YES;
    //    [self.nextKeyboardButton.bottomAnchor constraintEqualToAnchor:self.view.bottomAnchor].active = YES;
    }

The functions `addGestureToKeyboard`, `pressDeleteKey`, `keyPressed`  are defined below 

    -(void) addGestureToKeyboard
    {
        [self.keyboard.deleteKey addTarget:self action:@selector(pressDeleteKey) forControlEvents:UIControlEventTouchUpInside];
        [self.keyboard.globe addTarget:self action:@selector(advanceToNextInputMode) forControlEvents:UIControlEventTouchUpInside];
        
        for (UIButton *key in self.keyboard.keys)
        {
            [key addTarget:self action:@selector(keyPressed:) forControlEvents:UIControlEventTouchUpInside];
        }
        
        
    }
    -(void) pressDeleteKey
    {
        [self.textDocumentProxy deleteBackward];
    }
    
    -(void)keyPressed:(UIButton *)key
    {
        [self.textDocumentProxy insertText:[key currentTitle]];
    }

Run the Main Application and go to Settings->General->Keyboard->Add New Keyboard-> and add keyboard from the third party keyboard section (The displayed keyboardName would be keyBoardCustom)

The keyboard name can be changed by adding a key called `Bundle display name` and in the Value String Value enter the desired name for the keyboard of the main Project.



[![enter image description here][10]][10]

You can also watch this  [Youtube Video][9]
 


  [1]: http://i.stack.imgur.com/tZZCr.png
  [2]: http://i.stack.imgur.com/17jG0.png
  [3]: http://i.stack.imgur.com/EBX9s.png
  [4]: http://i.stack.imgur.com/lmep3.png
  [5]: http://i.stack.imgur.com/VSnGG.png
  [6]: http://i.stack.imgur.com/6bn96.png
  [7]: http://i.stack.imgur.com/7Fey1.png
  [8]: http://i.stack.imgur.com/QZuRQ.png
  [9]: https://www.youtube.com/watch?v=gczzfq6DuHo
  [10]: http://i.stack.imgur.com/PrKMW.png

