---
title: "UI Testing"
slug: "ui-testing"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Syntax
- XCUIApplication() // Proxy for an application. The information identifying the application is specified in the Xcode target settings as the "Target Application".
- XCUIElement() // A user interface element in an application.


## Accessibility Identifier
## When Accessibility enabled in Utilities

* Select `storyboard`.
* Expand `the Utilities`
* Select `Identity Inspector`
* Select your element on storyboard
* Add new Accessibility Identifier (in example `addButton`)

[![enter image description here][1]][1]


## When Accessibility disabled in Utilities

* Select `storyboard`.
* Expand `the Utilities`
* Select `Identity Inspector`
* Select your element on storyboard
* Add attribute in `User Defined Runtime Attributes`
* For `Key Path` type - `accessibilityIdentifier`
* For `Type` - `String
* For `Value` - new accessibility identifier for your element (in example `view`)

[![enter image description here][2]][2]

## Setting up in UITest file

    import XCTest

    class StackOverFlowUITests: XCTestCase {
    
        private let app = XCUIApplication()
    
        //Views
    
        private var view: XCUIElement!
    
        //Buttons
    
        private var addButton: XCUIElement!
    
    
        override func setUp() {
            super.setUp()
        
            app.launch()
        
            //Views
        
            view = app.otherElements["view"]
        
            //Buttons
        
            addButton = app.buttons["addButton"]
        }
    
        func testMyApp() {

            addButton.tap()
            view.tap()
        }    
    }

  [1]: https://i.stack.imgur.com/poCUD.png
  [2]: https://i.stack.imgur.com/Y9Psk.png


In `[ ]` add Accessibility Identifier for element.

## UIView, UIImageView, UIScrollView
   
    let imageView = app.images["imageView"]
    let scrollView = app.scrollViews["scrollView"]
    let view = app.otherElements["view"]

## UILabel

    let label = app.staticTexts["label"]

## UIStackView

    let stackView = app.otherElements["stackView"]

## UITableView
        
    let tableView = app.tables["tableView"]

## UITableViewCell
        
    let tableViewCell = tableView.cells["tableViewCell"]

## UITableViewCell elements
    
    let tableViewCellButton = tableView.cells.element(boundBy: 0).buttons["button"]
    
## UICollectionView
        
    let collectionView = app.collectionViews["collectionView"]

## UIButton, UIBarButtonItem
        
    let button = app.buttons["button"]
    let barButtonItem = app.buttons["barButtonItem"]

## UITextField

 * normal UITextField



    let textField = app.textFields["textField"]


* password UITextField


    let passwordTextField = app.secureTextFields["passwordTextField"]

## UITextView
     
    let textView = app.textViews["textView"]


## UISwitch

    let switch = app.switches["switch"]

## Alerts

    let alert = app.alerts["About yourself"] // Title of presented alert





    

## Adding Test Files to Xcode Project
## When creating the project

You should check "Include UI Tests" in the project creation dialog.


[![enter image description here][1]][1]


## After creating the project

If you missed checking `UI target` while creating project, you could always add test target later.

Setps: 
* While project open go to `File` -> `New` -> `Target`
* Find `iOS UI Testing Bundle`

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/WrHnW.png
  [2]: https://i.stack.imgur.com/zDp7e.png

## Disable animations during UI Testing
In a test you can disable animations by adding in `setUp`:

        app.launchEnvironment = ["animations": "0"]

Where `app` is instance of XCUIApplication.

## Lunch and Terminate application while executing
## Lunch application for testing

    override func setUp() {
        super.setUp()
 
        let app = XCUIApplication()

        app.launch()
    }

## Terminating application

    func testStacOverFlowApp() {
        
        app.terminate()
    }

## Rotate devices
Device can be rotate by changing `orientation` in `XCUIDevice.shared().orientation`:

    XCUIDevice.shared().orientation = .landscapeLeft
    XCUIDevice.shared().orientation = .portrait

