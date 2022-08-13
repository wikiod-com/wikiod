---
title: "Add Ons and Modules"
slug: "add-ons-and-modules"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Addons and modules are encouraged to be registered with Packagist which then means they are found and registered with the [SilverStripe add-on repository][1]

Installation of modules is recommended through [use of Composer][2]

  [1]: http://addons.silverstripe.org/
  [2]: https://docs.silverstripe.org/en/3.4/getting_started/composer/
  
  


## SilverStripe Grid Field Extensions Module
The [SilverStripe Grid Field Extensions Module][1] has some very nice features to enhance the basic `GridField`...

 - `GridFieldAddExistingSearchButton` - a more advanced search form for adding items
 - `GridFieldAddNewInlineButton` - builds on `GridFieldEditableColumns` to allow inline creation of records.
 - `GridFieldAddNewMultiClass` - lets the user select from a list of classes to create a new record from
 - `GridFieldEditableColumns` - allows inline editing of records
 - `GridFieldOrderableRows` - drag and drop re-ordering of rows
 - `GridFieldRequestHandler` - a basic utility class which can be used to build custom grid field detail views including tabs, breadcrumbs and other CMS features
 - `GridFieldTitleHeader` - a simple header which displays column titles

More documentation is found within [the module here][2].

  [1]: https://github.com/silverstripe-australia/silverstripe-gridfieldextensions
  [2]: https://github.com/silverstripe-australia/silverstripe-gridfieldextensions/blob/master/docs/en/index.md

## Better Buttons for GridField
The module [Better Buttons for GridField][1] adds new form actions and buttons to the GridField detail form.

 - Save and add another: Create a record, and go right to adding another one, without having to click the back button, and then add again
 - Save and close: Save the record and go back to list view
 - User-friendly delete: Extracted from the tray of constructive actions and moved away so is less likely to be clicked accidentally. Includes inline confirmation of action instead of browser alert box
 - Cancel: Same as the back button, but in a more convenient location
 - Previous/Next record: Navigate to the previous or next record in the list without returning to list view
 - and many more...

More documentation (and images) on the [documentation for the module][2]

  [1]: http://%20https://github.com/unclecheese/silverstripe-gridfield-betterbuttons
  [2]: https://github.com/unclecheese/silverstripe-gridfield-betterbuttons/blob/master/README.md

## UserForms
The module [UserForms][1] enables CMS users to create dynamic forms via a drag and drop interface and without getting involved in any PHP code.

Main Features

 - Construct a form using all major form fields (text, email, dropdown, radio, checkbox..)
 - Ability to extend userforms from other modules to provide extra fields.
 - Ability to email multiple people the form submission
 - View submitted submissions and export them to CSV
 - Define custom error messages and validation settings
 - Optionally display and hide fields using javascript based on users input
 - Displays a confirmation message when navigating away from a partially completed form

More documentation links can be found [here in the github repository][2]

  [1]: https://github.com/silverstripe/silverstripe-userforms
  [2]: https://github.com/silverstripe/silverstripe-userforms/tree/master/docs/en

## Display Logic
The [Display Logic module][1] allows you to add conditions for displaying or hiding certain form fields based on client-side behavior.  This module is incredibly useful to make forms much more professional by showing only the appropriate fields and without adding a lot of custom JavaScript.

Example usage...

    $products->displayIf("HasProducts")->isChecked();
    
    $sizes->hideUnless("ProductType")->isEqualTo("t-shirt")
          ->andIf("Price")->isGreaterThan(10);
    
    $payment->hideIf("Price")->isEqualTo(0);
    
    $shipping->displayIf("ProductType")->isEqualTo("furniture")
               ->andIf()
                  ->group()
                    ->orIf("RushShipping")->isChecked()
                    ->orIf("ShippingAddress")->isNotEmpty()
                  ->end();

There are many more examples on the [module readme.md][2]

  [1]: https://github.com/unclecheese/silverstripe-display-logic
  [2]: https://github.com/unclecheese/silverstripe-display-logic/blob/master/README.md

## Grouped CMS Menu
The [Grouped CMS Menu Module][1] allows you to group CMS menu items into nested lists which expand when hovered over. This is useful when there are so many CMS menu items that screen space becomes an issue.

  [1]: https://github.com/silverstripe-australia/silverstripe-grouped-cms-menu

## Dashboard
The [Dashboard module][1] provides a splash page for the CMS in SilverStripe 3 with configurable widgets that display relevant information. Panels can be created and extended easily. The goal of the Dashboard module is to provide users with a launchpad for common CMS actions such as creating specific page types or browsing new content.

There are Images and videos about this module can be found in [this blog post][2].

There are some included Panels by default...

 - Recently edited pages
 - Recently uploaded files
 - RSS Feed
 - Quick links
 - Section editor
 - Google Analytics
 - Weather

When you have this module installed it creates a dashboard per member, so if you have a large amount of members which will never use the admin and performance becomes an issue I recommend creating the members with these extra settings before writing it...

    Member::create(array(
       'HasConfiguredDashboard' => 1
    ));

There is much more documentation in the [modules readme.md][3]

  [1]: https://github.com/unclecheese/silverstripe-dashboard
  [2]: https://www.silverstripe.org/blog/the-dashboard-module-make-a-splash-in-silverstripe-3/
  [3]: https://github.com/unclecheese/silverstripe-dashboard/blob/master/README.md

