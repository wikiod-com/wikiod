---
title: "Getting started with crystal-reports"
slug: "getting-started-with-crystal-reports"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
SAP Crystal Reports can be installed as a [standalone program][1] or [integrated into Visual Studio][2]. (SAP Crystal Reports for Visual Studio) - Both of which require very little effort outside of the installation wizard.


  [1]: http://www.crystalreports.com/
  [2]: http://go.sap.com/product/analytics/crystal-visual-studio.html

## Grouping records
Groups can be added and nested within a report to organize the data in a hierarchy of sorted lists. Outer groups supersede any groups within. This directly affects which records are affected by the `Previous` keyword.

 1. Select Insert, Group from the menu bar.
 2. Select the field to group the data by from the top drop-down list.
 3. Select the sort direction from the second drop-down list.
 4. Mark the Customize Group Name Field (only available in Crystal
    Reports 8.5) checkbox to show a different value in the group header.
 5. Click OK.

## Change the Language of Crystal Reports UI
It is possible to switch the language of Crystal Reports user interface. It may be useful to change the language to English before posting screenshots on StackOverflow.

You can switch between languages using `View | Product Locale`:

[![enter image description here][1]][1]

This menu shows all language packs that have been selected during initial product install.

To add additional language packs modify the installation of Crystal Reports. Open `Products and Features`, select your Crystal reports installation, then choose `Modify`. In the installation wizard choose all languages you would like to use.


  [1]: https://i.stack.imgur.com/v1UV0.png


