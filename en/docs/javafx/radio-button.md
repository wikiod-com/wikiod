---
title: "Radio Button"
slug: "radio-button"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Creating Radio Buttons
Radio Buttons allow you to let the user choose one element of those given. There are two ways to declare a `RadioButton` with a text besides it. Either by using the default constructor `RadioButton()` and setting the text with the `setText(String)` method or by using the other constructor `RadioButton(String)`.

    RadioButton radioButton1 = new RadioButton();
    radioButton1.setText("Select me!");
    RadioButton radioButton2= new RadioButton("Or me!");

As `RadioButton` is an extension of `Labeled` there can also be an `Image` specified to the `RadioButton`. After creating the `RadioButton` with one of the constructors simply add the `Image` with the `setGraphic(ImageView)` method like here:

    Image image = new Image("ok.jpg");
    RadioButton radioButton = new RadioButton("Agree");
    radioButton.setGraphic(new ImageView(image));

## Use Groups on Radio Buttons
A `ToggleGroup` is used to manage the `RadioButton`s so that just one in each group can be selected at each time. 

Create a simple `ToggleGroup` like following:

    ToggleGroup group = new ToggleGroup();

After creating a `Togglegroup` it can be assigned to the `RadioButton`s by using `setToggleGroup(ToggleGroup)`. Use `setSelected(Boolean)` to pre-select one of the `RadioButton`s.

    RadioButton radioButton1 = new RadioButton("stackoverlow is awesome! :)");
    radioButton1.setToggleGroup(group);
    radioButton1.setSelected(true);

    RadioButton radioButton2 = new RadioButton("stackoverflow is ok :|");
    radioButton2.setToggleGroup(group);
 
    RadioButton radioButton3 = new RadioButton("stackoverflow is useless :(");
    radioButton3.setToggleGroup(group);


## Events for Radio Buttons
Typically, when one of the `RadioButton`s in a `ToggleGroup` is selected the application performs an action. Below is an example which prints the user data of the selected `RadioButton` which has been set with `setUserData(Object)`.

    radioButton1.setUserData("awesome")
    radioButton2.setUserData("ok");
    radioButton3.setUserData("useless");
    
    ToggleGroup group = new ToggleGroup();
    group.selectedToggleProperty().addListener((obserableValue, old_toggle, new_toggle) -> {
        if (group.getSelectedToggle() != null) {
            System.out.println("You think that stackoverflow is " + group.getSelectedToggle().getUserData().toString());
        }
    });         

## Requesting focus for Radio Buttons
Let's say the second `RadioButton` out of three is pre-selected with `setSelected(Boolean)`, the focus is still at the first `RadioButton` by default. To change this use the `requestFocus()` method. 

    radioButton2.setSelected(true);
    radioButton2.requestFocus();

