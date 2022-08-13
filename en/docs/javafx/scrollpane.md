---
title: "ScrollPane"
slug: "scrollpane"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

The ScrollPane is a control that offers a dynamic view of its content. This view is controlled in various ways; (increment-decrement button / mouse wheel) to have an integral view of the content.




## A) Fixed content's size :
The size of the content will be the same as that of its ScrollPane container.    
    
    import javafx.scene.control.ScrollPane;   //Import the ScrollPane
    import javafx.scene.control.ScrollPane.ScrollBarPolicy; //Import the ScrollBarPolicy
    import javafx.scene.layout.Pane;

    ScrollPane scrollpane;
    Pane content = new Pane();  //We will use this Pane as a content

    scrollpane = new ScrollPane(content);  //Initialize and add content as a parameter
    scrollpane.setPrefSize(300, 300);   //Initialize the size of the ScrollPane
    
    scrollpane.setFitToWidth(true);  //Adapt the content to the width of ScrollPane
    scrollpane.setFitToHeight(true); //Adapt the content to the height of ScrollPane
    
    
    scrollpane.setHbarPolicy(ScrollBarPolicy.ALWAYS);  //Control the visibility of the Horizontal ScrollBar
    scrollpane.setVbarPolicy(ScrollBarPolicy.NEVER);  //Control the visibility of the Vertical ScrollBar
    //There are three types of visibility (ALWAYS/AS_NEEDED/NEVER)

## B) Dynamic content's size :
The size of the content will change depending on the added elements that exceed the 
content limits in both axes (horizontal and vertical) that can be seen by moving through the view.

     

    import javafx.scene.control.ScrollPane;   //Import the ScrollPane
    import javafx.scene.control.ScrollPane.ScrollBarPolicy; //Import the ScrollBarPolicy
    import javafx.scene.layout.Pane;

    ScrollPane scrollpane;
    Pane content = new Pane();  //We will use this Pane as a content

    scrollpane = new ScrollPane();  
    scrollpane.setPrefSize(300, 300);   //Initialize the size of the ScrollPane
    content.setMinSize(300,300); //Here a minimum size is set so that the container can be extended.
    scrollpane.setContent(content); // we add the content to the ScrollPane

**Note :** 
Here we don't need both methods (setFitToWidth/setFitToHeight).

## Styling the ScrollPane :
The appearance of the ScrollPane can be easily changed, by having some notions of "*CSS*" and respecting some control "*properties*" and of course having some "*imagination*".

**A) The elements that make up ScrollPane :**

[![I do not own the original pic, it's belong to oracle][1]][1]

**B) CSS properties :**

    .scroll-bar:vertical .track{}

    .scroll-bar:horizontal .track{}

    .scroll-bar:horizontal .thumb{}
    
    .scroll-bar:vertical .thumb{}


    .scroll-bar:vertical *.increment-button,
    .scroll-bar:vertical *.decrement-button{}

    .scroll-bar:vertical *.increment-arrow .content, 
    .scroll-bar:vertical *.decrement-arrow .content{}

    .scroll-bar:vertical *.increment-arrow, 
    .scroll-bar:vertical *.decrement-arrow{}


    .scroll-bar:horizontal *.increment-button,
    .scroll-bar:horizontal *.decrement-button{}

    .scroll-bar:horizontal *.increment-arrow .content, 
    .scroll-bar:horizontal *.decrement-arrow .content{}

    .scroll-bar:horizontal *.increment-arrow, 
    .scroll-bar:horizontal *.decrement-arrow{}

    .scroll-pane .corner{}

    .scroll-pane{}



  [1]: https://i.stack.imgur.com/Uw9g8.png

