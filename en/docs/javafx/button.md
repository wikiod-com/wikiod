---
title: "Button"
slug: "button"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Adding a graphic to a button


## Create a Button
Creation of a `Button` is simple:

    Button sampleButton = new Button();

This will create a new `Button` without any text or graphic inside.

If you want to create a `Button` with a text, simply use the constructor that takes a `String` as parameter (which sets the [`textProperty`][1] of the `Button`):

    Button sampleButton = new Button("Click Me!");

If you want to create a `Button` with a graphic inside or any other `Node`, use this constructor:

    Button sampleButton = new Button("I have an icon", new ImageView(new Image("icon.png")));


  [1]: https://docs.oracle.com/javase/8/javafx/api/javafx/scene/control/Labeled.html#textProperty--

## Adding an action listener

Buttons fire action events when they are activated (e.g. clicked, a keybinding for the button is pressed, ...).

    button.setOnAction(new EventHandler<ActionEvent>() {
        @Override
        public void handle(ActionEvent event) {
            System.out.println("Hello World!");
        }
    });

If you are using Java 8+, you can use lambdas for action listeners.

    button.setOnAction((ActionEvent a) -> System.out.println("Hello, World!"));
    // or
    button.setOnAction(a -> System.out.println("Hello, World!"));


## Default and Cancel Buttons
`Button` API provides an easy way to assign common keyboard shortcuts to buttons without the need to access accelerators' list assigned to `Scene` or explicitly listening to the key events. Namely, two convenience methods are provided: `setDefaultButton` and `setCancelButton`:

 - Setting `setDefaultButton` to `true` will cause the `Button` to fire every time it receives a `KeyCode.ENTER` event.

 - Setting `setCancelButton` to `true` will cause the `Button` to fire every time it receives a `KeyCode.ESCAPE` event.

The following example creates a `Scene` with two buttons that are fired when enter or escape keys are pressed, regardless whether they are focused or not.

    FlowPane root = new FlowPane();
            
    Button okButton = new Button("OK");
    okButton.setDefaultButton(true);
    okButton.setOnAction(e -> {
        System.out.println("OK clicked.");
    });
            
    Button cancelButton = new Button("Cancel");            
    cancelButton.setCancelButton(true);
    cancelButton.setOnAction(e -> {
        System.out.println("Cancel clicked.");
    });
            
    root.getChildren().addAll(okButton, cancelButton);
    Scene scene = new Scene(root);

The code above will not work if these `KeyEvents` are consumed by any parent `Node`:

    scene.setOnKeyPressed(e -> {
        e.consume();
    });

