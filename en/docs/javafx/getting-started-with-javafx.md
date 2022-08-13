---
title: "Getting started with javafx"
slug: "getting-started-with-javafx"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World program
The following code creates a simple user interface containing a single `Button` that prints a `String` to the console on click.

    import javafx.application.Application;
    import javafx.scene.Scene;
    import javafx.scene.control.Alert;
    import javafx.scene.control.Alert.AlertType;
    import javafx.scene.control.Button;
    import javafx.scene.layout.StackPane;
    import javafx.stage.Stage;
    
    public class HelloWorld extends Application {
    
        @Override
        public void start(Stage primaryStage) {
            // create a button with specified text
            Button button = new Button("Say 'Hello World'");
    
            // set a handler that is executed when the user activates the button
            // e.g. by clicking it or pressing enter while it's focused
            button.setOnAction(e -> {
               //Open information dialog that says hello
               Alert alert = new Alert(AlertType.INFORMATION, "Hello World!?");
               alert.showAndWait();
            });
    
            // the root of the scene shown in the main window
            StackPane root = new StackPane();
    
            // add button as child of the root
            root.getChildren().add(button);
    
            // create a scene specifying the root and the size
            Scene scene = new Scene(root, 500, 300);
    
            // add scene to the stage
            primaryStage.setScene(scene);
    
            // make the stage visible
            primaryStage.show();
        }
    
        public static void main(String[] args) {
            // launch the HelloWorld application.
    
            // Since this method is a member of the HelloWorld class the first
            // parameter is not required
            Application.launch(HelloWorld.class, args);
        }
    
    }

The `Application` class is the entry point of every JavaFX application. Only one `Application` can be launched and this is done using

    Application.launch(HelloWorld.class, args);

This creates a instance of the `Application` class passed as parameter and starts up the JavaFX platform.

The following is important for the programmer here:

1. First `launch` creates a new instance of the `Application` class (`HelloWorld` in this case). The `Application` class therefore needs a no-arg constructor.
2. `init()` is called on the `Application` instance created. In this case the default implementation from `Application` does nothing.
3. `start` is called for the `Appication` instance and the primary `Stage` (= window) is passed to the method. This method is automatically called on the JavaFX Application thread (Platform thread).
4. The application runs until the platform determines it's time to shut down. This is done when the last window is closed in this case.
5. The `stop` method is invoked on the `Application` instance. In this case the implementation from `Application` does nothing. This method is automatically called on the JavaFX Application thread (Platform thread).

In the `start` method the scene graph is constructed. In this case it contains 2 `Node`s: A `Button` and a `StackPane`.

The `Button` represents a button in the UI and the `StackPane` is a container for the `Button` that determines it's placement.

A `Scene` is created to display these `Node`s. Finally the `Scene` is added to the `Stage` which is the window that shows the whole UI.

## Installation or Setup
> The JavaFX APIs are available as a fully integrated feature of the
> Java SE Runtime Environment (JRE) and the Java Development Kit (JDK ).
> Because the JDK is available for all major desktop platforms (Windows,
> Mac OS X, and Linux), JavaFX applications compiled to JDK 7 and later
> also run on all the major desktop platforms. Support for ARM platforms
> has also been made available with JavaFX 8. JDK for ARM includes the
> base, graphics and controls components of JavaFX.

To install JavaFX install your chosen version of the Java Runtime environment and [Java Development kit][1].

Features offered by JavaFX include:

 1. Java APIs.
 2. FXML and Scene Builder.
 3. WebView.
 4. Swing interoperability.
 5. Built-in UI controls and CSS.
 6. Modena theme.
 7. 3D Graphics Features.
 8. Canvas API.
 9. Printing API.
 10. Rich Text Support.
 11. Multitouch Support.
 12. Hi-DPI support.
 13. Hardware-accelerated graphics pipeline.
 14. High-performance media engine.
 15. Self-contained application deployment model.

  [1]: http://www.oracle.com/technetwork/java/javase/downloads/index.html

