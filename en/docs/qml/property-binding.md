---
title: "Property binding"
slug: "property-binding"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

An object's property can be assigned a static value which stays constant until it is explicitly assigned a new value. 
However, to make the fullest use of QML and its built-in support for dynamic object behaviors, most QML objects use property bindings.

Property bindings are a core feature of QML that lets developers specify relationships between different object properties. When a property's dependencies change in value, the property is automatically updated according to the specified relationship.

## A more complicated example
In the simple example, we simply set the width of the rectangle to that of it's parent. Let's consider a more complicated example:

<!-- language: lang-js -->
    ApplicationWindow {
        visible: true
        width: 400
        height: 640
    
        Rectangle{
            id: rect
            anchors.centerIn: parent
            height: 100
            width: parent.width/2 + parent.width/3
            color: "blue"
        }
    }
In the example, we perform arithmetic operation on the value being binded. If you resize the running application window to maximum width, the gap between the rectangle and the application window will be wider and vice-versa.

## Basics about property bindings
Consider this simple example:

<!-- language: lang-js -->
    import QtQuick 2.7
    import QtQuick.Controls 2.0
    
    ApplicationWindow {
        visible: true
        width: 400
        height: 640
    
        Rectangle{
            id: rect
            anchors.centerIn: parent
            height: 100
            width: parent.width
            color: "blue"
        }
    }
In the above example, the width of `Rectangle` is bound to that of it's parent. If you change the width of the running application window, the width of rectangle also changes.

## Create Bindings with Dynamically Created QML Files
When using instances of QML files by directly declaring them, every `property` creates a binding. This is explained in the above examples.

This is how you dynamically create components:

```
var component = Qt.createComponent("Popup.qml");
var popup = component.createObject(parent, {"width": mainWindow.width, "height": mainWindow.height});
```

When the size of the `mainWindow` changes, the size of the created `PopUp` is not affected. To create a binding you set the size of the `popup` like this:

```
var component = Qt.createComponent("Popup.qml");
var options = {
    "width": Qt.binding(function() { return mainWindow.width }),
    "height": Qt.binding(function() { return mainWindow.height }),
};
var popup = component.createObject(parent, options);
```

Now the size of the `PopUp` will depend on `mainWindow`.

