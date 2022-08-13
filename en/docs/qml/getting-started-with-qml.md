---
title: "Getting started with qml"
slug: "getting-started-with-qml"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
A simple application showing the text "Hello World" in the center of the window. 

<!-- language: lang-js -->
    import QtQuick 2.3
    import QtQuick.Window 2.0
    
    Window {
        visible: true
        width: 640
        height: 480
        title: qsTr("Hello World") //The method qsTr() is used for translations from one language to other.
    
        Text {
            text: qsTr("Hello World")
            anchors.centerIn: parent
        }
    }



## Installation
QML comes with newer Version of the cross-platform application framework [Qt][1].
You can find the newest Version of Qt in the [Downloads section][2].

To create a new QML Project in the [Qt Creator IDE][3], select "File -> New ..." and under "Applications" select "Qt Quick-Application".
After clicking "select" you can now name and set the path for this project. After hitting "next" you can select which components you want to use, if unsure just leave the default and click on "next".
The two next steps will allow you to setup up a Kit and Source Control if you want to, otherwise keep the default settings.

You now have created a simple and ready to use QML application.

  [1]: https://www.qt.io/
  [2]: https://www.qt.io/download-open-source/#section-2
  [3]: https://www.qt.io/ide/

## Creating a simple button
You can easily transform every component in a clickable button using the MouseArea  component. The code below displays a 360x360 window with a button and a text in the center; pressing the button will change the text:

<!-- language: lang-js -->
    import QtQuick 2.0

    Rectangle {
        width: 360
        height: 360

        Rectangle {
            id: button
    
            width: 100
            height: 30
            color: "red"
            radius: 5     // Let's round the rectangle's corner a bit, so it resembles more a button
            anchors.centerIn: parent
    
            Text {
                id: buttonText
                text: qsTr("Button")
                color: "white"
                anchors.centerIn: parent
            }
    
            MouseArea {
                // We make the MouseArea as big as its parent, i.e. the rectangle. So pressing anywhere on the button will trigger the event
                anchors.fill: parent
    
                // Exploit the built-in "clicked" signal of the MouseArea component to do something when the MouseArea is clicked.
                // Note that the code associated to the signal is plain JavaScript. We can reference any QML objects by using their IDs
                onClicked: {
                    buttonText.text = qsTr("Clicked");
                    buttonText.color = "black";
                }
            }
        }
    }


## Display an image
This example shows the simplest usage of the Image component to display an image.

The Image `source` property is a [url type][1] that can be either a file with an absolute or relative path, an internet URL (`http://`) or a [Qt resource][2] (`qrc:/`)

    import QtQuick 2.3

    Rectangle {
        width: 640
        height: 480

        Image {
             source: "image.png"
        }
    }


  [1]: http://doc.qt.io/qt-5/qml-url.html
  [2]: http://doc.qt.io/qt-5/resources.html

## Mouse Event
This example shows how mouse event is used in QML. 

    import QtQuick 2.7
    import QtQuick.Window 2.2
    
    Window {
        visible: true
        Rectangle {
            anchors.fill: parent
            width: 120; height: 240
            color: "#4B7A4A"
    
            MouseArea {
                anchors.fill: parent // set mouse area (i.e. covering the entire rectangle.)
                acceptedButtons:  Qt.AllButtons
                onClicked: {
                    // print to console mouse location
                    console.log("Mouse Clicked.")
                    console.log("Mouse Location: <",mouseX,",",mouseY,">")
    
                    //change Rectangle color
                    if ( mouse.button === Qt.RightButton )
                        parent.color = 'blue'
                    if ( mouse.button === Qt.LeftButton )
                        parent.color = 'red'
                    if ( mouse.button === Qt.MiddleButton )
                        parent.color = 'yellow'
                }
                onReleased: {
                    // print to console
                    console.log("Mouse Released.")
                }
                onDoubleClicked: {
                    // print to console
                    console.log("Mouse Double Clicked.")
                }
    
            }
        }
    
    
    }




