---
title: "Animation"
slug: "animation"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Simple number animation
One of the very basic animations that you could come across is the `NumberAnimation`. This animation works by changing the numeric value of a property of an item from an initial state to a final state. Consider the following complete example:

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
            width: 100
            color: "blue"
            MouseArea{
                anchors.fill: parent
                onClicked: na.running = true
            }
    
            NumberAnimation {
                id: na    //ID of the QML Animation type  
                target: rect    //The target item on which the animation should run  
                property: "height"    //The property of the target item which should be changed by the animator to show effect  
                duration: 200    //The duration for which the animation should run  
                from: rect.height    //The initial numeric value of the property declared in 'property'
                to: 200    //The final numeric value of the property declared in 'property'
            }
        }
    }


## Behavior based animation
A behavior based animation allows you to specify that when a property changes the change should be animated over time.

    ProgressBar {
        id: progressBar
        from: 0
        to: 100
        Behavior on value {
            NumberAnimation {
                duration: 250
            }
        }
    }

In this example if anything changes the progress bar value the change will be animated over 250ms

