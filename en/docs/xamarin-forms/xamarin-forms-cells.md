---
title: "Xamarin.Forms Cells"
slug: "xamarinforms-cells"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## EntryCell
An EntryCell is a Cell that combines the capabilities of a Label and an Entry. The
EntryCell can be useful in scenarios when building some functionality within your application
to gather data from the user. They can easily be placed into a TableView and be treated as a
simple form.

**XAML**

    <EntryCell Label="Type Something"
    Placeholder="Here"/>

**Code**

    var entryCell = new EntryCell {
    Label = "Type Something",
    Placeholder = "Here"
    };

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/eF9s4.png

## SwitchCell
A SwitchCell is a Cell that combines the capabilities of a Label and an on-off switch. A
SwitchCell can be useful for turning on and off functionality, or even user preferences or
configuration options.

**XAML**

    <SwitchCell Text="Switch It Up!" />

**Code**

    var switchCell = new SwitchCell {
    Text = "Switch It Up!"
    };

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/QosL1.png

## TextCell
A TextCell is a Cell that has two separate text areas for displaying data. A TextCell is
typically used for information purposes in both TableView and ListView controls. The two text
areas are aligned vertically to maximize the space within the Cell. This type of Cell is also
commonly used to display hierarchical data, so when the user taps this cell, it will navigate to
another page.

**XAML**

    <TextCell Text="I am primary"
    TextColor="Red"
    Detail="I am secondary"
    DetailColor="Blue"/>

**Code**

    var textCell = new TextCell {
    Text = "I am primary",
    TextColor = Color.Red,
    Detail = "I am secondary",
    DetailColor = Color.Blue
    };

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/sk9q1.png

## ImageCell
An ImageCell is exactly what it sounds like. It is a simple Cell that contains only an Image.
This control functions very similarly to a normal Image control, but with far fewer bells and
whistles.

**XAML**

    <ImageCell ImageSource="http://d2g29cya9iq7ip.cloudfront.net/content/imag
    es/company/aboutus-video-bg.png?v=25072014072745")),
     Text="This is some text"
     Detail="This is some detail" />

**Code**

    var imageCell = new ImageCell {
    ImageSource = ImageSource.FromUri(new Uri("http://d2g29cya9iq7ip.clou
    109
    dfront.net/content/images/company/aboutus-videobg.png?v=25072014072745")),
     Text = "This is some text",
     Detail = "This is some detail"
    };

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/4THsm.png

## ViewCell
You can consider a ViewCell a blank slate. It is your personal canvas to create a Cell that
looks exactly the way you want it. You can even compose it of instances of multiple other View
objects put together with Layout controls. You are only limited by your imagination. And maybe
screen size.

**XAML**

    <ViewCell>
    <ViewCell.View>
    <StackLayout>
    <Button Text="My Button"/>
    
    <Label Text="My Label"/>
    <Entry Text="And some other stuff"/>
    </StackLayout>
    </ViewCell.View>
    </ViewCell>

**Code**

    var button = new Button { Text = "My Button" };
    var label = new Label { Text = "My Label" };
    var entry = new Entry { Text ="And some other stuff" };
    var viewCell = new ViewCell {
    View = new StackLayout {
    Children = { button, label, entry }
    }
    };

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/Yruxa.png

