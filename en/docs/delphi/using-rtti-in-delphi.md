---
title: "Using RTTI in Delphi"
slug: "using-rtti-in-delphi"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Delphi provided Runtime Type Information (RTTI) more than a decade ago. Yet even today many developers aren't fully aware of its risks and benefits.

In short, Runtime Type Information is information about an object's data type that is set into memory at run-time.

RTTI provides a way to determine if an object's type is that of a particular class or one of its descendants.

RTTI IN DELPHI - EXPLAINED

The [Run-Time Type Information In Delphi - Can It Do Anything For You?][1] article by Brian Long provides a great introduction to the RTTI capabilities of Delphi. Brian explains that the RTTI support in Delphi has been added first and foremost to allow the design-time environment to do its job, but that developers can also take advantage of it to achieve certain code simplifications.
This article also provides a great overview of the RTTI classes along with a few examples.

Examples include: Reading and writing arbitrary properties, common properties with no common ancestor, copying properties from one component to another, etc.


  [1]: http://www.blong.com/Conferences/BorConUK98/DelphiRTTI/CB140.htm

## Basic Class Information
This example shows how to obtain the ancestry of a component using the `ClassType` and `ClassParent` properties. It uses a button `Button1: TButton` and a list box `ListBox1: TListBox` on a form `TForm1`.

When the user clicks the button, the name of the button’s class and the names of its parent classes are added to the list box.

    procedure TForm1.Button1Click(Sender: TObject) ;
    var
      ClassRef: TClass;
    begin
       ListBox1.Clear;
       ClassRef := Sender.ClassType;
       while ClassRef <> nil do
       begin
         ListBox1.Items.Add(ClassRef.ClassName) ;
         ClassRef := ClassRef.ClassParent;
       end;
    end;

The list box contains the following strings after the user clicks the button:

 - TButton
 - TButtonControl
 - TWinControl
 - TControl
 - TComponent
 - TPersistent
 - TObject

