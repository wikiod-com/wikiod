---
title: "Introduction Part 2 Stores and top-level folders"
slug: "introduction-part-2-stores-and-top-level-folders"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

First part of an introduction to stores and the folders they contain. Contains macros to display (1) the names of accessible stores and (2) the names of accessible stores and the top level folders within them.


## 2.1 Expected prior knowledge

  * You are an Outlook user and understand terms such as “email”, “received time”, “subject” and “Folder Pane”.
  * You know how to access Outlook’s Visual Basic Editor and create a module. See Introduction Part 1 if necessary.
  * You have at least a basic knowledge of VBA. I declare Subroutines and variables without explanation. I use Withs, Ifs and Loops without explanation. I tell you something is a collection. I tell you to copy code to a module and run it. There are many online tutorials although most are for Excel VBA and concentrate more on using the language with Excel than on the language. Searching for “VBA tutorial” brings up some that concentrate on the language more than the application that look satisfactory.
  * You are not required to know the Outlook Object Model; this tutorial introduces you to a small part of it.

## 2.2 Stores
Outlook stores emails, calendar items, notes, tasks and so on in files known as **Stores**. If you look at your Folder Pane you will see something like:

    Aaaaaaaaaa
      Inbox
      Drafts
      Deleted Items
      :  :
    
    Bbbbbbbbbb
      Inbox
      Drafts
      Deleted Items
      :  :
    
    Cccccccccc
    :  :

"Aaaaaaaaaa", "Bbbbbbbbbb" and "Cccccccccc" are the user or display names of Stores. I have always accepted Outlook defaults for these names which have changed over the years. Once the default was my name now it is my email address. The filename for these stores may be the same but with an extension such as PST or OST or may be something completely different. A VBA macro needs the user name to access a store and is not concerned with the file names or the extension.

You can have as many stores as you wish. I have “Outlook data file” which was created for me when I installed Outlook. When I added accounts for my email addresses, Outlook created new stores named for the email address such as “JohnDoe@hotmail.com” and “DoeJohn@gmail.com”. To reduce the size of my main store I save old emails in stores with names such “Archive 2015”.

If you are a business user, you may have access to shared stores or to the stores of colleagues.

The macros below show three different ways of listing the stores you can access. I suggest you create a new module to hold the code below and to use <kbd>F4</kbd> to access the module’s properties so you can be name it as “ModIntro” or some other name of your choice. If you completed Part 1 of this series, you will already have such a module.

Copy these macros to a module and test that each gives the same output.

    Sub ListStores1()
    
      Dim InxStoreCrnt As Integer
      Dim NS As NameSpace
      Dim StoresColl As Folders
    
      Set NS = CreateObject("Outlook.Application").GetNamespace("MAPI")
      Set StoresColl = NS.Folders
    
      For InxStoreCrnt = 1 To StoresColl.Count
        Debug.Print StoresColl(InxStoreCrnt).Name
      Next
    
    End Sub
    Sub ListStores2()
     
     Dim StoresColl As Stores
     Dim StoreCrnt As Store
     
     Set StoresColl = Session.Stores
     
     For Each StoreCrnt In StoresColl
       Debug.Print StoreCrnt.DisplayName
     Next
     
    End Sub
    Sub ListStores3()
    
      Dim InxStoreCrnt As Long
    
      With Application.Session
        For InxStoreCrnt = 1 To .Folders.Count
          Debug.Print .Folders(InxStoreCrnt).Name
        Next
      End With
    
    End Sub

You will find with VBA that there are often several methods of achieving the same effect. Above I have shown three methods of accessing the stores. You do not need to remember them all – pick your own favourite – but you do need to be aware that there are several methods because other people, whose code you may need to study, will have different favourites.

The variables `StoresColl` in macros `ListStores1()` and `ListStores2()` are both collections but hold different types of object: `Store` and `Folder`. A `Store` object can only reference a file on your disc. A `Folder` can reference a file on disc but can also reference folders within a store such as “Inbox” and “Sent Items”. `Stores`, `Folders`, `Store` and `Folder` are all part of the Outlook Object Model. This tutorial series introduces you to the model but it is not a formal definition. If you want a formal definition, type “outlook vba object model” into your favourite search engine. Make sure you look at the VBA version of the model.




## 2.3 Top level folders
In my Folder Pane example above, I only list three standard folders: “Inbox”, “Drafts” and “Deleted Items”. There are other standard folders and you can create as many folders of your own  as you wish. Some people create folders under Inbox but I prefer to create new folders at the same level as Inbox. Your folders can have sub-folders which can have their own sub-folders to any depth.

The following macro will produce a listing of the form:

    A
       A1
       A2
       A3
    B
       B1
       B2
    C
       C1
       C2
       C3
       C4

where A, B and C are stores and A1, B1, C1 and so on are folders within A, B and C. If A1, B1, C1 and so on have sub-folders, they will not be listed by this macro. Accessing more deeply nested folders will be covered in the next part of this tutorial.

    Sub ListStoresAndTopLevelFolders()
    
      Dim FldrCrnt As Folder
      Dim InxFldrCrnt As Long
      Dim InxStoreCrnt As Long
      Dim StoreCrnt As Folder
    
      With Application.Session
        For InxStoreCrnt = 1 To .Folders.Count
          Set StoreCrnt = .Folders(InxStoreCrnt)
          With StoreCrnt
            Debug.Print .Name
            For InxFldrCrnt = .Folders.Count To 1 Step -1
              Set FldrCrnt = .Folders(InxFldrCrnt)
              With FldrCrnt
                Debug.Print "   " & .Name
              End With
            Next
          End With
        Next
      End With
    
    End Sub

## 2.4 What you should remember from this tutorial
  * A store is a file in which Outlook stores emails, calendar items, notes, tasks and so on.
  * A store may contain Outlook standard folders such as “Inbox” and “Sent Items”.
  * A store may also contain user created folders.
  * Both Outlook standard folders and user created folders may contain user created sub-folders, sub-sub-folders and so on to any depth.
  * How to list stores.
  * How to list stores and the top level folders within those stores.

Confession: I do not remember either of the “Hows”. I have subroutines and functions that remember for me.

