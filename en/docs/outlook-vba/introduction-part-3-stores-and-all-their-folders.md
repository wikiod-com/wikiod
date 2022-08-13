---
title: "Introduction Part 3 Stores and all their folders"
slug: "introduction-part-3-stores-and-all-their-folders"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Completes the introduction to stores and folders started in part 2 of this tutorial

**Expected prior knowledge**: You have studied part 2 of this tutorial or are already familiar with its contents. 

 


## 3. 0 Contents
  * How to reference any accessible folder.
  * How to get the full name of a referenced folder.
  * A pair of routines that together will list every folder within every accessible store.
  * A routine to move a folder from one parent folder to another.

## 3.1 Function GetFldrNames() which is needed for several of the demonstration macros
A number of the demonstration macros within this part requires a function which I will explain later.  For the moment, please just copy `GetFldrNames()` to a suitable module.  I use this function frequently and keep it, and other like it that I use in many different macros, in a module named “ModGlobalOutlook”.  You might like to do the same.  Alternatively, you might prefer to keep the macro with all the other macros within this tutorial series; you can move it later if you change your mind.  

    Public Function GetFldrNames(ByRef Fldr As Folder) As String()
    
      ' * Fldr is a folder. It could be a store, the child of a store,
      '   the grandchild of a store or more deeply nested.
      ' * Return the name of that folder as a string array in the sequence:
      '    (0)=StoreName (1)=Level1FolderName (2)=Level2FolderName  ...
      
      ' 12Oct16  Coded
      ' 20Oct16  Renamed from GetFldrNameStr and amended to return a string array
      '          rather than a string
      
      Dim FldrCrnt As Folder
      Dim FldrNameCrnt As String
      Dim FldrNames() As String
      Dim FldrNamesRev() As String
      Dim FldrPrnt As Folder
      Dim InxFN As Long
      Dim InxFnR As Long
    
      Set FldrCrnt = Fldr
      FldrNameCrnt = FldrCrnt.Name
      ReDim FldrNamesRev(0 To 0)
      FldrNamesRev(0) = Fldr.Name
      ' Loop getting parents until FldrCrnt has no parent.
      ' Add names of Fldr and all its parents to FldrName as they are found
      Do While True
        Set FldrPrnt = Nothing
        On Error Resume Next
        Set FldrPrnt = Nothing   ' Ensure value is Nothing if following statement fails
        Set FldrPrnt = FldrCrnt.Parent
        On Error GoTo 0
        If FldrPrnt Is Nothing Then
          ' FldrCrnt has no parent
          Exit Do
        End If
        ReDim Preserve FldrNamesRev(0 To UBound(FldrNamesRev) + 1)
        FldrNamesRev(UBound(FldrNamesRev)) = FldrPrnt.Name
        Set FldrCrnt = FldrPrnt
      Loop
    
      ' Copy names to FldrNames in reverse sequence so they end up in the correct sequence
      ReDim FldrNames(0 To UBound(FldrNamesRev))
      InxFN = 0
      For InxFnR = UBound(FldrNamesRev) To 0 Step -1
        FldrNames(InxFN) = FldrNamesRev(InxFnR)
        InxFN = InxFN + 1
      Next
    
      GetFldrNames = FldrNames
    
    End Function

 


## 3.2 Referencing a default folder
In `TestDefaultFldr()` I set `Fldr` to the default Inbox.  The constant `olFolderInbox` can be replaced by other values giving access to any of the default folders.  If you type `Set Fldr = Session.GetDefaultFolder(`, the VB editor will display a drop down list of all the possible values.

    Sub TestDefaultFldr()
    
      Dim Fldr As Folder
    
      Set Fldr = Session.GetDefaultFolder(olFolderInbox)
    
      Debug.Print Join(GetFldrNames(Fldr), "|")
    
    End Sub

On my laptop, `TestDefaultFldr()` displays `Outlook data file|Inbox` which came as a surprise.  I wrote `GetFldrNames(Fldr)` to make sure that the folder I had referenced was the one I wanted.  I had accessed the default Inbox and found it was empty!  Store “Output data file” came with the default installation and I had ignored it since Outlook had created a store for each of my email accounts.  It was only after discovering my empty default Inbox that I thought about how Outlook would know which of my email accounts was the account I would want as the default.  Of the standard Outlook folders, there is either no default or the default is within “Output data file”.  It may be possible to change which Inbox is the default Inbox but I have not investigated because I am not sure which of my email accounts I would make the default if I did change.  Just remember that all your Calendar Items, Tasks and so on are within “Outlook data file” and make sure you include “Outlook.pst” in your archive list.

Most Outlook objects have the property `Parent`.  `GetFldrNames(Fldr)` records the name of the folder in an array before trying to access its parent.  It loops adding names to the end of the array until it reaches the store. The store does not have a parent so the attempt to access it fails.  The sequence of names in the array is reversed and then returned to the caller.  I have used `Join` to turn the array of names into a displayable string.


## 3.3 Referencing any folder within any accessible store
`TestFldrChain()` demonstrates how to reference any folder within any accessible store:

    Sub TestFldrChain()
    
      Dim Fldr As Folder
    
      Set Fldr = Session.Folders("A").Folders("A2"). _
                               Folders("A21").Folders("A213")
    
      Debug.Print Join(GetFldrNames(Fldr), "|")
    
    End Sub    

In `TestFldrChain()`: A is the name of a store; A2 is the name of a folder within A; A21 is the name of a folder within A2 and A213 is the name of a folder within A21.

What is happening here?

`Session` has a property `Folders` which is a list of all accessible stores.     

`Session.Folders(integer)`, which I used in Part 2 of this tutorial, allows me to step through the stores in sequence when I do not know their names.  `Session.Folders("A")` allows me to access a folder when I know its name.

`Session.Folders("A")` is a folder and it too has a property `Folders`.  

`Session.Folders("A").Folders("A2")` gives me access to folder “A2” within store “A”.

I can chain as many `Folders("x")`s as necessary to reach any folder.  If the chain is too long for one line, you can split the statement across several lines as I have.

Look for the most deeply nested folder within your installation and replace A, A2, A21 and A213 by the names of your store and folders.  Increase or decrease the number of Folders in the chain as necessary.

If you update and run `TestFldrChain()`, it will output the following except that A, A2 and so on will have been replaced by your folder names:

    A|A2|A21|A213 



## 3.4 Listing the names of every folder within every accessible store
In Part 2, you were shown how to list every accessible store and the top level folders within each store.  This involved a loop through the stores and then a loop for each store through its folders Above you have seen how to reference a known folder at any depth within the hierarchy of folders.  This involved chaining together as many `Folders("x")`s as necessary to reach the folder.

I now want to list every folder, at any depth, within every store.  The easiest coding technique for solving this type of problem where you must move down chains of varying lengths is **recursion**.  If you are a serious programmer in another language or tool, you may already know about recursion.  If you have ambitions to be a serious programmer, you will need to understand recursion eventually but not necessarily today.  “Recursion” is one of those concepts that many find difficult to grasp at first.  You can type “Recursion” into your favourite search engine and read the various attempts at explaining this concept.  Alternatively, you can accept these macro work but not worry how they work.

Note the comment in `ListStoresAndAllFolders()`: these macros need a reference to “Microsoft Scripting Runtime”.  Click <kbd>Tools</kbd> in the tab bar at the top of the VB Editor window then click <kbd>References</kbd>.  You will get a list of all the available references (libraries).  Some at the top will already be ticked.  The remainder are in alphabetic order.  Scroll down the list and click the box to the left of “Microsoft Scripting Runtime” to get a tick.  Then click <kbd>OK</kbd>

     Sub ListStoresAndAllFolders()
    
      ' Displays the name of every accessible store
      ' Under each store, displays an indented list of all its folders
    
      ' Technique for locating desktop from answer by Kyle:
      ' http://stackoverflow.com/a/17551579/973283
    
      ' Needs reference to “Microsoft Scripting Runtime” if "TextStream"
      ' and "FileSystemObject" are to be recognised
    
      Dim FileOut As TextStream
      Dim FldrCrnt As Folder
      Dim Fso As FileSystemObject
      Dim InxFldrCrnt As Long
      Dim InxStoreCrnt As Long
      Dim Path As String
      Dim StoreCrnt As Folder
    
      Path = CreateObject("WScript.Shell").SpecialFolders("Desktop")
      
      Set Fso = CreateObject("Scripting.FileSystemObject")
      Set FileOut = Fso.CreateTextFile(Path & "\ListStoresAndAllFolders.txt", True)
    
      With Application.Session
        For InxStoreCrnt = 1 To .Folders.Count
          Set StoreCrnt = .Folders(InxStoreCrnt)
          With StoreCrnt
            FileOut.WriteLine .Name
            For InxFldrCrnt = .Folders.Count To 1 Step -1
              Set FldrCrnt = .Folders(InxFldrCrnt)
              Call ListAllFolders(FldrCrnt, 1, FileOut)
            Next
          End With
        Next
      End With
    
      FileOut.Close
    
    End Sub
    Sub ListAllFolders(ByRef Fldr As Folder, ByVal Level As Long, ByRef FileOut As TextStream)
    
      ' This routine:
      '  1. Output name of Fldr
      '  2. Calls itself for each child of Fldr
      ' It is designed to be called by ListStoresAndAllFolders()
    
      Dim InxFldrCrnt As Long
    
      With Fldr
        FileOut.WriteLine Space(Level * 2) & .Name
        For InxFldrCrnt = .Folders.Count To 1 Step -1
          Call ListAllFolders(.Folders(InxFldrCrnt), Level + 1, FileOut)
        Next
      End With
    
    End Sub

After you have run `ListStoresAndAllFolders`, there will be a new file on your DeskTop named “ListStoresAndAllFolders.txt” which will contain the promised list of stores and folders.


## 3.5 Moving a folder from one parent folder to another
Why do I want to reference a folder?  In the next part I will show you how to access emails within a referenced folder.  Here I will show you how to move a folder.  I created a folder named “Test” within my Inbox.  In `TestMoveFolder()`, I replaced “A” with the name of the store containing my Inbox.  Running `TestMoveFolder()` moved “Test” to “Deleted Items”.

    Sub TestMoveFolder()
    
      Dim FldrDest As Folder
      Dim FldrToMove As Folder
    
      Set FldrToMove = Session.Folders("A").Folders("Inbox").Folders("Test")
      Set FldrDest = Session.Folders("A").Folders("Deleted Items")
      
      FldrToMove.MoveTo FldrDest
    
    End Sub

## 3.6 What you should remember from this part of the tutorial
  * How to reference a default folder and the possible limitations of this technique.
  * How to reference any single folder at any depth within any accessible store. 
  * How to display the full name of a referenced folder.
  * How to reference one of the many, many available libraries that provide functionality beyond the default set of subroutines and functions.
  * How to display the name of every folder within every accessible store.
  * How to move a folder from one parent folder to another.


