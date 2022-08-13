---
title: "Useful Shortcuts"
slug: "useful-shortcuts"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Basic navigation
# Go to editor (from tool window)

<kbd>Esc</kbd>

# Switching focus to corresponding tool window

## Windows: <kbd>Alt</kbd> + &lt;tool window number&gt;   
## OS X / macOS: <kbd>Cmd</kbd> + &lt;tool window number&gt;  

# For example switching focus to the project window

## Windows: <kbd>Alt</kbd> + <kbd>1</kbd>  
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>1</kbd>  

# Recent files popup

## Windows: <kbd>Ctrl</kbd> + <kbd>E</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>E</kbd>

# Find Action

## Windows: <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>A</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Shift</kbd> + <kbd>A</kbd>

# Navigate to
* # File : 
    + ## Windows: <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>N</kbd>
    + ## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Shift</kbd> + <kbd>N</kbd>

* # Class : 
    + ## Windows: <kbd>Ctrl</kbd> + <kbd>N</kbd>
    + ## OS X / macOS: <kbd>Cmd</kbd> + <kbd>N</kbd>

* # Symbol (class/method/variable/constant name) :

    + ## Windows: <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>Shift</kbd> + <kbd>N</kbd>
    + ## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Option</kbd> + <kbd>Shift</kbd> + <kbd>N</kbd>

    Note that you can use class name to narrow down the method/variable/constant     search, for example to find symbol `usersCollection` in class `UserDAO` type:

        UserDAO.usersCollection

* # Everywhere : 
    + ## Windows: <kbd>Shift</kbd> + <kbd>Shift</kbd>
    + ## OS X / macOS: <kbd>Shift</kbd> + <kbd>Shift</kbd>

To search for something that has multiple words, e.g., `InetAddressCachePolicy 
` you can just type `InAddCacPo` or something similar that contains parts of words in the whole name.  
# Go to line number
## Windows: <kbd>Ctrl</kbd> + <kbd>G</kbd>

## OS X / macOS: <kbd>Cmd</kbd> + <kbd>L</kbd>

# Go back to last edit location
## Windows: <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>Backspace</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Shift</kbd> + <kbd>Backspace</kbd>

## Refactoring
# Copy
<kbd>F5</kbd>

# Move
<kbd>F6</kbd>

# Safe delete
## Windows / Linux: <kbd>Alt</kbd> + <kbd>Delete</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Delete</kbd>
Note that the <kbd>Delete</kbd> key on OS X / macOS is the equivalent of the <kbd>Backspace</kbd> key on other operating systems.

# Rename
<kbd>Shift</kbd>+ <kbd>F6</kbd>

# Extract Method
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>M</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Option</kbd> + <kbd>M</kbd>

# Extract Field
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>F</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Option</kbd> + <kbd>F</kbd>

# Extract Variable
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>V</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Option</kbd> + <kbd>V</kbd>

# Extract Constant
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>C</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Option</kbd> + <kbd>C</kbd>

# Extract Parameter
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>P</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Option</kbd> + <kbd>P</kbd>

## Selection

**Selection with increasing scope**

This comes handy when you want to select a block to extract a variable / method etc, no need to do a precise bracket matching, just put the caret somewhere in the statement and keep doing this
## Windows: <kbd>Ctrl</kbd> + <kbd>W</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>W</kbd>

**Selection with decreasing scope**

## Windows: <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>W</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Shift</kbd> + <kbd>W</kbd>

This feature is also very useful when editing / playing with json documents in your IDE. 

**Vertical selection** 

Press and hold 
## Windows: <kbd>Alt</kbd>
## OS X / macOS: <kbd>Opt</kbd>

and select normally using mouse / trackpad (the way you select a word in a row etc)  

This is how it should look like

<img src="http://i.stack.imgur.com/pwFrQ.png" width="260" height="145" />


**Multiple carets** 

Press and hold 
## Windows: <kbd>Alt</kbd> + <kbd>Shift</kbd>
## OS X / macOS: <kbd>Opt</kbd> + <kbd>Shift</kbd>

and click where all you want to put a caret. You can choose to put multiple carets in a single line or across lines at different positions. 

Now you can perform all operations that you would have been able to perform on a single selected word (hold <kbd>Ctrl</kbd> (windows) or <kbd>option</kbd> (mac OS) and use <kbd>Left</kbd> or <kbd>Right</kbd> keys to jump across words) and all those will affect all caret positions. 

You can even cut  / paste multiple selections from one place to another. 

<img src="http://i.stack.imgur.com/8g1of.png" width="254" height="123" />

Having multiple carets is very usefult when you want to change the structure of text across many lines / many positions in same line. 

**Selecting duplicate occurences**

Select some text and press
## Windows: <kbd>Alt</kbd> + <kbd>J</kbd>
## OS X / macOS: <kbd>ctrl</kbd> + <kbd>G</kbd>  

to select the next occurance of the same text. 

You get one caret at each of the selected occurrence that could be used to change each occurrence simultaneously. 

E.g., I've tried to put an example in this gif, hope it helps 

<img src="http://i.stack.imgur.com/ah2ON.gif" width="289" height="139" />


## Show Method Parameters
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>P</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>P</kbd>

Shows what parameters a method and all of its overloads accepts.

[![Method Parameters][1]][1]


  [1]: http://i.stack.imgur.com/c5MgX.png

## Compile and Run
# Make project (compile modifed and dependent)
## Windows: <kbd>Ctrl</kbd> + <kbd>F9</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>F9</kbd>

# Compile selected file, package or module
This is useful to know, as when debugging this shortcut can be used to quickly reload / hotswap classes.
## Windows: <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>F9</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Shift</kbd> + <kbd>F9</kbd>

# Select configuration and run

## Windows: <kbd>Alt</kbd> + <kbd>Shift</kbd> + <kbd>F10</kbd>
## OS X / macOS: <kbd>Option</kbd> + <kbd>Shift</kbd> + <kbd>F10</kbd>

# Select configuration and debug

## Windows: <kbd>Alt</kbd> + <kbd>Shift</kbd> + <kbd>F9</kbd>
## OS X / macOS: <kbd>Option</kbd> + <kbd>Shift</kbd> + <kbd>F9</kbd>

# Run
<kbd>Shift</kbd> + <kbd>F10</kbd>

# Debug
<kbd>Shift</kbd> + <kbd>F9</kbd> 

# Run context configuration from editor

## Windows: <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>F10</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Shift</kbd> + <kbd>F10</kbd>

## Code Completion
# Basic code completion (the name of any class, method or variable)
## Windows: <kbd>Ctrl</kbd> + <kbd>Space</kbd>  
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Space</kbd>  

# Smart code completion (filters the list of methods and variables by expected type)
## Windows: <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>Space</kbd>  
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Shift</kbd> + <kbd>Space</kbd>  

# Overwriting code with a suggestion
<kbd>Tab</kbd>

# Adding code from a completion suggestion
<kbd>Enter</kbd>

## Search/Replace
# Search everywhere
Double <kbd>Shift</kbd>

# Find
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>F</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>F</kbd>

# Find next
<kbd>F3</kbd>

# Find previous
<kbd>Shift</kbd> + <kbd>F3</kbd>

# Replace
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>R</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>R</kbd>

# Find in path
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>F</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Shift</kbd> + <kbd>F</kbd>

# Replace in path
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>R</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Shift</kbd> + <kbd>R</kbd>

## Other
# Surround with
Surrounds a code block with an `if`, `for`, `<editor-fold ...>` and more.
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>T</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Option</kbd> + <kbd>T</kbd>

## Usage Search
# Find usages / Find usages in file
## Windows / Linux: <kbd>Alt</kbd> + <kbd>F7</kbd> / <kbd>Ctrl</kbd> + <kbd>F7</kbd>
## OS X / macOS: <kbd>Option</kbd> + <kbd>F7</kbd> / <kbd>Ctrl</kbd> + <kbd>F7</kbd>

# Highlight usages in file
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>F7</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Shift</kbd> + <kbd>F7</kbd>

# Show usages
## Windows / Linux: <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>F7</kbd>
## OS X / macOS: <kbd>Cmd</kbd> + <kbd>Option</kbd> + <kbd>F7</kbd>

