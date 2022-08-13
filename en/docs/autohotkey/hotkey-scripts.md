---
title: "Hotkey Scripts"
slug: "hotkey-scripts"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Syntax
 - keybindings::
 - ::abbreviation::
 - Return

## Parameters
| Keybindings | Details |
| --------- | ------- |
| ^ | Ctrl key |
| ! | Alt key |
| \+ | Shift key |
| \# | Windows key |
| {enter} | send enter key |
| {tab} | send tab key |
| \* | wildcard, any key can be pressed down |
| ~ | key's native function will not be blocked |
| <symbol | specifies left key (<+ is left shift) |
| \>symbol | specifies right key |

## Hotstring
To make a script to replace a phrase use the `::abbreviation::` hotstring syntax. It will replace `btw` with `by the way` whenever you enter `btw` and then the space key.

    ::btw::by the way

If you wanted to make a login script to make logging in faster you could make a script like this (the file is not encrypted so any information in your script will be visible to anyone with access to the file).

    ::lmi::user{tab}password{enter}

## Context-sensitive Hotkeys and Hotstrings
In order to create a hotkey or hotstring that only triggers when certain windows are active or exist, you can put one or several of the following *directives* before the hotkey definition:

    #IfWinActive [, WinTitle, WinText]
    #IfWinExist [, WinTitle, WinText]
    #IfWinNotActive [, WinTitle, WinText]
    #IfWinNotExist [, WinTitle, WinText]

Example: You want `stackoverflow.com` to be sent whenever you type `so` (and a whitespace after that) in Google Chrome, but ignore the hotstring in any other window.

    #IfWinActive, ahk_class Chrome_WidgetWin_1
    ::so::stackoverflow.com


By using `#If [, Expression ]`, you can make a hotkey trigger only when an arbitrary expression is true, for example:

    #If A_Hour < 9
    F1::
        MsgBox, It is too early to ask for help!
    return



## Remap keys
The following example remaps the key <kbd>Z</kbd> to <kbd>Y</kbd> and vice versa, e.g. if you want to work with the QWERTY layout on a QWERTZ keyboard.

    z::y
    y::z

## Hotkey
To make a hotkey that sends the key sequence 'Hello World' from pressing <kbd>Ctrl</kbd> + <kbd>J</kbd> onto the active window (can be demonstrated in notepad, e.g.)

    ^j::
        Send, Hello World
    Return

## Multiple keypress
To run a script when multiple keys are pressed use the `&` between the keys.

    Numpad0 & Numpad1::
        MsgBox You pressed 0 and 1
    return

## Toggleable hotkeys
Following script enters predefined strings on hotkey presses if the scroll lock is active. 
This can be useful if you often paste a number of repeating strings.
Included hotkey for script refresh (for example if you need to edit paste-able strings).

    ; refresh script hotkey
    Numpad9:: 
        GetKeyState, state, ScrollLock, T
        if ( state = "D" )
            Reload
    Return

    Numpad1:: 
        GetKeyState, state, ScrollLock, T
        if ( state = "D" )
            Send,         Hello 
    Return

    Numpad2:: 
        GetKeyState, state, ScrollLock, T
        if ( state = "D" )
            Send,         World
    Return
    ;...

