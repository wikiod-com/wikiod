---
title: "AppleScript User Interaction"
slug: "applescript-user-interaction"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
- **display dialog** *text* [default answer *text*] [hidden answer *boolean*] [buttons *list of text*] [default button *text/integer*] [cancel button *text/integer*] [with title *text*] [with icon *text/integer/stop/‌note/‌caution/file*] [giving up after *integer*]
- **display alert** *text* [message *text*] [as *critical/informational/warning*] [buttons *list of text*] [default button *text/integer*] [cancel button *text/integer*] [giving up after *integer*]
- **display notification** *text* [with title *text*] [subtitle *text*] [sound name *text*]
- **choose file** [with prompt *text*] [of type *text*] [default location *alias*] [invisibles *boolean*] [multiple selections allowed *boolean*] [showing package contents *boolean*]
- **choose folder** [with prompt *text*] [default location *alias*] [invisibles *boolean*] [multiple selections allowed *boolean*] [showing package contents *boolean*]
- **choose from list** [with title *text*] [with prompt *text*] [default items *list of text/number*] [OK button name *text*] [cancel button name *text*] [multiple selections allowed *boolean*] [empty selection allowed *boolean]*
- **choose URL** [showing *list of Web/‌FTP/‌Telnet/‌File/‌News/‌Directory/‌Media/‌Remote*] [editable URL *boolean*]
- **choose color** [default color *RGB color*]

AppleScript User Interaction is part of Standard Additions. You can find the full documentation in the dictionary StandardAdditions.sdef through Script Editor's Open Dictionary.

## Display a dialog or alert
AppleScript can display dialogs and alerts to the user. Dialogs are for optionally requesting user input.

    display dialog "Hello World"
    display alert "Hello World"

<img src="http://i.stack.imgur.com/uCWfF.png" width="320">
<img src="http://i.stack.imgur.com/HisEe.png" width="320">

You can customise the buttons of either using `buttons` and passing a list of text.

    display dialog "Hello World" buttons {"button one", "button two", "button three"}

<img src="http://i.stack.imgur.com/7Znih.png" width="420">

Upon clicking a button, the button clicked is returned:

    {button returned:"button one"}

