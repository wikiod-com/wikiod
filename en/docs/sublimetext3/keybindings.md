---
title: "Keybindings"
slug: "keybindings"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Keybindings are, as a lot of things in Sublime Text, `JSON`. Make sure you understand how to use them, they're going to save you a *lot* of time!


## Syntax
- keys: [list] a list of keystroke to press
- command: [string] the command to run
- args: [dict] the argument to pass to the command
- context: [list] a list of checker that will tell if the shortcut is enabled

Shortcuts *have* to be stored in a file called `Default.sublime-keymap` to be taken into account by Sublime Text.

## Platform specific

> What if I want to create some shortcuts only for OSX for example?

Well, you can. Just add ` (<platform>)` after the `Default`. Here are the 3 possibilities:

- `Default (Windows).sublime-keymap`
- `Default (Linux).sublime-keymap`
- `Default (OSX).sublime-keymap`

## Basic shortcut
Here is a simple shortcut that runs the command `upper_case` when you press <kbd>ctrl+u</kbd>.


```json
{
    "keys": ["ctrl+u"],
    "command": "upper_case"
}
```

> I've set the content of my keybindings like this, but it **doesn't work!**

It's normal! It's because it has to be a *list* of object (you probably want more than *one* shortcut, right?). Here's how it should look like:

```json
[
    {
        "keys": ["ctrl+u"],
        "command": "upper_case"
    }
]
```

Now it works!

