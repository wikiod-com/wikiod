---
title: "Programmable completion"
slug: "programmable-completion"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Simple completion using function
    _mycompletion() {
        local command_name="$1" # not used in this example
        local current_word="$2"
        local previous_word="$3" # not used in this example
        # COMPREPLY is an array which has to be filled with the possible completions
        # compgen is used to filter matching completions
        COMPREPLY=( $(compgen -W 'hello world' -- "$current_word") )
    }    
    complete -F _mycompletion mycommand

Usage Example:

    $ mycommand [TAB][TAB]
    hello world
    $ mycommand h[TAB][TAB]
    $ mycommand hello  

## Simple completion for options and filenames


