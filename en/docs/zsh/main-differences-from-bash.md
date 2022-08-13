---
title: "Main differences from bash"
slug: "main-differences-from-bash"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Aliases
# Global aliases
In bash, aliases can only be placed at the beginning of of a command, but zsh supports aliases anywhere. If you place the following line in your `$ZDOTDIR/.zshrc`

    alias -g G=' | grep -i'

You can then run

    cat haystack.txt G "needle"

# Suffix aliases (Added in zsh 4.2.x)
Suffix aliases allow you to tell zsh to open files with specify a program to open files with certain extensions. Examples:

    alias -s c="emacs"
    alias -s php="vim"
    alias -s java="$EDITOR"

Now in your shell, if you have a php file, `file.php`, and you run the command

    file.php

It will automatically open `file.php` in vim.

## Pipes and subshells
In bash, every command in a pipeline is executed in a subshell. In zsh, the last command in a pipeline is executed in current shell. 
For example, the following code
    
    var="before"
    echo "after" | read var
    echo $var

will print *before* in bash, but *after* in zsh.

## Differences in quoting
In bash, you have to quote arguments in order to preserve white space:

    # bash
    
    function print_first_argument {
        echo "$1"
    }

    argument="has white space"
    print_first_argument "$argument"

In Zsh, you don't need the quotes, because of different evaluation order:

    # zsh
    
    function print_first_argument {
        echo $1
    }

    argument="has white space"
    print_first_argument $argument



## Wildcard Handling
When nothing matches a wildcard such as `*` in bash,  it gets passed on as a literal `*` to the command, as if you had typed `\*`. However, zsh throws an error.

Bash:

    duncan@K7DXS-Laptop-Arch:~/test$ echo *.txt
    *.txt
    duncan@K7DXS-Laptop-Arch:~/test$ touch abc.txt
    duncan@K7DXS-Laptop-Arch:~/test$ echo *.txt
    abc.txt
    duncan@K7DXS-Laptop-Arch:~/test$ 

Zsh:

    K7DXS-Laptop-Arch% echo *.txt   
    abc.txt
    K7DXS-Laptop-Arch% rm abc.txt 
    K7DXS-Laptop-Arch% echo *.txt
    zsh: no matches found: *.txt
    K7DXS-Laptop-Arch% 

This is most noticeable in programs that use a literal `*`, such as find:

    duncan@K7DXS-Laptop-Arch:~/test$ ls -R
    .:
    abc
    
    ./abc:
    123.txt

Bash: 

    duncan@K7DXS-Laptop-Arch:~/test$ find -name *.txt
    ./abc/123.txt
    duncan@K7DXS-Laptop-Arch:~/test$ 

Zsh:

    K7DXS-Laptop-Arch% find -name *.txt
    zsh: no matches found: *.txt
    K7DXS-Laptop-Arch% find -name \*.txt
    ./abc/123.txt
    K7DXS-Laptop-Arch% find -name '*.txt' # Notice single rather than double quotes
    ./abc/123.txt
    K7DXS-Laptop-Arch% 

