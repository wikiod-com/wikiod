---
title: "Here documents and here strings"
slug: "here-documents-and-here-strings"
draft: false
images: []
weight: 9828
type: docs
toc: true
---

## Execute command with here document
    ssh -p 21 example@example.com <<EOF
      echo 'printing pwd'
      echo "\$(pwd)"
      ls -a
      find '*.txt'
    EOF

`$` is escaped because we do not want it to be expanded by the current shell i.e `$(pwd)` is to be executed on the remote shell.

Another way:

    ssh -p 21 example@example.com <<'EOF'
      echo 'printing pwd'
      echo "$(pwd)"
      ls -a
      find '*.txt'
    EOF    

*Note*: The closing EOF **should** be at the beginning of the line (No whitespaces before). If indentation is required, tabs may be used if you start your heredoc with `<<-`. See the [Indenting here documents](https://www.wikiod.com/bash/here-documents-and-here-strings#Indenting here documents) and [Limit Strings](https://www.wikiod.com/bash/here-documents-and-here-strings#Limit Strings) examples for more information.

## Indenting here documents
You can indent the text inside here documents with tabs, you need to use the `<<-` redirection operator instead of `<<`:

    $ cat <<- EOF
        This is some content indented with tabs `\t`.
        You cannot indent with spaces you __have__ to use tabs.
        Bash will remove empty space before these lines.
        __Note__: Be sure to replace spaces with tabs when copying this example.
    EOF

    This is some content indented with tabs _\t_.
    You cannot indent with spaces you __have__ to use tabs.
    Bash will remove empty space before these lines.
    __Note__: Be sure to replace spaces with tabs when copying this example.

One practical use case of this (as mentioned in `man bash`)
is in shell scripts, for example:

    if cond; then
        cat <<- EOF
        hello
        there
        EOF
    fi

It is customary to indent the lines within code blocks as in this `if` statement, for better readability.
Without the `<<-` operator syntax, we would be forced to write the above code like this:

    if cond; then
        cat << EOF
    hello
    there
    EOF
    fi

That's very unpleasant to read, and it gets much worse in a more complex realistic script.

## Here strings
<!-- if version [gte 2.05b] -->

You can feed a command using here strings like this:

    $ awk '{print $2}' <<< "hello world - how are you?"
    world

    $ awk '{print $1}' <<< "hello how are you
    > she is fine"
    hello
    she

You can also feed a `while` loop with a here string:

    $ while IFS=" " read -r word1 word2 rest
    > do
    > echo "$word1"
    > done <<< "hello how are you - i am fine"
    hello

<!-- end version if -->


## Create a file
A classic use of here documents is to create a file by typing its content:

    cat > fruits.txt << EOF
    apple
    orange
    lemon
    EOF

The here-document is the lines between the `<< EOF` and `EOF`.

This here document becomes the input of the `cat` command.
The `cat` command simply outputs its input,
and using the output redirection operator `>` we redirect to a file `fruits.txt`.

As a result, the `fruits.txt` file will contain the lines:

    apple
    orange
    lemon

The usual rules of output redirection apply:
if `fruits.txt` did not exist before, it will be created.
If it existed before, it will be truncated.

## Run several commands with sudo
    sudo -s <<EOF
      a='var'
      echo 'Running serveral commands with sudo'
      mktemp -d
      echo "\$a"
    EOF

* `$a` needs to be escaped to prevent it to be expanded by the current shell

Or

    sudo -s <<'EOF'
      a='var'
      echo 'Running serveral commands with sudo'
      mktemp -d
      echo "$a"
    EOF

## Limit Strings
A heredoc uses the _limitstring_ to determine when to stop consuming input. The terminating limitstring **must**
- Be at the start of a line.
- Be the only text on the line
**Note:** If you use `<<-`  the limitstring can be prefixed with tabs `\t`

Correct:

    cat <<limitstring
    line 1
    line 2
    limitstring

This will output:

>     line 1
>     line 2

Incorrect use:

    cat <<limitstring
    line 1
    line 2
     limitstring
    
Since `limitstring` on the last line is not exactly at the start of the line, the shell will continue to wait for further input, until it sees a line that starts with `limitstring` and doesn't contain anything else. Only then it will stop waiting for input, and proceed to pass the here-document to the `cat` command.

Note that when you prefix the initial limitstring with a hyphen, any tabs at the start of the line are removed before parsing, so the data and the limit string can be indented with tabs (for ease of reading in shell scripts).

    cat <<-limitstring
            line 1    has a tab each before the words line and has
                line 2 has two leading tabs
            limitstring

will produce 

>     line 1    has a tab each before the words line and has
>     line 2 has two leading tabs

with the leading tabs (but not the internal tabs) removed.

