---
title: "Getting started with Bash"
slug: "getting-started-with-bash"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
# Interactive Shell

The Bash shell is commonly used **interactively:** It lets you enter and edit commands, then executes them when you press the <kbd>Return</kbd> key. Many Unix-based and Unix-like operating systems use Bash as their default shell (notably Linux and macOS). The terminal automatically enters an interactive Bash shell process on startup.

Output `Hello World` by typing the following:
  
    echo "Hello World"
    #> Hello World  # Output Example

## Notes

 - You can change the shell by just typing the name of the shell in terminal. For example: `sh`, `bash`, etc.

 - [`echo`][2] is a Bash builtin command that writes the arguments it receives to the standard output.  It appends a newline to the output, by default.

----------------------

# Non-Interactive Shell

The Bash shell can also be run **non-interactively** from a script, making the shell require no human interaction. Interactive behavior and scripted behavior should be identical &ndash; an important design consideration of Unix V7 Bourne shell and transitively Bash. Therefore anything that can be done at the command line can be put in a script file for reuse.

Follow these steps to create a `Hello World` script:

1. Create a new file called `hello-world.sh`

       touch hello-world.sh

2. Make the script executable by running [`chmod`][3]` +x hello-world.sh`
3. Add this code:

       #!/bin/bash
       echo "Hello World"
       
   **Line 1**: The first line of the script must start with the character sequence `#!`, referred to as  *shebang*<sup>1</sup>. The shebang instructs the operating system to run `/bin/bash`, the Bash shell, passing it the script's path as an argument.

    E.g. `/bin/bash hello-world.sh`

   **Line 2**: Uses the [`echo`][2] command to write `Hello World` to the standard output.


4. Execute the `hello-world.sh` script from the command line using one of the following: 

   - `./hello-world.sh` &ndash; most commonly used, and recommended
   - `/bin/bash hello-world.sh`
   - `bash hello-world.sh` &ndash; assuming `/bin` is in your `$PATH`
   - `sh hello-world.sh`

For real production use, you would omit the `.sh` extension (which is misleading anyway, since this is a Bash script, not a `sh` script) and perhaps move the file to a directory within your [`PATH`](/documentation/bash/4797/internal-variables/16877/path) so that it is available to you regardless of your current working directory, just like a system command such as `cat` or `ls`.

Common mistakes include:  
1. Forgetting to apply execute permission on the file, i.e., `chmod +x hello-world.sh`, resulting in the output of `./hello-world.sh: Permission denied`.

2. Editing the script on Windows, which produces incorrect line ending characters that Bash cannot handle.

    A common symptom is `: command not found`  where the carriage return has forced the cursor to the beginning of line, overwriting the text before the colon in the error message.

    The script can be fixed using the `dos2unix` program.

    An example use: `dos2unix hello-world.sh`

    *`dos2unix` edits the file inline.*

 3. Using `sh ./hello-world.sh`, not realizing that `bash` and `sh` are distinct shells with distinct features (though since Bash is backwards-compatible, the opposite mistake is harmless).

    Anyway, simply relying on the script's shebang line is vastly preferable to explicitly writing `bash` or `sh` (or `python` or `perl` or `awk` or `ruby` or...) before each script's file name.

    A common shebang line to use in order to make your script more portable is to use `#!/usr/bin/env bash` instead of hard-coding a path to Bash.  That way, `/usr/bin/env` has to exist, but beyond that point, `bash` just needs to be on your `PATH`.  On many systems, `/bin/bash` doesn't exist, and you should use `/usr/local/bin/bash` or some other absolute path; this change avoids having to figure out the details of that.

---

<sup>1</sup> *Also referred to as sha-bang, hashbang, pound-bang, hash-pling.*


  [1]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
  [2]: https://www.gnu.org/software/bash/manual/bash.html#index-echo
  [3]: http://ss64.com/bash/chmod.html

## Hello World Using Variables
Create a new file called `hello.sh` with the following content and give it executable permissions with `chmod +x hello.sh`.

> Execute/Run via: `./hello.sh`

    #!/usr/bin/env bash
    
    # Note that spaces cannot be used around the `=` assignment operator
    whom_variable="World"
    
    # Use printf to safely output the data
    printf "Hello, %s\n" "$whom_variable"
    #> Hello, World

This will print `Hello, World` to standard output when executed.

 To tell bash where the script is you need to be very specific, by pointing it to the containing directory, normally with `./` if it is your working directory, where `.` is an alias to the current directory. If you do not specify the directory, `bash` tries to locate the script in one of the directories contained in the `$PATH` environment variable.

--------------

The following code accepts an argument `$1`, which is the first command line argument, and outputs it in a formatted string, following `Hello, `.

> Execute/Run via: `./hello.sh World`

    #!/usr/bin/env bash
    printf "Hello, %s\n" "$1"
    #> Hello, World

It is important to note that `$1` has to be quoted in double quote, not single quote. `"$1"` expands to the first command line argument, as desired, while `'$1'` evaluates to literal string `$1`.

> **Security Note:**  
> Read [**Security implications of forgetting to quote a variable in bash shells**](http://unix.stackexchange.com/q/171346/4667) to understand the importance of placing the variable text within double quotes.


## Hello World with User Input
The following will prompt a user for input, and then store that input as a string (text) in a variable. The variable is then used to give a message to the user.

    #!/usr/bin/env bash
    echo  "Who are you?"
    read name
    echo "Hello, $name."

The command `read` here reads one line of data from standard input into the variable `name`. This is then referenced using `$name` and printed to standard out using `echo`.

Example output:

    $ ./hello_world.sh
    Who are you?
    Matt
    Hello, Matt.

Here the user entered the name "Matt", and this code was used to say `Hello, Matt.`.

And if you want to append something to the variable value while printing it, use curly brackets around the variable name as shown in the following example:

    #!/usr/bin/env bash
    echo  "What are you doing?"
    read action
    echo "You are ${action}ing."

Example output:

    $ ./hello_world.sh
    What are you doing?
    Sleep
    You are Sleeping.

Here when user enters an action, "ing" is appended to that action while printing.

## Importance of Quoting in Strings
Quoting is important for string expansion in bash. With these, you can control how the bash parses and expands your strings. 

# There are two types of quoting: 

- **Weak**: _uses double quotes: "_
- **Strong**: _uses single quotes: '_

If you want to bash to expand your argument, you can use **Weak Quoting**:

    #!/usr/bin/env bash
    world="World"
    echo "Hello $world"
    #> Hello World

If you don't want to bash to expand your argument, you can use **Strong Quoting**:

    #!/usr/bin/env bash
    world="World"
    echo 'Hello $world'
    #> Hello $world

You can also use escape to prevent expansion:

    #!/usr/bin/env bash
    world="World"
    echo "Hello \$world"
    #> Hello $world

----

For more detailed information other than beginner details, you can continue to read it [here](https://www.wikiod.com/bash/quoting).

## Viewing information for Bash built-ins
    help <command>

This will display the Bash help (manual) page for the specified built-in.

For example, `help unset` will show:

> <!-- language: none -->
>     unset: unset [-f] [-v] [-n] [name ...]
>        Unset values and attributes of shell variables and functions.
>    
>        For each NAME, remove the corresponding variable or function.
>    
>        Options:
>          -f    treat each NAME as a shell function
>          -v    treat each NAME as a shell variable
>          -n    treat each NAME as a name reference and unset the variable itself
>            rather than the variable it references
>    
>        Without options, unset first tries to unset a variable, and if that fails,
>        tries to unset a function.
>    
>        Some variables cannot be unset; also see `readonly'.
>    
>        Exit Status:
>        Returns success unless an invalid option is given or a NAME is read-only.

To see a list of all built-ins with a short description, use

    help -d

## Hello World in "Debug" mode
    $ cat hello.sh 
    #!/bin/bash 
    echo "Hello World"
    $ bash -x hello.sh 
    + echo Hello World
    Hello World

The `-x` argument enables you to walk through each line in the script. One good example is here: 

    $ cat hello.sh
    #!/bin/bash 
    echo "Hello World\n" 
    adding_string_to_number="s"
    v=$(expr 5 + $adding_string_to_number) 
    
    $ ./hello.sh 
    Hello World

    expr: non-integer argument

The above prompted error is not enough to trace the script; however, using the following way gives you a better sense where to look for the error in the script.  

    $ bash -x hello.sh 
    + echo Hello World\n
    Hello World

    + adding_string_to_number=s
    + expr 5 + s
    expr: non-integer argument
    + v=


## Handling Named Arguments
<!-- language: lang-bash -->

    #!/bin/bash

    deploy=false
    uglify=false

    while (( $# > 1 )); do case $1 in
       --deploy) deploy="$2";;
       --uglify) uglify="$2";;
       *) break;
     esac; shift 2
    done

    $deploy && echo "will deploy... deploy = $deploy"
    $uglify && echo "will uglify... uglify = $uglify"

    # how to run
    # chmod +x script.sh
    # ./script.sh --deploy true --uglify false

