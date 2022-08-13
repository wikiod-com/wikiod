---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9828
type: docs
toc: true
---

## Syntax
 - Define a function with the `function` keyword:

       function f {
    }
 - Define a function with `()`:

       f(){
    }
 - Define a function with both the `function` keyword and `()`:

       function f(){
    }


## Functions with arguments
In `helloJohn.sh`:

```
#!/bin/bash

greet() {
  local name="$1"
  echo "Hello, $name"
}

greet "John Doe"
```

```
# running above script
$ bash helloJohn.sh
Hello, John Doe
```

1. If you don't modify the argument in any way, there is no need to copy it to a `local` variable - simply `echo "Hello, $1"`.

2. You can use `$1`, `$2`, `$3` and so on to access the arguments inside the function.
    > **Note:** for arguments more than 9 `$10` won't work (bash will read it as **$1**0), you need to do `${10}`, `${11}` and so on.

3. `$@` refers to all arguments of a function:

    ```
    #!/bin/bash
    foo() {
      echo "$@"
    }
    
    foo 1 2 3 # output => 1 2 3
    ```
    >**Note:** You should practically always use double quotes around `"$@"`, like here.

    Omitting the quotes will cause the shell to expand wildcards (even when the user specifically quoted them in order to avoid that) and generally introduce unwelcome behavior and potentially even security problems.
    ```
    foo "string with spaces;" '$HOME' "*"
    # output => string with spaces; $HOME *
    ```
4. for default arguments use `${1:-default_val}`.
   Eg:
   ```
   #!/bin/bash
   foo() {
     local val=${1:-25}
     echo "$val"
   }
  
   foo     # output => 25
   foo 30  # output => 30
   ```

5. to require an argument use `${var:?error message}`
    ```
    foo() {
      local val=${1:?Must provide an argument}
      echo "$val"
    }
    ```

## Simple Function
In `helloWorld.sh`

    #!/bin/bash

    # Define a function greet
    greet ()
    {
        echo "Hello World!"
    }
    
    # Call the function greet
    greet

In running the script, we see our message

    $ bash helloWorld.sh
    Hello World!

*Note that* [sourcing][1] a file with functions makes them available in your current bash session.

    $ source helloWorld.sh   # or, more portably, ". helloWorld.sh"
    $ greet
    Hello World!

  [1]: https://www.wikiod.com/bash/sourcing


You can `export` a function in some shells, so that it is exposed to child processes.

    bash -c 'greet'  # fails
    export -f greet  # export function; note -f
    bash -c 'greet'  # success

## Handling flags and optional parameters
The *getopts* builtin can be used inside functions to write functions that accommodate flags and optional parameters.  This presents no special difficulty but one has to handle appropriately the values touched by *getopts*.  As an example, we define a *failwith* function that writes a message on *stderr* and exits with code 1 or an arbitrary code supplied as parameter to the `-x` option:

    # failwith [-x STATUS] PRINTF-LIKE-ARGV
    #  Fail with the given diagnostic message
    #
    # The -x flag can be used to convey a custom exit status, instead of
    # the value 1.  A newline is automatically added to the output.

    failwith()
    {
        local OPTIND OPTION OPTARG status
    
        status=1
        OPTIND=1
    
        while getopts 'x:' OPTION; do
            case ${OPTION} in
                x)    status="${OPTARG}";;
                *)    1>&2 printf 'failwith: %s: Unsupported option.\n' "${OPTION}";;
            esac
        done
    
        shift $(( OPTIND - 1 ))
        {
            printf 'Failure: '
            printf "$@"
            printf '\n'
        } 1>&2
        exit "${status}"
    }

This function can be used as follows:

    failwith '%s: File not found.' "${filename}"
    failwith -x 70 'General internal error.'

and so on.

Note that as for *printf*, variables should not be used as first argument.  If the message to print consists of the content of a variable, one should use the `%s` specifier to print it, like in

    failwith '%s' "${message}"


## Return value from a function
The `return` statement in Bash doesn't return a value like C-functions, instead it exits the function with a return status. You can think of it as the exit status of that function.

If you want to return a value from the function then send the value to `stdout` like this:

    fun() {
        local var="Sample value to be returned"
        echo "$var"
        #printf "%s\n" "$var"
    }

Now, if you do:

    var="$(fun)"

the output of `fun` will be stored in `$var`.

## The exit code of a function is the exit code of its last command
Consider this example function to check if a host is up:

    is_alive() {
        ping -c1 "$1" &> /dev/null
    }

This function sends a single ping to the host specified by the first function parameter.
The output and error output of `ping` are both redirected to `/dev/null`,
so the function will never output anything.
But the `ping` command will have exit code 0 on success,
and non-zero on failure.
As this is the last (and in this example, the only) command of the function,
the exit code of `ping` will be reused for the exit code of the function itself.

This fact is very useful in conditional statements.

For example, if host `graucho` is up, then connect to it with `ssh`:

    if is_alive graucho; then
        ssh graucho
    fi

Another example: repeatedly check until host `graucho` is up, and then connect to it with `ssh`:

    while ! is_alive graucho; do
        sleep 5
    done
    ssh graucho

## Print the function definition
    getfunc() {
        declare -f "$@"
    }
    
    function func(){
        echo "I am a sample function"
    }
    
    funcd="$(getfunc func)" 
    getfunc func # or echo "$funcd"

Output:

    func () 
    { 
        echo "I am a sample function"
    }



## A function that accepts named parameters
    foo() {
      while [[ "$#" -gt 0 ]]
      do
        case $1 in
          -f|--follow)
            local FOLLOW="following"
            ;;
          -t|--tail)
            local TAIL="tail=$2"
            ;;
        esac
        shift
      done
    
      echo "FOLLOW: $FOLLOW"
      echo "TAIL: $TAIL"
    }

Example usage:

    foo -f
    foo -t 10
    foo -f --tail 10
    foo --follow --tail 10


