---
title: "sys"
slug: "sys"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The **sys** module provides access to functions and values concerning the program's runtime environment, such as the command line parameters in `sys.argv` or the function `sys.exit()` to end the current process from any point in the program flow.

While cleanly separated into a module, it's actually built-in and as such will always be available under normal circumstances.

## Syntax
 - Import the sys module and make it available in the current namespace:
   
       import sys

 - Import a specific function from the sys module directly into the current namespace:
   
       from sys import exit


For details on all **sys** module members, refer to the [official documentation][1].


  [1]: https://docs.python.org/library/sys.html


## Command line arguments
    if len(sys.argv) != 4:         # The script name needs to be accounted for as well.
        raise RuntimeError("expected 3 command line arguments")
    
    f = open(sys.argv[1], 'rb')    # Use first command line argument.
    start_line = int(sys.argv[2])  # All arguments come as strings, so need to be
    end_line = int(sys.argv[3])    # converted explicitly if other types are required.

Note that in larger and more polished programs you would use modules such as [click][1] to handle command line arguments instead of doing it yourself.


  [1]: http://click.pocoo.org/

## Script name
    # The name of the executed script is at the beginning of the argv list.
    print('usage:', sys.argv[0], '<filename> <start> <end>')
    
    # You can use it to generate the path prefix of the executed program
    # (as opposed to the current module) to access files relative to that,
    # which would be good for assets of a game, for instance.
    program_file = sys.argv[0]
    
    import pathlib
    program_path = pathlib.Path(program_file).resolve().parent
    

## Standard error stream
    # Error messages should not go to standard output, if possible.
    print('ERROR: We have no cheese at all.', file=sys.stderr)

    try:
        f = open('nonexistent-file.xyz', 'rb')
    except OSError as e:
        print(e, file=sys.stderr)


## Ending the process prematurely and returning an exit code
    def main():
        if len(sys.argv) != 4 or '--help' in sys.argv[1:]:
            print('usage: my_program <arg1> <arg2> <arg3>', file=sys.stderr)
            
            sys.exit(1)    # use an exit code to signal the program was unsuccessful

        process_data()


