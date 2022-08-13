---
title: "Parsing Command Line arguments"
slug: "parsing-command-line-arguments"
draft: false
images: []
weight: 9846
type: docs
toc: true
---

Most command line tools rely on arguments passed to the program upon its execution. Instead of prompting for input, these programs expect data or specific flags (which become booleans) to be set. This allows both the user and other programs to run the Python file passing it data as it starts. This section explains and demonstrates the implementation and usage of command line arguments in Python.

## Hello world in argparse
The following program says hello to the user.  It takes one positional argument, the name of the user, and can also be told the greeting.

    import argparse
    
    parser = argparse.ArgumentParser()
    
    parser.add_argument('name', 
        help='name of user'
    )
    
    parser.add_argument('-g', '--greeting', 
        default='Hello',
        help='optional alternate greeting'
    )
    
    args = parser.parse_args()
    
    print("{greeting}, {name}!".format(
           greeting=args.greeting,
           name=args.name)
    )

<!-- language: lang-none -->

    $ python hello.py --help
    usage: hello.py [-h] [-g GREETING] name
    
    positional arguments:
      name                  name of user
    
    optional arguments:
      -h, --help            show this help message and exit
      -g GREETING, --greeting GREETING
                            optional alternate greeting

<!-- language: lang-none -->

    $ python hello.py world
    Hello, world!
    $ python hello.py John -g Howdy
    Howdy, John!

For more details please read the [argparse documentation][1].

 [1]: https://docs.python.org/3/library/argparse.html

## Using command line arguments with argv
Whenever a Python script is invoked from the command line, the user may supply additional **command line arguments** which will be passed on to the script. These arguments will be available to the programmer from the system variable `sys.argv` ("argv" is a traditional name used in most programming languages, and it means "**arg**ument **v**ector").

By convention, the first element in the `sys.argv` list is the name of the Python script itself, while the rest of the elements are the tokens passed by the user when invoking the script.

    # cli.py
    import sys
    print(sys.argv)

    $ python cli.py
    => ['cli.py']

    $ python cli.py fizz
    => ['cli.py', 'fizz']

    $ python cli.py fizz buzz
    => ['cli.py', 'fizz', 'buzz']

Here's another example of how to use `argv`. We first strip off the initial element of sys.argv because it contains the script's name. Then we combine the rest of the arguments into a single sentence, and finally print that sentence prepending the name of the currently logged-in user (so that it emulates a chat program).

    import getpass
    import sys

    words = sys.argv[1:]
    sentence = " ".join(words)
    print("[%s] %s" % (getpass.getuser(), sentence))

The algorithm commonly used when "manually" parsing a number of non-positional arguments is to iterate over the `sys.argv` list. One way is to go over the list and pop each element of it:

    # reverse and copy sys.argv
    argv = reversed(sys.argv)
    # extract the first element
    arg = argv.pop()
    # stop iterating when there's no more args to pop()
    while len(argv) > 0:
        if arg in ('-f', '--foo'):
            print('seen foo!')
        elif arg in ('-b', '--bar'):
            print('seen bar!')
        elif arg in ('-a', '--with-arg'):
            arg = arg.pop()
            print('seen value: {}'.format(arg))
        # get the next value
        arg = argv.pop()


## Setting mutually exclusive arguments with argparse


## Basic example with docopt
[docopt](http://docopt.org/) turns command-line argument parsing on its head. Instead of parsing the arguments, you just **write the usage string** for your program, and docopt **parses the usage string** and uses it to extract the command line arguments.

    """
    Usage:
        script_name.py [-a] [-b] <path>

    Options:
        -a            Print all the things.
        -b            Get more bees into the path.
    """
    from docopt import docopt


    if __name__ == "__main__":
        args = docopt(__doc__)
        import pprint; pprint.pprint(args)

Sample runs:

```
$ python script_name.py
Usage:
    script_name.py [-a] [-b] <path>
$ python script_name.py something
{'-a': False,
 '-b': False,
 '<path>': 'something'}
$ python script_name.py something -a
{'-a': True,
 '-b': False,
 '<path>': 'something'}
$ python script_name.py -b something -a
{'-a': True,
 '-b': True,
 '<path>': 'something'}
```

## Custom parser error message with argparse


## Conceptual grouping of arguments with argparse.add_argument_group()


## Advanced example with docopt and docopt_dispatch
As with docopt, with [docopt_dispatch] you craft your `--help` in the `__doc__` variable of your entry-point module. There, you call `dispatch` with the doc string as argument, so it can run the parser over it.

That being done, instead of handling manually the arguments (which usually ends up in a high cyclomatic if/else structure), you leave it to dispatch giving only how you want to handle the set of arguments.

This is what the `dispatch.on` decorator is for: you give it the argument or sequence of arguments that should trigger the function, and that function will be executed with the matching values as parameters.

    """Run something in development or production mode.
    
    Usage: run.py --development <host> <port>
           run.py --production <host> <port>
           run.py items add <item>
           run.py items delete <item>
    
    """
    from docopt_dispatch import dispatch
    
    @dispatch.on('--development')
    def development(host, port, **kwargs):
        print('in *development* mode')
    
    @dispatch.on('--production')
    def development(host, port, **kwargs):
        print('in *production* mode')
    
    @dispatch.on('items', 'add')
    def items_add(item, **kwargs):
        print('adding item...')
    
    @dispatch.on('items', 'delete')
    def items_delete(item, **kwargs):
        print('deleting item...')
    
    if __name__ == '__main__':
        dispatch(__doc__)

