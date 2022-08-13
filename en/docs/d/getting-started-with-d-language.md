---
title: "Getting started with D Language"
slug: "getting-started-with-d-language"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
The D programming language's standard compiler DMD can run on all major platforms. To install DMD see [here](http://dlang.org/download.html). To install by command line you may run the command (found on the D website):

    curl -fsS https://dlang.org/install.sh | bash -s dmd

# Package Managers #

## Arch Linux ##

    pacman -S dlang

## Chocolatey ##

    choco install dmd

## Gentoo ##

    layman -f -a dlang

## OSX Homebrew ##

    brew install dmd

## Debian/Ubuntu ##

Installation on Debian/Ubuntu distributions needs that the [APT repository][1] be added to the sources list.

    wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
    wget -qO - https://dlang.org/d-keyring.gpg | sudo apt-key add -
    apt-get update
    apt-get install dmd-bin


  [1]: http://d-apt.sourceforge.net/

# Other compilers

[LDC][2] is a D compiler thats uses the oficial DMD compiler frontend and LLVM as its backend.

[GDC][3] is a D compiler that uses the GCC backend to generate code.

  [2]: https://wiki.dlang.org/LDC
  [3]: http://gdcproject.org/

# IDEs

In order to make life easier you may also want to install an IDE (Integrated Development Environment).
The D-Language Wiki has a list of available IDEs and Plugins for all Platforms [here](https://wiki.dlang.org/IDEs).

## Hello World
    import std.stdio;

    // Let's get going!
    void main()
    {
        writeln("Hello World!");
    }

  To compile and run, save this text as a file called `main.d`. From the command line run `dmd main.d` to compile the program. Finally, run `./main` to execute the program in a bash shell or you can click on the executable on windows.

## Read values from a string
```
import std.format;

void main() {
    string s = "Name Surname 18";
    string name, surname;
    int age;
    formattedRead(s, "%s %s %s", &name, &surname, &age);
    // %s selects a format based on the corresponding argument's type
}
```
Official documentation for the format strings can be found at:
https://dlang.org/phobos/std_format.html#std.format

## Hello, World!
To create the classic "Hello, world" printing program, create a file called `hello.d` with a text editor containing the following code : 

    import std.stdio;
    
    void main() {
        writeln("Hello, World!");    //writeln() automatically adds a newline (\n) to the output
    }

# Explanation :
    import std.stdio
This line tells the compiler that functions defined in the Standard Library module `std.stdio` will be used.
Any module may be imported, as long as the compiler knows where to look for them.
Many functions are provided as part of D's massive Standard Library.

    void main() {
This line declares the function `main`, returning `void`. Note that unlike C and C++, D allows main to be of type `void`. The function `main` is special as it is the entry point of the program, i.e., this is where the execution of the program begins.
A few notes about functions in general : 
 - A function's name can be anything that starts with a letter and is composed of letters, digits and underscores.
 - Expected parameters will be a comma-separated list of variable names and their data types.

 - The value that the function is expected to return can be any existing data type, and it must match the type of expression used in the return statement within the function.

The curly braces `{ … }` are used in pairs to indicate where a block of code begins and ends. They can be used in a lot of ways, but in this case they indicate where the function begins and ends.

    writeln("Hello, World!");
`writeln` is a function declared in `std.stdio` that writes its agruments to `stdout`. 
In this case, its argument is `"Hello, World"`, which will be written to the console. Various format characters, similar to the ones used by C's `printf` may be used, like `\n`, `\r`, etc.

Every statement needs to be terminated by a semi-colon.

Comments are used to indicate something to the person reading the code and are treated like a blank by the compiler. In the code above, this is a comment:

    //writeln() automatically adds a newline (\n) to the output
These are pieces of code that are ignored by the compiler. There are three different ways to comment in D :
1. `//` - Comment all text in the same line, after the `//`
2. `/* comment text */` - These are useful for multiline comments
3. `/+ comment text +` - These are also multiline comments

They are very useful to convey what a function / piece of code is doing to a fellow developer.

# Compiling and Running the Program
To run this program, the code must fist be compiled into an executable. This can be done with the help of the compiler.

To compile using DMD, the reference D compiler, open a terminal, navigate to the the location of the file `hello.d` that you created and then run :

`dmd hello.d`

If no errors are found, the compiler will output an executable named after your source file. This can now be run by typing 

`./hello`

Upon execution, the program will print out `Hello, World!`, followed by a newline.

