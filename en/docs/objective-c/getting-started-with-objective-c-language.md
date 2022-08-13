---
title: "Getting started with Objective-C Language"
slug: "getting-started-with-objective-c-language"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
This program will output "Hello World!"

    #import <Foundation/Foundation.h>

    int main(int argc, char * argv[]) {
        NSLog(@"Hello World!");
    }

`#import` is a pre-processor directive, which indicates we want to *import* or include the information from that file into the program. In this case, the compiler will copy the contents of `Foundation.h` in the `Foundation` framework to the top of the file.  The main difference between #import and #include is that #import is "smart" enough to not reprocess files that have already been included in other #includes.

The [C Language documentation](https://www.wikiod.com/c/getting-started-with-c-language#Hello World) explains the `main` function.

The `NSLog()` function will print the string provided to the console, along with some debugging information. In this case, we use an Objective-C string literal: `@"Hello World!"`. In C, you would write this as `"Hello World!"`, however, Apple's Foundation Framework adds the `NSString` class which provides a lot of useful functionality, and is used by NSLog. The simplest way to create an instance of `NSString` is like this: <code>@"***string content here***"</code>. 

>Technically, NSLog() is part of Apple's Foundation Framework and is not actually part of the Objective-C language.  However, the Foundation Framework is ubiquitous throughout Objective-C programming.  Since the Foundation Framework is not open-source and cannot be used outside of Apple development, there are open-source alternatives to the framework which are associated with [OPENStep][1] and [GNUStep][2].

---

### Compiling the program

Assuming we want to compile our Hello World program, which consist of a single `hello.m` file, the command to compile the executable is:
<!-- language: none -->

    clang -framework Foundation hello.m -o hello

Then you can run it:
<!-- language: none -->

    ./hello

This will output:

<!-- language: none -->

    Hello World!

The options are:

  - `-framework`: Specifies a framework to use to compile the program. Since this program uses Foundation, we include the Foundation framework.

  - `-o`: This option indicate to which file we'd like to output our program. In our case `hello`. If not specified, the default value is `a.out`.


  [1]: http://toastytech.com/guis/openstep.html
  [2]: http://www.nongnu.org/gap/index.html

