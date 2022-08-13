---
title: "Getting started with makefile"
slug: "getting-started-with-makefile"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic Makefile
Consider writing a "hello world!" program in c. Lets say our source code is in a file called source.c, now in order to run our program we need to compile it, typically on Linux (using gcc) we would need to type `$> gcc source.c -o output` where output is the name of the executable to be generated. For a basic program this works well but as programs become more complex our compilation command can also become more complex. This is where a *Makefile* comes in, makefiles allow us to write out a fairly complex set of rules for how to compile a program and then simply compile it by typing make on the command line. For instance here is a possible example Makefile for the hello wold example above.

**Basic Makefile**

Lets make a basic *Makefile* and save it to our system in the same directory as our source code named *Makefile*. Note that this file needs to be named Makefile, however the capitol M is optional. That said it is relatively standard to use a capitol M.

    output: source.c
        gcc source.c -o output

Note that there is exactly one tab before the gcc command on the second line (this is important in makefiles). Once this Makefile is written every time the user types make (in the same directory as the Makefile) make will check to see if source.c has been modified (checks the time stamp) if it has been modified more recently than output it will run the compilation rule on the following line. 

**Variables in Makefiles**

Depending on the project you may want to introduce some variables to your make file. Here is an example Makefile with variables present.

    CFLAGS = -g -Wall
    
    output: source.c
        gcc $< $(CFLAGS) -o $@

Now lets explore what happened here. In the first line we declared a variable named CFLAGS that holds several common flags you may wish to pass to the compiler, note that you can store as many flags as you like in this variable. Then we have the same line as before telling make to check source.c to see if it has been changed more recently than output, if so it runs the compilation rule. Our compilation rule is mostly the same as before but it has been shortened by using variables, the `$<` variable is built into make (referred to as an automatic variable see *https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html*) and it always stands for the source so in this case *source.c*. `$(CFLAGS)` is our variable that we defined before, but note that we had to put the variable in parenthesis with a $ in front like this`$(someVariable)`. This is the syntax for telling Make to expand the variable out to what you typed before. Finally we have the $@ symbol, once again this is a variable built into make, and it simply stands for the target of the compilation step, so in this case it stands for *output*.

**Clean**

Make clean is another useful concept to learn about make files. Lets modify the *Makefile* from above

    CFLAGS = -g -Wall
    TARGETS = output
    
    output: source.c
        gcc $< $(CFLAGS) -o $@

    clean:
        rm $(TARGETS)

As you can see we simply added one more rule to our *Makefile*, and one additional variable that contains all of our targets. This is a somewhat common rule to have in makefiles as it allows you to quickly remove all of the binaries you produced simply by typing `$> make clean`. By typing make clean you tell the make program to run the clean rule and then make will run the rm command to delete all of your targets. 

I hope this brief overview of using make helps you speed up your workflow, *Makefiles* can become very complex, but with these ideas you should be able to get started using make and have a better understanding of what is going on in other programmers *Makefiles*. For more information about using make an excellent resource is https://www.gnu.org/software/make/manual/.

## Defining Rules
# Quick start

A rule describes when and how certain files (rule's **targets**) are created. It can also serve to update a target file if any of the files required for its creation (target's **prerequisites**) are newer than the target. 

Rules follow the syntax below: (Note that *commands* following a rule are indented by a **tab**)

```
targets: prerequisites
        <commands>
```

where *targets* and *prerequisites* are file names or special reserved names and *commands* (if present) are executed by a shell to build/rebuild *targets* that are out-of-date.

To execute a rule one can simply run the `make` command in the terminal from the same directory where the *Makefile* resides. Running `make` without specifying the target, will execute the first rule defined in the *Makefile*. By convention, the first rule in the *Makefile* is often called *all* or *default*, commonly listing all valid build targets as prerequisites.

`make` only executes the rule if the target is out-of-date, meaning either it doesn't exist or its modification time is older than any of its prerequisites. If the list of prerequisites is empty, the rule will only be executed when it is first invoked to build the targets. However, when the rule does not create a file and the target is a dummy variable, the rule will always be executed.

<!-- if version [eq GNU make] -->

# Pattern Rules

Pattern rules are used to specify multiple targets and construct prerequisite names from target names. They are more general and more powerful compared to ordinary rules as each target can have its own prerequisites. In pattern rules, a relationship between a target and a prerequisite is build based on prefixes including path names and suffixes, or both. 

Imagine we want to build the targets `foo.o` and `bar.o`, by compiling C scripts, `foo.c` and `bar.c`, respectively. This could be done by using the ordinary rules below:

```
foo.o: foo.c
    cc -c $< -o $@

bar.o: bar.c
    cc -c $< -o $@

```
where *automatic variable* `$<` is the name of the first prerequisite and `$@` the name of the target (A complete list of automatic variables can be found [here](https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html#Automatic-Variables)).

However, as the targets share the same suffix, the above two rules can now be substituted by the following pattern rule:

<a id="pr"></a> 

```
%.o: %.c
    cc -c $< -o $@
```

<!-- end version if -->

# Implicit Rules

*Implicit rules* tell `make` how to use customary methods to build certain types of target files, which are used very often. `make` uses the target file name to determine which implicit rule to invoke.

The pattern rule example we saw in the [previous section](#pr), does not actually need to be declared in a *Makefile* as `make` has an implicit rule for C compilation. Thus, in the following rule, the prerequisites `foo.o` and `bar.o` will be build using the implicit rule for C compilation, before building `foo`.

```
foo : foo.o bar.o
    cc -o foo foo.o bar.o $(CFLAGS) $(LDFLAGS)
```
A catalogue of implicit rules and the variables used by them can be found [here][posix-implicit].

[posix-implicit]: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/make.html#tag_20_76_13_09
[//]: # (ToDO: Double Colon Rules)


## generic rule to gzip a file
if a directory contain 2 files:
    
    $ ls
    makefile
    example.txt

and ```makefile``` contain the following text

    %.gz: %
        gzip $<

then you can obtain ```example.txt.gz``` by typing in the shell

    $ make -f makefile example.txt.gz

the makefile consist of only one rule that instruct *make* how to create a file whose name end with *.gz* if there is a file with the same name but the *.gz* suffix.


## makefile Hello World
C:\makefile:
    
    helloWorld :
    [TAB]echo hello world

run results:

    C:\>make
    echo hello world
    hello world

**Note:** [TAB] should be replaced by an actual tab, stackoverflow replaces tabs with spaces, and spaces are not used the same as tabs in a makefile.

