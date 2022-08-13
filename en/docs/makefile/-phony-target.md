---
title: ".PHONY target"
slug: "phony-target"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Using .PHONY for non-files targets
Use `.PHONY` to specify the targets that are not files, e.g., `clean` or `mrproper`.

**Good example**

    .PHONY: clean
    clean:
        rm *.o temp

**Bad example**

    clean:
        rm *.o temp

In the good example `make` knows that `clean` is not a file, therefore it will not search if it is or not up to date and will execute the recipe. 

In the bad example `make` will look for a file named `clean`. If it doesn't exist or is not up to date it will execute the recipe, but if it does exist and is up to date the recipe will not be executed. 

## Using .PHONY for recursive invocations of 'make' command
Recursive use of make means using make as a command within a makefile. This technique is useful when a large project contains sub-directories, each having their respective makefiles. The following example will help understand advantage of using .PHONY with recursive make.


    /main
         |_ Makefile
         |_ /foo
                |_ Makefile
                |_ ... // other files
         |_ /bar
                |_ Makefile
                |_ ... // other files
         |_ /koo
                |_ Makefile
                |_ ... // other files

To run sub-directory's makefile from within the makefile of main, the main's makefile would have looping as shown below (there are other ways in which this can be achieved, but that is out of scope of the current topic)

    SUBDIRS = foo bar koo
    
    subdirs:
            for dir in $(SUBDIRS); do \
              $(MAKE) -C $$dir; \
            done

However, there are pitfalls with this method.

 1. Any error detected in a sub-make is ignored by this rule, so it will
    continue to build the rest of the directories even when one fails.
 2. Make's ability to perform Parallel execution of multiple build
    targets is not utilized since only one rule is used.

By declaring the sub-directories as .PHONY targets (you must do this as the sub-directory obviously always exists; otherwise it won’t be built) these problems can be overcome.

    SUBDIRS = foo bar koo
    
    .PHONY: subdirs $(SUBDIRS)
    
    subdirs: $(SUBDIRS)
    
    $(SUBDIRS):
            $(MAKE) -C $@

 



