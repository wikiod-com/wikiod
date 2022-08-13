---
title: "Advanced Makefile"
slug: "advanced-makefile"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Building from different source folders to different target folders
Main features of this Makefile : 

 - Automatic detection of C sources in specified folders
 - Multiple source folders
 - Multiple corresponding target folders for object and dependency files
 - Automatic rule generation for each target folder
 - Creation of target folders when they don't exist
 - Dependency management with `gcc` : Build only what is necessary
 - Works on `Unix` and `DOS` systems
 - Written for GNU Make

This Makefile can be used to build a project with this kind of structure : 

    \---Project
        +---Sources
        |   +---Folder0
        |   |       main.c
        |   |       
        |   +---Folder1
        |   |       file1_1.c
        |   |       file1_1.h
        |   |       
        |   \---Folder2
        |           file2_1.c
        |           file2_1.h
        |           file2_2.c
        |           file2_2.h
        \---Build
            |   Makefile
            |   myApp.exe
            |   
            +---Folder0
            |       main.d
            |       main.o
            |       
            +---Folder1
            |       file1_1.d
            |       file1_1.o
            |       
            \---Folder2
                    file2_1.d
                    file2_1.o
                    file2_2.d
                    file2_2.o


**Makefile**

    # Set project directory one level above of Makefile directory. $(CURDIR) is a GNU make variable containing the path to the current working directory
    PROJDIR := $(realpath $(CURDIR)/..)
    SOURCEDIR := $(PROJDIR)/Sources
    BUILDDIR := $(PROJDIR)/Build
    
    # Name of the final executable
    TARGET = myApp.exe
    
    # Decide whether the commands will be shwon or not
    VERBOSE = TRUE
    
    # Create the list of directories
    DIRS = Folder0 Folder1 Folder2
    SOURCEDIRS = $(foreach dir, $(DIRS), $(addprefix $(SOURCEDIR)/, $(dir)))
    TARGETDIRS = $(foreach dir, $(DIRS), $(addprefix $(BUILDDIR)/, $(dir)))
    
    # Generate the GCC includes parameters by adding -I before each source folder
    INCLUDES = $(foreach dir, $(SOURCEDIRS), $(addprefix -I, $(dir)))
    
    # Add this list to VPATH, the place make will look for the source files
    VPATH = $(SOURCEDIRS)
    
    # Create a list of *.c sources in DIRS
    SOURCES = $(foreach dir,$(SOURCEDIRS),$(wildcard $(dir)/*.c))
    
    # Define objects for all sources
    OBJS := $(subst $(SOURCEDIR),$(BUILDDIR),$(SOURCES:.c=.o))
    
    # Define dependencies files for all objects
    DEPS = $(OBJS:.o=.d)
    
    # Name the compiler
    CC = gcc
    
    # OS specific part
    ifeq ($(OS),Windows_NT)
        RM = del /F /Q 
        RMDIR = -RMDIR /S /Q
        MKDIR = -mkdir
        ERRIGNORE = 2>NUL || true
        SEP=\\
    else
        RM = rm -rf 
        RMDIR = rm -rf 
        MKDIR = mkdir -p
        ERRIGNORE = 2>/dev/null
        SEP=/
    endif
    
    # Remove space after separator
    PSEP = $(strip $(SEP))
    
    # Hide or not the calls depending of VERBOSE
    ifeq ($(VERBOSE),TRUE)
        HIDE =  
    else
        HIDE = @
    endif
    
    # Define the function that will generate each rule
    define generateRules
    $(1)/%.o: %.c
        @echo Building $$@
        $(HIDE)$(CC) -c $$(INCLUDES) -o $$(subst /,$$(PSEP),$$@) $$(subst /,$$(PSEP),$$<) -MMD
    endef
    
    .PHONY: all clean directories 
    
    all: directories $(TARGET)
    
    $(TARGET): $(OBJS)
        $(HIDE)echo Linking $@
        $(HIDE)$(CC) $(OBJS) -o $(TARGET)
    
    # Include dependencies
    -include $(DEPS)
    
    # Generate rules
    $(foreach targetdir, $(TARGETDIRS), $(eval $(call generateRules, $(targetdir))))
    
    directories: 
        $(HIDE)$(MKDIR) $(subst /,$(PSEP),$(TARGETDIRS)) $(ERRIGNORE)
    
    # Remove all objects, dependencies and executable files generated during the build
    clean:
        $(HIDE)$(RMDIR) $(subst /,$(PSEP),$(TARGETDIRS)) $(ERRIGNORE)
        $(HIDE)$(RM) $(TARGET) $(ERRIGNORE)
        @echo Cleaning done ! 
   

**How to use this Makefile**
To adapt this Makefile to your project you have to : 

 1. Change the `TARGET` variable to match your target name
 2. Change the name of the `Sources` and `Build` folders in `SOURCEDIR` and `BUILDDIR`
 3. Change the verbosity level of the Makefile in the Makefile itself or in make call
 4. Change the name of the folders in `DIRS` to match your sources and build folders
 5. If required, change the compiler and the flags

## Zipping lists
<!-- if version [eq GNU make] -->

This `pairmap` function takes three arguments:

 1. A function name
 2. First space-separated list
 3. Second space-separated list

For each zipped tuple in the lists it will call the function with the following arguments:

 1. Tuple element from the first list
 2. Tuple element from the second list

It will expand to a space-separated list of the function expansions.

```
list-rem = $(wordlist 2,$(words $1),$1)
pairmap = $(and $(strip $2),$(strip $3),$(call \
    $1,$(firstword $2),$(firstword $3)) $(call \
    pairmap,$1,$(call list-rem,$2),$(call list-rem,$3)))
```

For example, this:

```
LIST1 := foo bar baz
LIST2 := 1 2 3

func = $1-$2

all:
    @echo $(call pairmap,func,$(LIST1),$(LIST2))

.PHONY: all
```

Will print `foo-1 bar-2 baz-3`.

<!-- end version if -->

