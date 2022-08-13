---
title: "Getting started with shell"
slug: "getting-started-with-shell"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
At the command prompt:

    $ echo "Hello World"

Output:

    Hello World

To create a script, create a text document with the following content:

    #!/bin/sh
    echo "Hello World"
    
Save the script with the name `hello.sh` (or any filename) and make the script executable by giving the following permission:

    $ chmod 755 hello.sh

Or:

    $ chmod +x hello.sh

Run the script:
 
    $ ./hello.sh

Output:

    Hello World
To run a local shell script without executable permission:

**1.bash**


    $ bash hello.sh
    Hello World

**2.ksh**


    $ ksh hello.sh
    Hello World

**3.sh**


    $ sh hello.sh
    Hello World

## Installation or Setup
A command shell is a command line interface computer program to an operating system.

**Some Variants**

 **1. *Bash*** : Comes as default shell on ubuntu

**2. KornShell(ksh)** :
    
  

To install ksh in Ubuntu
    

       $ sudo apt-get install ksh
    
    

   To start working with ksh



        $ ksh
        $ ps $$
          PID TTY      STAT   TIME COMMAND
          4187 pts/2    S      0:00 ksh

    
Enter the commands at the ksh prompt

**3. *csh*** :

 

To install csh in Ubuntu

        $ sudo apt-get install csh
        
To work with csh, go to the command line and enter csh

        $ csh
        %





