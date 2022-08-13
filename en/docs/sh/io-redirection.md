---
title: "IO Redirection"
slug: "io-redirection"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Generally a command takes inputs from terminal and outputs back to terminal. Normally a command reads input from keyboard and outputs result to the screen. Here is the importance of Input/Output Redirection

## Syntax
- [fd]<file
- [fd]<&fd
- [fd]<&-
- [fd]>file
- [fd]>&fd
- [fd]>&-
- [fd]>|file
- [fd]>>file
- [fd]<>file
- [fd]<<[-] word  
  ...  
  word

Resources
---

- [The POSIX 'Shell Command Language' section on 'Redirection'][posix]

[posix]: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_07    

## Output Redirection
Usually output of a command goes to the terminal. Using the concept of Output redirection, the output of a command can be redirected to a file. So insted of displaying the output to the terminal it can be send to a file. 
'>' character is used for output redirection. 

    $ pwd > file1
    $ cat file1
    /home/cg/root
In the above example, the command the output 'pwd' of the command is redirected to a file called 'file1'. 



  




## Input Redirection
The commands normally take their input from the standard input device keyboard. Using Input redirection concept, we can have their input redirected from a file.
To redirect standard input from a file instead of the keyboard, the '<' character is used.

    $ cat file1 
    monday
    tuesday
    wednsday
    thursday
    friday
    saturday
    sunday
The above is the content of file1

    $ sort < file1
    friday
    monday
    saturday
    sunday
    thursday
    tuesday
    wednsday
here insted of taking input from keyboard, we redirected it from the file1 and sort it in ascending order.

 

