---
title: "Basic console commands"
slug: "basic-console-commands"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## which
To determine where on your system an executable in your path exists, use the [`which` command][1]:

    $ which python
    $

If there is no response, that executable does not exist in your path. The system will simply return you a new prompt without an error message. If the executable does exist on your path, it will show the directory where it actually exists:

    $ which ls
    /bin/ls

This can be helpful in determining why behavior does not match expectation by ensuring you're executing the version of the executable that you think you are. For instance, if you have both Python 2 and Python 3 installed, they both might be executed by typing `python` in the terminal - but the executable actually being run may be different than expected. As shown above this command will work for any standard unix command, which are all backed by individual executables.


  [1]: https://linux.die.net/man/1/which

## pwd - print working directory
    $> pwd
    /home/myUserHome
    $> cd ..
    $> pwd
    /home

will print the current path to the console.




## file manipulation commands
List of commands that will be introduced here:

    ls     #view contents of a directory
    touch  #create new file
    mkdir  #create new directory
    cp     #copy contents of one file to another
    mv     #move file from one location to another
    rm     #delete a file or directory

ls examples

    jennifer@my_computer:~/Desktop$ ls
    c++ projects    Research Paper.docx     test.cpp

shows the current directory

    jennifer@my_computer:~/Desktop$ ls c++\ projects
    DNA_analysis.cpp        encryption.cpp  pool_game.cpp

shows the directory "c++ projects". Space characters in file names are typed as "\ ".

touch example

    jennifer@my_computer:~/Desktop$ ls
    c++ projects    Research Paper.docx     test.cpp
    jennifer@my_computer:~/Desktop$ touch ruby_test.rb
    jennifer@my_computer:~/Desktop$ ls
    c++ projects    Research Paper.docx     ruby_test.rb    test.cpp

mkdir example

    jennifer@my_computer:~/Desktop$ mkdir ruby
    jennifer@my_computer:~/Desktop$ ls
    c++ projects    Research Paper.docx     ruby    ruby_test.rb    test.cpp
    jennifer@my_computer:~/Desktop$ cd ruby
    jennifer@my_computer:~/Desktop/ruby$ ls
    <nothing>
    jennifer@my_computer:~/Desktop/ruby

It doesn't actually print `<nothing>`. It's just how I'm representing that it doesn't output anything

cp examples

    jennifer@my_computer:~/Desktop/ruby$ cd ..
    jennifer@my_computer:~/Desktop$ cp test.cpp c++_test.cpp
    jennifer@my_computer:~/Desktop$ ls
    c++ projects    c++_test.cpp    Research Paper.docx     ruby    ruby_test.rb
    test.cpp

This is when the last arg to `cp`, in this case "c++_test.cpp" is not an existing directory. `cp` will create a file called "c++_test.cpp", with contents identical to that of "test.cpp". If c++_test.cpp already existed, `cp` would have deleted what was previously there before copying the contents of "test.cpp" over.

    jennifer@my_comptuer:~/Desktop$ ls ruby
    <nothing>
    jennifer@my_computer:~/Desktop$ cp ruby_test.rb ruby
    jennifer@my_computer:~/Desktop$ ls ruby
    ruby_test.rb

This is what happens when the last arg to `cp`, in this case "ruby", is a directory. `cp` creates a file with the same name as "ruby_test.rb", but in the directory "ruby".

mv examples

    jennifer@my_computer:~/Desktop$ ls
    c++ projects    c++_test.cpp    Research Paper.docx     ruby    ruby_test.rb
    test.cpp
    jennifer@my_computer:~/Desktop$ mv ruby_test.rb ruby\ test.rb
    jennifer@my_computer:~/Desktop$ ls
    c++ projects    c++_test.cpp    Research Paper.docx     ruby    ruby test.rb
    test.cpp

This is what happens when the last arg to `mv`, in this case "ruby test.rb", is not an existing directory. The file "ruby_test.rb" has been renamed to "ruby test.rb". If "ruby test.rb" already existed, it would have been overwritten Note, again, that spaces are preceded by a '\'.

    jennifer@my_computer:~/Desktop$ ls
    c++ projects    c++_test.cpp    Research Paper.docx     ruby    ruby test.rb
    test.cpp
    jennifer@my_computer:~/Desktop$ ls c++\ projects
    DNA_analysis.cpp        encryption.cpp  pool_game.cpp
    jennifer@my_computer:~/Desktop$ mv test.cpp c++\ projects
    jennifer@my_computer:~/Desktop$ ls
    c++ projects    c++_test.cpp    Research Paper.docx     ruby    ruby test.rb
    jennifer@my_computer:~/Desktop$ ls c++\ projects
    DNA_analysis.cpp        encryption.cpp  pool_game.cpp   test.cpp

This is what happens when `mv` is a directory that already existed. The file "test.cpp" gets moved to the directory "c++ projects".

rm examples

    jennifer@my_computer:~/Desktop$ ls
    c++ projects    c++_test.cpp    Research Paper.docx     ruby    ruby test.rb
    jennifer@my_computer:~/Desktop$ rm c++_test.cpp
    jennifer@my_computer:~/Desktop$ ls
    c++ projects    Research Paper.docx     ruby    ruby test.rb

c++_test.cpp has been deleted

    jennifer@my_computer:~/Desktop$ rm c++\ projects
    rm: cannot remove 'c++ projects': Is a directory
    jennifer@my_computer:~/Desktop$ ls
    c++ projects    Research Paper.docx     ruby    ruby test.rb

`rm` has an extra requirement to delete directories

    jennifer@my_computer:~/Desktop$ rm -rf c++\ projects
    jennifer@my_computer:~/Desktop$ ls
    Research Paper.docx     ruby    ruby test.rb

`-rf` must be added to delete a directory. 

To learn more about `ls`, type the command `ls --help`. For `touch`, type `touch --help`. Likewise with all 6 commands mentioned here. This prints out a detailed explanation of use without creating or deleting anything.

## cd command, directories explained
    michael@who-cares:~$

The symbol `~` after the `who-cares:` is the current directory. `~` actually means the person's home directory. In this case, that's `/home/michael`.

    michael@who-cares:~$ cd Downloads
    michael@who-cares:~/Downloads$

Looks for `Downloads` in the current directory, then makes that the current directory.

    michael@who-cares:~/Downlaods$ cd /var
    michael@who-cares:/var$

Since this directory started with a `/`, that means look in the root directory for the directory `var`. For those coming from windows, the root directory is the equivalent to `C:\`. Directories starting with `/` are called **"absolute directories"** and directories that don't are called **"relative directories"**

    michael@who-cares:/var cd lib/dbus
    michael@who-cares:/var/lib/dbus$

The `/` in the middle means do `cd lib` and once that's done `cd dbus` in one command.

    michael@who-cares:/var/lib/dbus$ cd .
    michael@who-cares:/var/lib/dbus$

`.` actually means "the current directory". The command `cd .` is basically useless, but `.` is useful for other things.

    michael@who-cares:/var/lib/dbus$ cd ..
    michael@who-cares:/var/lib$

`..` actually means "the parent of the current directory". As such, `cd ..` means "navigate one directory up".

    michael@who-cares:/var/lib$ cd ../log/apt
    michael@who-cares:/var/log/apt$

`.` and `..` can also be part of the `/` chain. Also, there's no limit to how long it can be.

    michael@who-cares:/var/log/apt$ cd /dev/bus
    michael@who-cares:/dev/bus$

The `/` chain can even exist when the directory starts at root.

    michael@who-cares:/dev/bus$ cd /
    michael@who-cares:/$

`cd /` takes you to the root directory. I wonder what happens if you type `cd ..` here... (don't worry. It's safe)

    michael@who-cares:/$ cd home
    michael@who-cares:/home$ cd michael
    michael@who-cares:~$

Every user has a directory for their stuff inside the home directory. If current directory is under the home directory, that part of the name, in this case `/home/michael`, it's replaced with `~`.

    michael@who-cares:~$ cd sys
    michael@who-cares:/sys$ cd ~/Desktop
    michael@who-cares:~/Desktop$ cd ~/..
    michael@who-cares:/home$

`~` can also be part of the `/` chain. It can even be in the same chain as `..`. If the directory starts with `~`, it's an absolute directory just like if it starts with `/`.

Last thing to try: type `cd` with no directory after.

## Basic Unix commands
    $pwd
Displays the present working directory.

    $who
Displays all the users logged in.

    $who am i
Shows the username of the current user.

    $date
Displays the current system date

    $which <command>
Shows the path of the specified command.
For example "$which pwd" will shows the path of 'pwd' command.

    $file <file_name>
Shows the type of the specified file(regular file, directory or other files)

    $cal
Displays the calender of the current month.

    $bc
Shows the mathematical calculation between two integers of floats. For example "$bc 2+3" will returns the arithmetic sum of 3 and 5.

    $ls
Lists the contents of the directory.

 - $ls -l : lists in long format.
 - $ls -c : Multi column output.
 - $ls -f : Lists the type of file.
 - $ls -r : Recursive listing of all subdirectories encountered.
 - $ls -a : Displays all files including hidden files.
 - $ls -i : Lists all files along with its I-Node number.


    $grep [options] <pattern> <input_file_names> 
Prints lines that contain a match for a pattern.
 - $grep -i : Perform case insensitive matching
 - $grep -v : Prints all lines that don’t contain the regex
 - $grep -r <pattern> <folderName> : Recursively search subdirectories listed and prints file names with occurrence of the pattern
 - $grep -I : Exclude binary files


## pushd/popd (store current dir on stack and go to dest / pop prev dir and go to it)
    $ pwd
    /home/bob/somedir1/somedir2/somedir3

    $ pushd /home/bob/otherdir1/otherdir2
    /home/bob/otherdir1/otherdir2 /home/bob/somedir1/somedir2/somedir3

    $ popd
    /home/bob/somedir1/somedir2/somedir3

    $ pushd /usr
    /usr /home/bob/somedir1/somedir2/somedir3

    $ pushd /var
    /var /usr /home/bob/somedir1/somedir2/somedir3

    $ popd
    /usr /home/bob/somedir1/somedir2/somedir3

    $ pwd
    /usr

    $ popd
    /home/bob/somedir1/somedir2/somedir3

    $ pwd
    /home/bob/somedir1/somedir2/somedir3

