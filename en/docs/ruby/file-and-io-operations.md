---
title: "File and IO Operations"
slug: "file-and-io-operations"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Parameters
|Flag |  Meaning
|-----|--------------------------------------------------------
|"r"  |  Read-only, starts at beginning of file  (default mode).
|"r+" |  Read-write, starts at beginning of file.
|"w"  |  Write-only, truncates existing file to zero length or creates a new file for writing.
|"w+" |  Read-write, truncates existing file to zero length or creates a new file for reading and writing.
|"a"  |  Write-only, starts at end of file if file exists, otherwise creates a new file for writing.
|"a+" |  Read-write, starts at end of file if file exists, otherwise creates a new file for reading and writing.
|"b"  |  Binary file mode. Suppresses EOL <-> CRLF conversion on Windows. And sets external encoding to ASCII-8BIT unless explicitly specified. (This flag may only appear in conjunction with the above flags. For example, `File.new("test.txt", "rb")` would open `test.txt` in `read-only` mode as a `binary` file.)
|"t"  |  Text file mode. (This flag may only appear in conjunction with the above flags. For example, `File.new("test.txt", "wt")` would open `test.txt` in `write-only` mode as a `text` file.)

## Writing a string to a file


## Open and closing a file
Manually open and close a file.

    # Using new method
    f = File.new("test.txt", "r") # reading
    f = File.new("test.txt", "w") # writing
    f = File.new("test.txt", "a") # appending

    # Using open method
    f = open("test.txt", "r")

    # Remember to close files
    f.close

Automatically close a file using a block.

    f = File.open("test.txt", "r") do |f|
      # do something with file f
      puts f.read # for example, read it
    end

    

## get a single char of input
Unlike `gets.chomp` this will not wait for a newline.

First part of the stdlib must be included

    require 'io/console'

Then a helper method can be written:

    def get_char
      input = STDIN.getch
      control_c_code = "\u0003"
      exit(1) if input == control_c_code
      input
    end

Its' imporant to exit if `control+c` is pressed.


## Reading from STDIN
    # Get two numbers from STDIN, separated by a newline, and output the result
    number1 = gets
    number2 = gets
    puts number1.to_i + number2.to_i
    ## run with: $ ruby a_plus_b.rb
    ## or:       $ echo -e "1\n2" | ruby a_plus_b.rb

## Reading from arguments with ARGV
    number1 = ARGV[0]
    number2 = ARGV[1]
    puts number1.to_i + number2.to_i
    ## run with: $ ruby a_plus_b.rb 1 2

