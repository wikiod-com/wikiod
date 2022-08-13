---
title: "Files and IO streams"
slug: "files-and-io-streams"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

The Ada standard library provides for I/O of traditional files of text or binary data, as well as I/O of streamed files. Files of binary data will be sequences of values of a type, while stream files can be sequences of values of possibly different types.

To read and write elements of different types from/to stream files, Ada uses subprograms denoted by types' attributes, namely `'Read`, `'Write`, `'Input`, and `'Output`. The latter two will read and write array bounds, record discriminants, and type tags, in addition to the bare input and output that `Read` and `'Write` will perform.

## Create And Write To A Stream
The subtypes' stream-oriented attributes are called to write objects to a file, bare and using binary default representations.

    with Ada.Streams.Stream_IO;
    
    procedure Main is
       type Fruit is (Banana, Orange, Pear);
       type Color_Value is range 0 .. 255;
       type Color is record
          R, G, B : Color_Value;
       end record;
    
       Fruit_Colors : constant array (Fruit) of Color :=
         (Banana => Color'(R => 243, G => 227, B => 18),
          Orange => Color'(R => 251, G => 130, B => 51),
          Pear   => Color'(R => 158, G => 181, B => 94));
    
       use Ada.Streams.Stream_IO;
    
       F : File_Type;
    
    begin
       Create (F, Name => "file.bin");
       for C in Fruit_Colors'Range loop
          Fruit'Write (Stream (F), C);
          Color'Write (Stream (F), Fruit_Colors (C));
       end loop;
       Close (F);
    end Main;

# Resulting File
    00000000  00 2e f3 00 e3 00 12 00  01 2e fb 00 82 00 33 00
    00000010  02 2e 9e 00 b5 00 5e 00                         


## Create and write to file
The procedures `Create`, [`Put_Line`][2], `Close` from the package [`Ada.Text_IO`][1] is used to create and write to the file `file.txt`.

    with Ada.Text_IO;
    
    procedure Main is
       use Ada.Text_IO;
       F : File_Type;
    begin
       Create (F, Out_File, "file.txt");
       Put_Line (F, "This string will be written to the file file.txt");
       Close (F);
    end;

# Resulting file `file.txt`
    This string will be written to the file.txt


  [1]: https://www.wikiod.com/ada/package-adatext_io
  [2]: https://www.wikiod.com/ada/package-adatext_io#Put_Line

## Open And Read From Stream File
Read the data of [Create And Write To A Stream][.streamout] back into a program.

  [.streamout]: https://www.wikiod.com/ada/files-and-io-streams#Create And Write To A Stream

    with Ada.Streams.Stream_IO;
    
    procedure Main is
       --
       --  ... same type definitions as in referenced example
       --
       Fruit_Colors : array (Fruit) of Color;
    
       use Ada.Streams.Stream_IO;
    
       F : File_Type;
       X : Fruit;
    begin
       Open (F, Mode => In_File, Name => "file.bin");
       loop
          Fruit'Read (Stream (F), X);
          Color'Read (Stream (F), Fruit_Colors (X));
       end loop;
    exception
       when End_Error =>
          Close (F);
       pragma Assert  -- check data are the same
         (Fruit_Colors (Banana) = Color'(R => 243, G => 227, B => 18) and
          Fruit_Colors (Orange) = Color'(R => 251, G => 130, B => 51) and
          Fruit_Colors (Pear)   = Color'(R => 158, G => 181, B => 94));
    end Main;

