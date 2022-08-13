---
title: "File IO"
slug: "file-io"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Reading from a file
Let's assume you have a file **lyrics.txt** which contains the following data:

    summer has come and passed
    the innocent can never last
    wake me up when september ends



# Read the entire file at a time

By using `file:read_file(File)`, you can read the entire file. It's an atomic operation:

    1> file:read_file("lyrics.txt").
    {ok,<<"summer has come and passed\r\nthe innocent can never last\r\nWake me up w
    hen september ends\r\n">>}

# Read one line at a time
 
`io:get_line` reads the text until the newline or the end of file.

    1> {ok, S} = file:open("lyrics.txt", read).
    {ok,<0.57.0>}
    2> io:get_line(S, '').
    "summer has come and passed\n"
    3> io:get_line(S, '').
    "the innocent can never last\n"
    4> io:get_line(S, '').
    "wake me up when september ends\n"
    5> io:get_line(S, '').
    eof
    6> file:close(S).
    ok
    
# Read with the random access
 
`file:pread(IoDevice, Start, Len)` reads from `Start`  as much as `Len` from `IoDevice`.

    1> {ok, S} = file:open("lyrics.txt", read).
    {ok,<0.57.0>}
    2> file:pread(S, 0, 6).
    {ok,"summer"}
    3> file:pread(S, 7, 3).
    {ok,"has"}       






## Writing to a file
# Write one line at a time

Open a file with `write` mode and use `io:format/2`:

    1> {ok, S} = file:open("fruit_count.txt", [write]).
    {ok,<0.57.0>}
    2> io:format(S, "~s~n", ["Mango 5"]).
    ok
    3> io:format(S, "~s~n", ["Olive 12"]).
    ok
    4> io:format(S, "~s~n", ["Watermelon 3"]).
    ok
    5>

The result will be a file named **fruit_count.txt** with the following contents:

    Mango 5
    Olive 12
    Watermelon 3

Note that opening a file in write mode will created it, if not already existent in the file system.

Note also that using the `write` option with `file:open/2` will **truncate** the file (even if you don't write anything into it). To prevent this, open the file in `[read,write]` or `[append]` mode.

# Write the entire file at once

`file:write_file(Filename, IO)` is the simplest function for writing a file at once. If the file already exists, it will overwritten, otherwise it will be created.

    1> file:write_file("fruit_count.txt", ["Mango 5\nOlive 12\nWatermelon 3\n"
    ]).
    ok
    2> file:read_file("fruit_count.txt").
    {ok,<<"Mango 5\nOlive 12\nWatermelon 3\n">>}
    3>

# Write with random access

For random access writing, `file:pwrite(IoDevice, Location, Bytes)` is used. If you want to replace some string in the file, this method is useful.

Let's assume you want to change "Olive 12" to "Apple 15" in the file created above.

    1> {ok, S} = file:open("fruit_count.txt", [read, write]).
    {ok,{file_descriptor,prim_file,{#Port<0.412>,676}}}
    2> file:pwrite(S, 8, ["Apple 15\n"]).
    ok
    3> file:read_file("fruit_count.txt").
    {ok,<<"Mango 5\nApple 15\nWatermelon 3">>}
    4> file:close(S).
    ok
    5>

