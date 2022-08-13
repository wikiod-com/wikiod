---
title: "Getting started with awk"
slug: "getting-started-with-awk"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## AWK by examples
AWK is string manipulation language, used largely in UNIX systems. The idea behind AWK was to create a versatile language to use when working on files, which wasn't too complex to understand. 

AWK has some other variants, but the main concept is the same, just with additional features. These other variants are NAWK and GAWK. GAWK contains all of the features of both, whilst NAWK is one step above AWK, if you like.

The most simple way to think of AWK, is to consider that it has 2 main parts. The pattern, and the action.

Probably the most basic example of AWK: (See also: Hello World)

    BEGIN {print "START"}
          {print        }
    END   {print "STOP" }

Here, the keywords `BEGIN` and `END` are the pattern, whilst the action is inside the {}. This example would be useless, but it would only take minor changes to actually make this into a useful function.

    BEGIN {print "File\tAuthor"}
          {print $8, "\t", $3}
    END {print " - DONE - "}

Here, `\t` represents a Tab character, and is used to even up the output line boundaries. $8 and $3 are similar to the use that is seen in `Shell Scripts`, but instead of using the 3rd and 8th arguments, it uses the 3rd and 8th column of the input line.

So, this example would print:  File    Author on the top line, whilst the second line is to do with the file paths. $8 is the name of the file, $3 is the owner (When looking at the directory path, this will be more clear). Finally, the bottom line would print, as you would expect  - DONE -

Credit for the above example goes to http://www.grymoire.com/Unix/Awk.html


Reference file
----------------

`coins.txt` from  Greg Goebel:

    gold     1    1986  USA                 American Eagle
    gold     1    1908  Austria-Hungary     Franz Josef 100 Korona
    silver  10    1981  USA                 ingot
    gold     1    1984  Switzerland         ingot
    gold     1    1979  RSA                 Krugerrand
    gold     0.5  1981  RSA                 Krugerrand
    gold     0.1  1986  PRC                 Panda
    silver   1    1986  USA                 Liberty dollar
    gold     0.25 1986  USA                 Liberty 5-dollar piece
    silver   0.5  1986  USA                 Liberty 50-cent piece
    silver   1    1987  USA                 Constitution dollar
    gold     0.25 1987  USA                 Constitution 5-dollar piece
    gold     1    1988  Canada              Maple Leaf



Minimal theory 
--------------

General awk one-liner:

    awk <awk program> <file>

or:

    <shell-command> | awk <awk program> 

`<shell-command>` and `<file>` are addressed as _awk input_.

`<awk program>` is a code following this template (single, not double, quotes): 
 
    'BEGIN   {<init actions>};
     <cond1> {<program actions>};
     <cond2> {<program actions>};
     ...
     END  {<final actions>}'


where:

* `<condX>` condition is most often a regular expression `/re/`, to be matched with awk input lines;  
* `<* actions>` are sequence of _statements_, similar to shell commands, equipped with C-like constructs. 

<br>
`<awk program>` is processed according to the following rules:

1. `BEGIN ...` and `END ...` are optional and executed before or after processing awk input lines.
2. For each line in the awk input, if condition `<condN>` is meat, then the related `<program actions>` block is executed.
3. `{<program actions>}` defaults to `{print $0}`.
     


__Conditions__ can be combined with standard logical operators:
 
        /gold/ || /USA/ && !/1986/
where `&&` has precedence over `||`;

__The`print` statement__.
`print item1 item2` statement prints items on STDOUT.   
 Items can be variables (`X`, `$0`), strings ("hello") or numbers.   
`item1, item2` are collated with the value of the `OFS` variable;   
`item1 item2` _are justapoxed_! Use `item1 " " item2` for spaces or [printf](#printf) for more features.


__Variables__ do not need `$`, i.e.: `print myVar;`   
The following special variables are builtin in awk:

* `FS`: acts as field separator to splits awk input lines in fields. I can be a single character, `FS="c"`; a null string, `FS=""` (then each individual character becomes a separate field); a regular expression without slashes, `FS="re"`; `FS=" "` stands for runs of spaces and tabs and is defaults value. 
* `NF`: the number of fields to read;
* `$1`, `$2`, ...:  1st field, 2nd field. etc. of the current input line,
* `$0`: current input line; 
* `NR`: current put line number.
* `OFS`: string to collate fields when printed.
* `ORS`: output record separator, by default a newline.
* `RS`: Input line (record) separator. Defaults to newline. Set as `FS`.
* `IGNORECASE`: affects FS and RS when are regular expression;

    
Examples
--------

Filter lines by regexp `gold` and count them:

    # awk 'BEGIN {print "Coins"} /gold/{i++; print $0}  END {print i " lines out of " NR}' coins.txt
    Coins
    gold     1    1986  USA                 American Eagle      
    gold     1    1908  Austria-Hungary     Franz Josef 100 Korona 
    gold     1    1984  Switzerland         ingot 
    gold     1    1979  RSA                 Krugerrand 
    gold     0.5  1981  RSA                 Krugerrand 
    gold     0.1  1986  PRC                 Panda                       
    gold     0.25 1986  USA                 Liberty 5-dollar piece
    gold     0.25 1987  USA                 Constitution 5-dollar piece
    gold     1    1988  Canada              Maple Leaf
    9 lines out of 13


Default `print $0` action and condition based on internal awk variable `NR`:

    # awk 'BEGIN {print "First 3 coins"} NR<4' coins.txt
    First 3 coins                                                  
    gold     1    1986  USA                 American Eagle         
    gold     1    1908  Austria-Hungary     Franz Josef 100 Korona 
    silver  10    1981  USA                 ingot



<a name="printf"/>
Formatting with C-style `printf`:
</a>



    # awk '{printf ("%s \t %3.2f\n", $1, $2)}' coins.txt
    gold     1.00                                      
    gold     1.00                                      
    silver   10.00                                     
    gold     1.00                                      
    gold     1.00                                      
    gold     0.50                                      
    gold     0.10                                      
    silver   1.00                                      
    gold     0.25                                      
    silver   0.50                                      
    silver   1.00                                      
    gold     0.25                                      
    gold     1.00



Condition Examples
------------------

    awk 'NR % 6'            # prints all lines except those divisible by 6
    awk 'NR > 5'            # prints from line 6 onwards (like tail -n +6, or sed '1,5d')
    awk '$2 == "foo"'       # prints lines where the second field is "foo"
    awk '$2 ~ /re/'         # prints lines where the 2nd field mateches the regex /re/
    awk 'NF >= 6'           # prints lines with 6 or more fields
    awk '/foo/ && !/bar/'   # prints lines that match /foo/ but not /bar/
    awk '/foo/ || /bar/'    # prints lines that match /foo/ or /bar/ (like grep -e 'foo' -e 'bar')
    awk '/foo/,/bar/'       # prints from line matching /foo/ to line matching /bar/, inclusive
    awk 'NF'                # prints only nonempty lines (or: removes empty lines, where NF==0)
    awk 'NF--'              # removes last field and prints the line

By adding an action `{...}` one can print a specific field, rather than the whole line, e.g.:

    awk '$2 ~ /re/{print $3 " " $4}'
    
prints the third and fourth field of lines where the second  field mateches the regex /re/.



Some string functions
----------------------

`substr()` function:

    # awk '{print substr($3,3) " " substr($4,1,3)}' 
    86 USA                                            
    08 Aus                                            
    81 USA                                            
    84 Swi                                            
    79 RSA                                            
    81 RSA                                            
    86 PRC                                            
    86 USA                                            
    86 USA                                            
    86 USA                                            
    87 USA                                            
    87 USA                                            
    88 Can                                            


`match(s, r [, arr])` returns the position in `s` where the regex `r` occurs  and sets the values of `RSTART` and `RLENGTH`. 
If the argument `arr` is provided, 
it returns the array `arr` where elements are set to the matched  parenthesized subexpression. The 0’th element matches of `arr` is set to the  entire  regex match. Also expressions  `arr[n, "start"]` and `arr[n, "length"]` provide the starting position and length  of each matching substring.

More string functions:

    sub(/regexp/, "newstring"[, target])
    gsub(/regexp/, "newstring"[, target])
    toupper("string")
    tolower("string")



Statements
----------

A simple statement is often any of the following:

    variable = expression 
    print [ expression-list ] 
    printf format [ , expression-list ] 
    next # skip remaining patterns on this input line
    exit # skip the rest of the input
    
    
If `stat1` and `stat2` are statements, the following are also statements: 

    {stat}
    
    {stat1;  stat2}

    {stat1 
    stat2}

    if ( conditional ) statement [ else statement ]

The following standard C-like are constructs are statements: 
    
    if ( conditional ) statement [ else statement ]
    while ( conditional ) statement
    for ( expression ; conditional ; expression ) statement
    break    # usual C meaning 
    continue # usual C meaning 


A C-style loop to print the variable length description element, starting with field 4:

    # awk '{out=""; for(i=4;i<=NF;i++){out=out" "$i}; print out}' coins.txt
    USA American Eagle                    
    Austria-Hungary Franz Josef 100 Korona
    USA ingot                             
    Switzerland ingot                     
    RSA Krugerrand                        
    RSA Krugerrand                        
    PRC Panda                             
    USA Liberty dollar                    
    USA Liberty 5-dollar piece            
    USA Liberty 50-cent piece             
    USA Constitution dollar               
    USA Constitution 5-dollar piece       
    Canada Maple Leaf

Note that `i` is initialized to 0.

If conditions and calculations applied to nuneric fields:

    # awk '/gold/ {if($3<1980) print $0 "$" 425*$2}' coins.txt    
    gold     1    1908  Austria-Hungary     Franz Josef 100 Korona      $425
    gold     1    1979  RSA                 Krugerrand                  $425   




AWK executable script
---------------------

    #!/usr/bin/gawk -f
    # This is a comment
    (pattern) {action}
    ...

__Passing shell variables__

    # var="hello"
    # awk -v x="$var" 'BEGIN {print x}'
    hello
    

## Hello world
The Hello world example is as simple as:

    awk 'BEGIN {print "Hello world"}'


The most basic `awk` program consists of a true value (typically `1`) and makes `awk` echo its input:

    $ date | awk '1'
    Mon Jul 25 11:12:05 CEST 2016

Since "hello world" is also a true value, you could also say:

    $ date | awk '"hello world"'
    Mon Jul 25 11:12:05 CEST 2016

However, your intention becomes much clearer if you write

    $ date | awk '{print}'
    Mon Jul 25 11:12:05 CEST 2016

instead.

## How to run AWK programs
If the program is short, you can include it in the command that runs awk:

    awk -F: '{print $1, $2}' /etc/passwd

In this example, using command line switch `-F:` we advise awk to use : as input fields delimiter. Is is the same like 

    awk 'BEGIN{FS=":"}{print $1,$2}' file

Alternativelly, we can save the whole awk code in an awk file and call this awk programm like this:

    awk -f 'program.awk' input-file1 input-file2 ...


program.awk can be whatever multiline program, i.e : 

    # file print_fields.awk
    BEGIN {print "this is a header"; FS=":"}
    {print $1, $2}
    END {print "that was it"}

And then run it with:

    awk -f print_fields.awk /etc/passwd   #-f advises awk which program file to load

Or More generally:

    awk -f program-file input-file1 input-file2 ...

The advantage of having the program in a seperate file is that you can write the programm with correct identation to make sense, you can include comments with # , etc

