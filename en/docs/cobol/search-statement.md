---
title: "SEARCH statement"
slug: "search-statement"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The COBOL `SEARCH` statement comes in two forms.  Linear top to bottom `SEARCH` and a binary `SEARCH ALL` algorithm.  Binary SEARCH ALL assumes a sorted table suitable for a binary search with no elements out of order.

*SEARCH statement*

[![SEARCH statement syntax diagram][1]][1]

*Linear SEARCH*

[![Linear SEARCH syntax diagram][2]][2]

*Binary SEARCH ALL*

[![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/Z8WAj.png
  [2]: https://i.stack.imgur.com/IMjhJ.png
  [3]: https://i.stack.imgur.com/CKWYk.png

## Linear SEARCH
    GCobol >>SOURCE FORMAT IS FIXED
          *> ***************************************************************
          *> Purpose:   Demonstration of the SEARCH verb
          *> Tectonics: cobc -x searchlinear.cob
          *> ***************************************************************
           identification division.
           program-id. searchlinear.
    
           data division.

           working-storage section.
           01 taxinfo.
              05 tax-table occurs 4 times indexed by tt-index.
                 10 province       pic x(2).
                 10 taxrate        pic 999v9999.
                 10 federal        pic 999v9999.
           01 prov                 pic x(2).
           01 percent              pic 999v9999.
           01 percentage           pic zz9.99.

          *> ***************************************************************
           procedure division.
           begin.

          *> ***************************************************************
          *> Sample for linear SEARCH, requires INDEXED BY table
          *> populate the provincial tax table;
          *>  *** (not really, only a couple of sample provinces) ***
          *> populate Ontario and PEI using different field loaders
           move 'AB' to province(1)
           move 'ON' to province(2)
           move 0.08 to taxrate(2)
           move 0.05 to federal(2)
           move 'PE00014000000000' to tax-table(3)
           move 'YT' to province(4)

          *> Find Ontario tax rate
           move "ON" to prov
           perform search-for-taxrate

          *> Setup for Prince Edward Island
           move 'PE' to prov
           perform search-for-taxrate

          *> Setup for failure
           move 'ZZ' to prov
           perform search-for-taxrate

           goback.
          *> ***************************************************************

           search-for-taxrate.
               set tt-index to 1
               search tax-table
                   at end display "no province: " prov end-display
                   when province(tt-index) = prov
                       perform display-taxrate
               end-search
           .

           display-taxrate.
               compute percent = taxrate(tt-index) * 100
               move percent to percentage
               display
                   "found: " prov " at " taxrate(tt-index)
                   "," percentage "%, federal rate of " federal(tt-index)
               end-display
           .

           end program searchlinear.


## Binary SEARCH ALL
    GCobol >>SOURCE FORMAT IS FIXED
          *> ***************************************************************
          *> Purpose:   Demonstration of the SEARCH ALL verb and table SORT
          *> Tectonics: cobc -x -fdebugging-line searchbinary.cob
          *> ***************************************************************
           identification division.
           program-id. searchbinary.
    
           environment division.
           input-output section.
           file-control.
               select optional wordfile
               assign to infile
               organization is line sequential.
    
           data division.
           file section.
           fd wordfile.
               01 wordrec          pic x(20).
    
           working-storage section.
           01 infile               pic x(256) value spaces.
              88 defaultfile       value '/usr/share/dict/words'.
           01 arguments            pic x(256).
    
          *> Note the based clause, this memory is initially unallocated
           78 maxwords             value 500000.
           01 wordlist             based.
              05 word-table occurs maxwords times
                  depending on wordcount
                  descending key is wordstr
                  indexed by wl-index.
                 10 wordstr        pic x(20).
                 10 wordline       usage binary-long.
           01 wordcount            usage binary-long.
    
           01 file-eof             pic 9 value low-value.
              88 at-eof            value high-values.
    
           01 word                 pic x(20).
    
          *> ***************************************************************
           procedure division.
           begin.
    
          *> Get the word file filename
           accept arguments from command-line end-accept
           if arguments not equal spaces
               move arguments to infile
           else
               set defaultfile to true
           end-if
    
          *> ***************************************************************
          *> Try playing with the words file and binary SEARCH ALL
          *>   requires KEY IS and INDEXED BY table description

          *> Point wordlist to valid memory
           allocate wordlist initialized

           open input wordfile

           move low-value to file-eof
           read wordfile
               at end set at-eof to true
           end-read

           perform
               with test before
               until at-eof or (wordcount >= maxwords)
                   add 1 to wordcount
                   move wordrec to wordstr(wordcount)
                   move wordcount to wordline(wordcount)
                   read wordfile
                       at end set at-eof to true
                   end-read
           end-perform

           close wordfile

          *> ensure a non-zero length table when allowing optional file
           evaluate true                  also file-eof
               when wordcount = 0         also any
                   move 1 to wordcount
                   display "No words loaded" end-display
               when wordcount >= maxwords also low-value
                   display "Word list truncated to " maxwords end-display
           end-evaluate

        >>D display "Count: " wordcount ": " wordstr(wordcount) end-display

          *> Sort the words from z to a
           sort word-table on descending key wordstr

          *> fetch a word to search for
           display "word to find: " with no advancing end-display
           accept word end-accept

          *> binary search the words for word typed in and display
          *> the original line number if/when a match is found
           set wl-index to 1
           search all word-table
               at end
                   display
                       word " not a word of " function trim(infile)
                   end-display
               when wordstr(wl-index) = word
                   display
                       word " sorted to " wl-index ", originally "
                       wordline(wl-index) " of " function trim(infile)
                   end-display
           end-search

          *> Release memory ownership
           free address of wordlist

           goback.
           end program searchbinary.


