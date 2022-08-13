---
title: "Find and Replace"
slug: "find-and-replace"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Substitute Command
This command:

    :s/foo/bar/g

substitutes each occurrence of `foo` with `bar` on the current line.

    fool around with a foodie

becomes

    barl around with a bardie

If you leave off the last `/g`, it will only replace the first occurence on the line. For example, 

    :s/foo/bar

On the previous line would become

    barl around with a foodie

This command:

    :5,10s/foo/bar/g

performs the same substitution in lines 5 through 10.

This command    

     :5,$s/foo/bar/g

performs the same substitution from line 5 to the end of the file.

This command:

    :%s/foo/bar/g

performs the same substitution on the whole buffer.

If you are in visual mode and hit the colon, the symbol `'<,'>`
will appear. You can then do this

    :'<,'>s/foo/bar/g

and have the substitution occur within your visual mode selection.




This command:

    :%s/foo/bar/gc

is equivalent to the command above but asks for confirmation on each occurence thanks to the `/c` flag (for "confirmation").

See `:help :s` and `:help :s_flags`.

See also [this section on command-line ranges][1].


  [1]: https://www.wikiod.com/vim/command-line-ranges


## Replace with or without Regular Expressions
This substitute command can use [Regular Expressions][1] and will match any instance of `foo` *followed by any( one ) character* since the period `.` in Regular Expressions matches any character, hence the following command will match all instances of `foo` followed by any character in the current line.
    
    :s/foo./bar/g
<pre>
  <b>1</b> fooing fooes fool foobar foosup
</pre>
will become
<pre>
  <b>1</b> barng bars bar barar barup
</pre>

If you want to match the literal `.` period you can escape it in the search field with a backslash `\`.

    :s/foo\./bar/g
<pre>
  <b>1</b> fooing fooes foo.l foo.bar foosup
</pre>
will become
<pre>
  <b>1</b> fooing fooes barl barbar foosup
</pre>
Or disable all pattern matching by following the `s` command with `no`.

    :sno/foo./bar/g
<pre>
  <b>1</b> fooing fooes foo.l foo.bar foosup
</pre>
will raise an error
<pre>
  <b>E486:</b> Pattern not found
</pre>


  [1]: https://en.wikipedia.org/wiki/Regular_expression

