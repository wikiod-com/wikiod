---
title: "Easter Eggs"
slug: "easter-eggs"
draft: false
images: []
weight: 9580
type: docs
toc: true
---

## When you're feeling down
Problem: Vim users are not always happy.

Solution: Make them happy.

<!-- if version [gte 7.4] -->
`:smile`
<!-- end version if -->



_Note: Requires patch version ≥[7.4.1005](https://github.com/vim/vim/commit/86e179dbe75010e9545e1a2fcc92a15d57bf27fd)_

## Help!
For the distressed user, vim provides words of wisdom.

    :help!

## Looking for the Holy Grail
Check this out:

    :help holy-grail

## The Answer
Vim knows The Answer:

`:help 42`

Vim will open the `usr_42.txt` document from the manual and show the following text:

> What is the meaning of life, the universe and everything?  **42**
>
> Douglas Adams, the only person who knew what this question really was about is
now dead, unfortunately.  So now you might wonder what the meaning of death
is...


## Knights who say Ni!
Check this out: 

    :Ni!

[Monty Python and the Holy Grail][1]

  [1]: https://en.wikipedia.org/wiki/Monty_Python_and_the_Holy_Grail

## Ceci n'est pas une pipe
If you look for the help section of `|` or `bar` : `:h bar` you can see:


                                                        bar
    |                       To screen column [count] in the current line.
                            exclusive motion.  Ceci n'est pas une pipe.

This is a reference to the painting _La trahison des images_ by René Magritte.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/1ga0w.jpg

## When a user is getting bored
Search for `:h UserGettingBored`

                            *UserGettingBored*
    UserGettingBored        When the user presses the same key 42 times.
                            Just kidding! :-)


## Spoon
Instead of looking for the `fork` help, you can search for the `spoon` help:


    :h spoon

                                                        fork spoon
    For executing external commands fork()/exec() is used when possible, otherwise
    system() is used, which is a bit slower.  The output of ":version" includes ...

## nunmap
`:help map-modes`

    :nunmap can also be used outside of a monastery.


