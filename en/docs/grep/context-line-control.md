---
title: "Context line control"
slug: "context-line-control"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

`-A`, `-B` and `-C` options are not available in POSIX (see the [POSIX specifications for `grep`][1]).


  [1]: http://pubs.opengroup.org/onlinepubs/009604499/utilities/grep.html

## Print lines before and/or after matching pattern
Usually `grep` prints only matching lines. In the example below `seq 9` generates a list of numbers from 1 to 9, one per line, and `grep` prints a single matching line:

    seq 9 | grep 5
    # 5

The `-C n` option (or `--context=n` in long form) prints `n` lines before and after each matching line, in addition to the matching line itself:

    seq 9 | grep -C 2 '5'
    # 3
    # 4
    # 5
    # 6
    # 7

Naturally, fewer than `n` lines will be printed if end-of-file or beginning-of-file is reached.

If we want to print lines only before or only after, but not both, we can use `-B n` (`--before-context=n`) or `-A n` (`--after-context=n`):

    seq 9 | grep -B 2 '5'
    # 3
    # 4
    # 5

    seq 9 | grep -A 2 '5'
    # 5
    # 6
    # 7

Note these options are not available in POSIX (see the [POSIX specifications for `grep`][1]).

If the contexts of two or more matching lines overlap, then all the lines are printed together as one large context. In the example below, `5` is part of the context of both `3` and `7`:

    seq 9 | grep -E --context=2 '3|7'
    # 1
    # 2
    # 3
    # 4
    # 5
    # 6
    # 7
    # 8
    # 9

However, if the contexts do not overlap, they are printed out with a group separator line. By default this is double hyphen (`--`):

    seq 9 | grep -E --context=2 '2|8'
    # 1
    # 2
    # 3
    # 4
    # --
    # 6
    # 7
    # 8
    # 9

We can set a different group separator line using the `--group-separator=SEP` option, or suppress this line entirely by using the `--no-group-separator` option:

    seq 9 | grep -E --context=0 --group-separator='****' '2|8'
    # 2
    # ****
    # 8

    seq 9 | grep -E --context=0 --group-separator='' '2|8'
    # 2
    # 
    # 8

    seq 9 | grep -E --context=0 --no-group-separator '2|8'
    # 2
    # 8

Finally, if we choose the `-v` option to print non-matching lines, then context is provided around those lines instead:

    seq 9 | grep -E -v '1|3|4|5|6|7|9'
    # 2
    # --
    # 8

    seq 9 | grep -E -v -C 1 '1|3|4|5|6|7|9'
    # 1
    # 2
    # 3
    # --
    # 7
    # 8
    # 9

  [1]: http://pubs.opengroup.org/onlinepubs/009604499/utilities/grep.html

