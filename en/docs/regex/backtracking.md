---
title: "Backtracking"
slug: "backtracking"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## What causes Backtracking?
To find a match, the regex engine will consume characters one by one. When a partial match begins, the engine will remember the start position so it can go back in case the following characters don't complete the match.

 - If the match is complete, the is no backtracking
 - If the match isn't complete, the engine will backtrack the string (like when you rewind an old tape) to try to find a whole match.

For example: `\d{3}[a-z]{2}` against the string `abc123def` will be browsed as such:


    abc123def
    ^ Does not match \d
    abc123def
     ^ Does not match \d
    abc123def
      ^ Does not match \d
    abc123def
       ^ Does match \d (first one)
    abc123def
        ^ Does match \d (second one)
    abc123def
         ^ Does match \d (third one)
    abc123def
          ^ Does match [a-z] (first one)
    abc123def
           ^ Does match [a-z] (second one)
               MATCH FOUND

Now lets change the regex to `\d{2}[a-z]{2}` against the same string (`abc123def`):

    abc123def
    ^ Does not match \d
    abc123def
     ^ Does not match \d
    abc123def
      ^ Does not match \d
    abc123def
       ^ Does match \d (first one)
    abc123def
        ^ Does match \d (second one)
    abc123def
         ^ Does not match [a-z]
    abc123def
        ^ BACKTRACK to catch \d{2} => (23)
    abc123def
          ^ Does match [a-z] (first one)
    abc123def
           ^ Does match [a-z] (second one)
               MATCH FOUND

## Why can backtracking be a trap?
Backtracking can be caused by optional quantifiers or alternation constructs, because the regex engine will try to explore every path. If you run the regex `a+b` against `aaaaaaaaaaaaaa` there is no match and the engine will find it pretty fast.

But if you change the regex to `(aa*)+b` the number of combinations will grow pretty fast, and most (not optimized) engines will try to explore all the paths and will take an eternity to try to find a match or throw a timeout exception. This is called **catastrophic backtracking**.

Of course, `(aa*)+b` seems a newbie error but it's here to illustrate the point and sometimes you'll end up with the same issue but with more complicated patterns.

A more extreme case of catastrophic backtracking occurs with the regex `(x+x+)+y` (you've probably seen it before [here](http://www.regular-expressions.info/catastrophic.html) and [here](https://blog.codinghorror.com/regex-performance/)), which needs exponential time to figure out that a string that contains `x`s and nothing else (e.g `xxxxxxxxxxxxxxxxxxxx`) don't match it.

# How to avoid it?

Be as specific as possible, reduce as much as possible the possible paths. Note that some regex matchers are not vulnerable to backtracking, such as those included in `awk` or `grep` because they are based on [Thompson NFA][1].


  [1]: https://swtch.com/~rsc/regexp/regexp1.html

