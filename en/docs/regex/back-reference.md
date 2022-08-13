---
title: "Back reference"
slug: "back-reference"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Basics
Back references are used to match the same text previously matched by a capturing group. This both helps in reusing previous parts of your pattern and in ensuring two pieces of a string match.

For example, if you are trying to verify that a string has a digit from zero to nine, a separator, such as hyphens, slashes, or even spaces, a lowercase letter, another separator, then another digit from zero to nine, you could use a regex like this:

    [0-9][-/ ][a-z][-/ ][0-9]

This would match `1-a-4`, but it would _also_ match `1-a/4` or `1 a-4`. If we want the separators to match, we can use a [capture group][1] and a back reference. The back reference will look at the match found in the indicated capture group, and ensure that the location of the back reference matches exactly.

Using our same example, the regex would become:

    [0-9]([-/ ])[a-z]\1[0-9]

The `\1` denotes the first capture group in the pattern. With this small change, the regex now matches `1-a-4` or `1 a 4` but not `1 a-4` or `1-a/4`.

The number to use for your back reference depends on the location of your capture group. The number can be from one to nine and can be found by counting your capture groups.

    ([0-9])([-/ ])[a-z][-/ ]([0-9])
    |--1--||--2--|          |--3--|

Nested capture groups change this count slightly. You first count the exterior capture group, then the next level, and continue until you leave the nest:

    (([0-9])([-/ ]))([a-z])
     |--2--||--3--|
    |-------1------||--4--|


  [1]: https://www.wikiod.com/regex/capture-groups

## Ambiguous Backreferences
**Problem:**  You need to match text of a certain format, for example:

<!-- language-all: lang-none -->

    1-a-0
    6/p/0
    4 g 0

That's a digit, a separator (one of `-`, `/`, or a space), a letter, the same separator, and a zero.

**Naïve solution:**  Adapting the regex from the [Basics example][1], you come up with this regex:

    [0-9]([-/ ])[a-z]\10

But that probably won't work.  Most regex flavors support more than nine capturing groups, and very few of them are smart enough to realize that, since there's only one capturing group, `\10` must be a backreference to group 1 followed by a literal `0`.  Most flavors will treat it as a backreference to group 10.  A few of those will throw an exception because there is no group 10; the rest will simply fail to match.

There are several ways to avoid this problem.  One is to use [named groups][2] (and named backreferences):

    [0-9](?<sep>[-/ ])[a-z]\k<sep>0

If your regex language supports it, the format `\g{n}` (where `n` is a number) can enclose the backreference number in curly brackets to separate it from any digits after it:

    [0-9]([-/ ])[a-z]\g{1}0

Another way is to use extended regex formatting, separating the elements with insignificant whitespace (in Java you'll need to escape the space in the brackets):

    (?x) [0-9] ([-/ ]) [a-z] \1 0

If your regex flavor doesn't support those features, you can add unnecessary but harmless syntax, like a non-capturing group:

    [0-9]([-/ ])[a-z](?:\1)0

...or a dummy quantifier (this is possibly the only circumstance in which `{1}` is useful):

    [0-9]([-/ ])[a-z]\1{1}0



  [1]: https://www.wikiod.com/regex/back-reference#Basics
  [2]: https://www.wikiod.com/regex/capture-groups#Named Capture Groups

