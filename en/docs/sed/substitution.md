---
title: "Substitution"
slug: "substitution"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Using different delimiters
POSIX/IEEE Open Group Base Specification [says][1]: 

> **[2addr] s/BRE/replacement/flags**
> 
> Substitute the replacement string for instances of the BRE in the
> pattern space. **Any character other than backslash or newline** can
> be used instead of a slash to delimit the BRE and the replacement.
> Within the BRE and the replacement, the BRE delimiter itself can be
> used as a literal character if it is preceded by a backslash.

There are cases when the delimiter `/` for `sed` replacement is in the BRE or replacement, triggering errors like:

    $ echo "2/3/4" | sed "s/2/3/X/"
    sed: -e expression #1, char 7: unknown option to `s'

For this, we can use different delimiters such as `#` or `_` or even a space:

    $ echo "2/3/4" | sed "s#2/3#X#"
    X/4
    $ echo "2/3/4" | sed "s_2/3_X_"
    X/4
    $ echo "2/3/4" | sed "s 2/3 X "
    X/4

  [1]: http://pubs.opengroup.org/onlinepubs/009604599/utilities/sed.html




## Substitution Using Shell Variables
Variables inside single quotes `'` don't get expanded by POSIX compatible shells, so using a shell variable in a `sed` substitution requires the use of double quotes `"` instead of single quotes `'`:

    $ var="he"
    $ echo "hello" | sed "s/$var/XX/"
    XXllo

    $ var="he"
    $ echo "hello" | sed 's/$var/XX/'
    hello

Be careful of command injection when evaluating variables:

    $ var='./&/;x;w/etc/passwd
    > x;s/he'
    $ echo "hello" | sed "s/$var/XX/"
    sed: /etc/passwd: Permission denied

If the above was run as root the output would have been indistinguishable from the first example, and the contents of `/etc/passwd` would be destroyed.

## Backreference
Using escaped brackets, you can define a capturing group in a pattern that can be backreferenced in the substitution string with `\1`:

    $ echo Hello world! | sed 's/\(Hello\) world!/\1 sed/'
    Hello sed

With multiple groups:

    $ echo one two three | sed 's/\(one\) \(two\) \(three\)/\3 \2 \1/'
    three two one

<!-- if version [eq BSD sed] [eq GNU sed] -->

When using extended regular expressions (see [Additional Options][soadop]) parenthesis perform grouping by default, and do not have to be escaped:

    $ echo one two three | sed -E 's/(one) (two) (three)/\3 \2 \1/'
    three two one

<!-- end version if -->

Words consisting of letter, digits and underscores can be matched using the expression
`[[:alnum:]_]\{1,\}`:

    $ echo Hello 123 reg_exp | sed 's/\([[:alnum:]_]\{1,\}\) \([[:alnum:]_]\{1,\}\) \([[:alnum:]_]\{1,\}\)/\3 \2 \1/'
    reg_exp 123 Hello

<!-- if version [eq GNU sed] -->

The sequence `\w` is equivalent to `[[:alnum:]_]`

    $ echo Hello 123 reg_exp | sed 's/\(\w\w*\) \(\w\w*\) \(\w\w*\)/\3 \2 \1/'
    reg_exp 123 Hello

<!-- end version if -->

[soadop]: https://www.wikiod.com/sed/additional-options

## Pattern flags - occurrence replacement
If we want to replace only the first occurrence in a line, we use `sed` as usual:

    $ cat example
    aaaaabbbbb
    aaaaaccccc
    aaaaaddddd
    $ sed 's/a/x/' example
    xaaaabbbbb
    xaaaaccccc
    xaaaaddddd
 
But what if we want to replace all occurrences?
 
We just add the `g` pattern flag at the end:
 
    $ sed 's/a/x/g' example
    xxxxxbbbbb
    xxxxxccccc
    xxxxxddddd
 
And if we want to replace one specific occurrence, we can actually specify which one:
 
    $ sed 's/a/x/3' example
    aaxaabbbbb
    aaxaaccccc
    aaxaaddddd
 
`/3` being the 3rd occurrence.

<br>

<!-- if version [eq GNU sed] -->

From `info sed`, see [GNU sed manual](https://www.gnu.org/software/sed/manual/sed.html#The-_0022s_0022-Command) for online version

> the POSIX standard does not specify what should happen when you mix
> the `g` and NUMBER modifiers, and currently there is no widely agreed
> upon meaning across `sed` implementations. For GNU `sed`, the
> interaction is defined to be: ignore matches before the NUMBERth, and
> then match and replace all matches from the NUMBERth on.

    $ sed 's/b/y/2g' example
    aaaaabyyyy
    aaaaaccccc
    aaaaaddddd

    $ sed 's/c/z/g3' example
    aaaaabbbbb
    aaaaacczzz
    aaaaaddddd

<!-- end version if -->

