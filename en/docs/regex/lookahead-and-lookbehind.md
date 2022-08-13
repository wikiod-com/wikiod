---
title: "Lookahead and Lookbehind"
slug: "lookahead-and-lookbehind"
draft: false
images: []
weight: 9832
type: docs
toc: true
---

## Syntax
- **Positive lookahead:** `(?=pattern)`
- **Negative lookahead:** `(?!pattern)`
- **Positive lookbehind**: `(?<=pattern)`
- **Negative lookbehind**: `(?<!pattern)`

Not supported by all regex engines.

Additionally, many regex engines limit the patterns inside lookbehinds to fixed-length strings. For example the pattern `(?<=a+)b` should match the `b` in `aaab` but throws an error in Python.

Capturing groups are allowed and work as expected, including backreferences. The lookahead/lookbehind itself is not a capturing group, however.

## Basics
A **positive lookahead** `(?=123)` asserts the text is followed by the given pattern, without including the pattern in the match. Similarly, a **positive lookbehind** `(?<=123)` asserts the text is preceded by the given pattern. Replacing the `=` with `!` negates the assertion.

---

**Input**: `123456`

- `123(?=456)` matches `123` (*positive lookahead*)
- `(?<=123)456` matches `456` (*positive lookbehind*)
- `123(?!456)` fails (*negative lookahead*)
- `(?<!123)456` fails (*negative lookbehind*)

---
**Input**: `456`

- `123(?=456)` fails
- `(?<=123)456` fails
- `123(?!456)` fails
- `(?<!123)456` matches `456`

## Simulating variable-length lookbehind with \K
Some regex flavors (Perl, PCRE, Oniguruma, Boost) only support fixed-length lookbehinds, but offer the `\K` feature, which can be used to simulate variable-length lookbehind at the start of a pattern. Upon encountering a `\K`, the matched text up to this point is discarded, and only the text matching the part of the pattern *following* `\K` is kept in the final result.

    ab+\Kc
    
Is equivalent to:

    (?<=ab+)c
    
---

In general, a pattern of the form:

    (subpattern A)\K(subpattern B)
    
Ends up being similar to:

    (?<=subpattern A)(subpattern B)
    
Except when the B subpattern can match the same text as the A subpattern - you could end up with subtly different results, because the A subpattern still consumes the text, unlike a true lookbehind.

## Using lookbehind to test endings
A lookbehind can be used at the end of a pattern to ensure it ends or not in a certain way.

`([a-z ]+|[A-Z ]+)(?<! )` matches sequences of only lowercase or only uppercase words while excluding trailing whitespace.

