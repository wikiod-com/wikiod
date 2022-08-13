---
title: "Regex Pitfalls"
slug: "regex-pitfalls"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Why doesn't dot (.) match the newline character ("\n")?
`.*` in regex basically means "catch **everything** until the end of input".

So, for simple strings, like `hello world`, `.*` works perfectly. But if you have a string representing, for example, lines in a file, these lines would be separated by a *line separator*, such as `\n` (newline) on Unix-like systems and `\r\n` (carriage return and newline) on Windows.

By default in most regex engines, `.` **doesn't** match newline characters, so the matching stops at the end of each *logical line*. If you want `.` to match **really** everything, including newlines, you need to enable "dot-matches-all" mode in your regex engine of choice (for example, add [`re.DOTALL`](https://docs.python.org/3/library/re.html#re.DOTALL) flag in Python, or [`/s`](http://php.net/manual/en/reference.pcre.pattern.modifiers.php) in PCRE. 

## Why does a regex skip some closing brackets/parentheses and match them afterwards?
Consider this example:

> He went into the cafe "Dostoevski" and said: "Good evening."

Here we have two sets of quotes. Let's assume we want to match both, so that our regex matches at `"Dostoevski"` **and** `"Good evening."`

At first, you could be tempted to keep it simple:

    ".*"  # matches a quote, then any characters until the next quote

But it doesn't work: it matches from the first quote in `"Dostoevski"` and **until** the closing quote in `"Good evening."`, including the `and said: ` part. [Regex101 demo](https://regex101.com/r/zvb3J1/3)

## Why did it happen? ##

This happens because the regex engine, when it encounters `.*`, "eats up" all of the input to the very end. Then, it needs to match the final `"`. So, it "backs off" from the end of the match, letting go of the matched text until the first `"` is found - and it is, of course, the last `"` in the match, at the end of `"Good evening."` part.

## How to prevent this and match exactly to the first quotes?

Use `[^"]*`. It doesn't eat all the input - only until the first `"`, just as needed. [Regex101 demo](https://regex101.com/r/zvb3J1/4) 

