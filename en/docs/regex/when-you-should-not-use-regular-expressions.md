---
title: "When you should NOT use Regular Expressions"
slug: "when-you-should-not-use-regular-expressions"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

Because regular expressions are limited to either a regular grammar or a context-free grammar, there are many common misuses of regular expressions. So in this topic there are a few example of when you should *NOT* use regular expressions, but use your favorite language instead.

> *Some people, when confronted with a problem, think:<br />
> “I know, I'll use regular expressions.”<br />
> Now they have two problems.*<br />
> [— Jamie Zawinski](http://regex.info/blog/2006-09-15/247)

## Simple string operations
Because *Regular Expressions* can do a lot, it is tempting to use them for the simplest operations. But using a regex engine has a cost in memory and processor usage: you need to compile the expression, store the automaton in memory, initialize it and then feed it with the string to run it.

And there are many cases where it's just not necessary to use it! Whatever your language of choice is, it always has the basic string manipulation tools. So, as a rule, when there's a tool to do an action in your standard library, use that tool, not a regex:

* split a string? 

For example the following snippet works in Python, Ruby and Javascript:

    'foo.bar'.split('.')

Which is easier to read and understand, as well as much more efficient than the (somehow) equivalent regular expression:

    (\w+)\.(\w+)

* Strip trailing spaces?

The same applies to trailing spaces!

    'foobar     '.strip() # python or ruby
    'foobar     '.trim() // javascript

Which would be equivalent to the following expression:

    ([^\n]*)\s*$ # keeping \1 in the substitution


## Parsing HTML (or XML, or JSON, or C code, or…)
If you want to extract something from a webpage (or any representation/programming language), a regex is the wrong tool for the task. You should instead use your language's libraries to achieve the task.

If you want to read HTML, or XML, or JSON, just use the library that parses it properly and serves it as usable objects in your favorite language! You'll end up with readable and more maintainable code, and you won't end up

* http://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags/1732454#1732454
* http://stackoverflow.com/questions/23548523/python-parsing-html-using-regular-expressions/23548604#23548604
* http://stackoverflow.com/questions/28933020/is-there-a-regex-to-generate-all-integers-for-a-certain-programming-language/28933318#28933318


## Matching pairs (like parenthesis, brackets…)
Some regex engines (such as .NET) can handle context-free expressions, and will work it out. But that's not the case for most standard engines. And even if they do, you'll end up having a complex hard-to-read expression, whereas using a parsing library could make the job easier.

* http://stackoverflow.com/questions/23654329/how-to-find-all-possible-regex-matches-in-python

