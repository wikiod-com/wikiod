---
title: "Getting started with yaml"
slug: "getting-started-with-yaml"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Comments
    # This comment occupies a whole line
    - some item # This comment succeeds content of a line
    - http://example.com/#nocomment
    - "This # does not introduce a comment."
    - |
        This is a block scalar.
        A # inside it does not introduce a comment.
       # unless it is less indented than the first line (this is one)

Note that for a `#` to introduce a comment, it must either

 * occur at the beginning of a line, or
 * be preceded by whitespace.

`#` must always be followed by whitespace. `#` inside quoted scalars never start comments. `#` may introduce comments at the end of block scalars, but therefore, it must be less indented than the block scalar's base indentation (which is usually determined by the indentation of its first non-empty line).

## Basic Yaml syntax
YAML is a text based format allowing to store structured data in a hierarchy. YAML is designed to be human and machine readable with a minimum of overhead. The YAML specification can be found at [yaml.org](http://yaml.org/spec/1.2/spec.html). There is also a [reference card](http://www.yaml.org/refcard.html)

Comments start with `#` and go till newline, comments must be separated from other tokens by whitespace. Whitespace isn't free, indentation must be spaces, not tabs. YAML will consider that lines prefixed with more spaces than the parent key are contained inside it. Moreover, all lines must be prefixed with the same amount of spaces to belong to the same map.

YAML has sequences and mappings as collection types, both can be represented in flow and block style.

An sequence of scalar strings in YAML looks like:

    [ one, two, three ]   # flow style

    # or block style

    - one
    - two
    - three
    
A mapping consists of key/value pairs:

    index: 4  # block style
    name: nali

    # or 

    { index: 4, name: nali }   # flow style

    # or nested (equivalent of { level: { one: { two: fun } } }):

    level:

      one:

        two: fun



## Splitting text strings over multiple lines
```
- Without quotes:
   You can just
   split a long piece of text like this.
- With quotes:
    "[But be careful:
     if you \"need\" punctuation, put double quotes around it. You can ev\
     en split without spaces by using backslashes."
- Or single quotes:
    'This works
     but isn''t as flexible'
- If you want to keep those new line characters: | 
    Then do
    it this way with 
    a pipe (|) character. (This string has three \n characters)
- Or you can have just the one final new line: >
    This string has
    just one \n character, at the very end.
- Block indicators:
    Look up >-, >+, |- and |+ for fine tuning.
```

## Escaping Characters
YAML supports three styles of escape notation:

1. Entity Escapes 

   a. space: "&amp;#x20;"

   b. colon: "&amp;#58;"

   c. ampersand: "&amp;amp;"

2. Unicode Escapes

   a. space: "\u0020"

   b. single quote: "\u0027"

   c. double quote: "\u0022"

3. Quoted Escapes

   a. double quote in single quote: 'Is "I always lie" a true statement?'

   b. nested double quote: " She said, \"I quit\" "

   c. nested single quote: ' He was speechless: '' '

## Basic YAML Types
    integer: 25
    string: "25"
    float: 25.0
    boolean: true
    null type: null

## YAML Sequential Data
Same list level:

    - Cat
    - Dog
    - Goldfish

Nested List:

    -
     - Cat
     - Dog
     - Goldfish

## Block Style Mappings
With implicit keys:

    key: value
    another key:
      - some
      - more
      - values
    [1, 2, 3]: last value, which has a flow style key

---

With implicit and explicit keys:

    ? key
    : value
    another key:
      - some
      - more
      - values
    ? [1, 2, 3]
    : last value, which has a flow style key

`key`, `another key` and `[1, 2, 3]` are keys of the same mapping, although they use different key styles.

---

Nested mappings:

    first level:
      second level:
        ? third level
        :
          forth level: value of implicit key
        ? third level, second key
        : value of explicit key
      ?
        mapping as: key of
        another: mapping
      : scalar value of mapping key
    first level, second key:
      last value

