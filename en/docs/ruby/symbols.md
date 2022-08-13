---
title: "Symbols"
slug: "symbols"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

## Syntax
- :symbol
- :'symbol'
- :"symbol"
- "symbol".to_sym
- %s{symbol}


**Advantages of using symbols over strings:**
--------------------------------

**1. A Ruby symbol is an object with O(1) comparison**

To compare two strings, we potentially need to look at every character. For two strings of length N, this will require N+1 comparisons

    def string_compare str1, str2
      if str1.length != str2.length
            return false
      end
      for i in 0...str1.length
        return false if str1[i] != str2[i]
      end
      return true
    end
    string_compare "foobar", "foobar"

But since every appearance of :foobar refers to the same object, we can compare symbols by looking at object IDs. We can do this with a single comparison.(O(1))

    def symbol_compare sym1, sym2
      sym1.object_id == sym2.object_id
    end
    symbol_compare :foobar, :foobar

**2. A Ruby symbol is a label in a free-form enumeration**

In C++, we can use “enumerations” to represent families of related constants:

    enum BugStatus { OPEN, CLOSED };
    BugStatus original_status = OPEN;
    BugStatus current_status  = CLOSED;

But because Ruby is a dynamic language, we don’t worry about declaring a BugStatus type, or keeping track of the legal values. Instead, we represent the enumeration values as symbols:

    original_status = :open
    current_status  = :closed

**3. A Ruby symbol is a constant, unique name**

In Ruby, we can change the contents of a string:

    "foobar"[0] = ?b # "boo"

But we can’t change the contents of a symbol:

    :foobar[0]  = ?b # Raises an error

**4. A Ruby symbol is the keyword for a keyword argument**

When passing keyword arguments to a Ruby function, we specify the keywords using symbols:

    # Build a URL for 'bug' using Rails.
    url_for :controller => 'bug',
            :action => 'show',
            :id => bug.id

**5. A Ruby symbol is an excellent choice for a hash key**

Typically, we’ll use symbols to represent the keys of a hash table:

    options = {}
    options[:auto_save]     = true
    options[:show_comments] = false

## Creating a Symbol
The most common way to create a `Symbol` object is by prefixing the string identifier with a colon:

    :a_symbol       # => :a_symbol
    :a_symbol.class # => Symbol

Here are some alternative ways to define a `Symbol`, in combination with a `String` literal:

    :"a_symbol"
    "a_symbol".to_sym

Symbols also have a `%s` sequence that supports arbitrary delimiters similar to how `%q` and `%Q` work for strings:

    %s(a_symbol)
    %s{a_symbol}

The `%s` is particularly useful to create a symbol from an input that contains white space:

    %s{a symbol} # => :"a symbol"

While some interesting symbols (`:/`, `:[]`, `:^`, etc.) can be created with certain string identifiers, note that symbols cannot be created using a numeric identifier:

    :1 # => syntax error, unexpected tINTEGER, ...
    :0.3 # => syntax error, unexpected tFLOAT, ...

Symbols may end with a single `?` or `!` without needing to use a string literal as the symbol's identifier:

    :hello?  # :"hello?" is not necessary.
    :world!  # :"world!" is not necessary.

Note that all of these different methods of creating symbols will return the same object:

    :symbol.object_id == "symbol".to_sym.object_id
    :symbol.object_id == %s{symbol}.object_id

Since Ruby 2.0 there is a shortcut for creating an array of symbols from words:

    %i(numerator denominator) == [:numerator, :denominator]

## Converting a String to Symbol
Given a `String`:

    s = "something"

there are several ways to convert it to a `Symbol`:

    s.to_sym
    # => :something
    :"#{s}"
    # => :something


## Converting a Symbol to String
Given a `Symbol`:

    s = :something

The simplest way to convert it to a `String` is by using the `Symbol#to_s` method:

    s.to_s
    # => "something"

Another way to do it is by using the `Symbol#id2name` method which is an alias for the `Symbol#to_s` method. But it's a method that is unique to the `Symbol` class:

    s.id2name
    # => "something"

