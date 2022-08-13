---
title: "ActiveSupport"
slug: "activesupport"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

ActiveSupport is a utility gem of general-purpose tools used by the rest of the Rails framework.

One of the primary ways it provides these tools is by monkeypatching Ruby's native types. These are referred to as **Core Extensions**.

## Core Extensions: String Access
# [String#at][at]

Returns a substring of a string object. Same interface as `String#[]`.

```ruby
str = "hello"
str.at(0)      # => "h"
str.at(1..3)   # => "ell"
str.at(-2)     # => "l"
str.at(-2..-1) # => "lo"
str.at(5)      # => nil
str.at(5..-1)  # => ""
```

# [String#from][from]

Returns a substring from the given position to the end of the string.

```ruby
str = "hello"
str.from(0)  # => "hello"
str.from(3)  # => "lo"
str.from(-2) # => "lo"
```

# [String#to][to]

Returns a substring from the beginning of the string to the given position.  
If the position is negative, it is counted from the end of the string.

```ruby
str = "hello"
str.to(0)  # => "h"
str.to(3)  # => "hell"
str.to(-2) # => "hell"
```

`from` and `to` can be used in tandem.

```ruby
str = "hello"
str.from(0).to(-1) # => "hello"
str.from(1).to(-2) # => "ell"
```

# [String#first][first]

Returns the first character, or a given number of characters up to the length of the string.

```ruby
str = "hello"
str.first    # => "h"
str.first(1) # => "h"
str.first(2) # => "he"
str.first(0) # => ""
str.first(6) # => "hello"
```

# [String#last][last]

Returns the last character, or a given number of characters from the end of the string counting backwards.

```ruby
str = "hello"
str.last    # => "o"
str.last(1) # => "o"
str.last(2) # => "lo"
str.last(0) # => ""
str.last(6) # => "hello"
```

  [at]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/access.rb#L27-L29
  [from]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/access.rb#L44-L46
  [to]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/access.rb#L61-L63
  [first]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/access.rb#L75-L83
  [last]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/access.rb#L95-L103

## Core Extensions: String to Date/Time Conversion
# [String#to_time][to_time]

Converts a string to a Time value. The `form` parameter can be either `:utc` or `:local`, defaults to `:local`.

```ruby
"13-12-2012".to_time               # => 2012-12-13 00:00:00 +0100
"06:12".to_time                    # => 2012-12-13 06:12:00 +0100
"2012-12-13 06:12".to_time         # => 2012-12-13 06:12:00 +0100
"2012-12-13T06:12".to_time         # => 2012-12-13 06:12:00 +0100
"2012-12-13T06:12".to_time(:utc)   # => 2012-12-13 06:12:00 UTC
"12/13/2012".to_time               # => ArgumentError: argument out of range
```

# [String#to_date][to_date]

Converts a string to a Date value.

```ruby
"1-1-2012".to_date   # => Sun, 01 Jan 2012
"01/01/2012".to_date # => Sun, 01 Jan 2012
"2012-12-13".to_date # => Thu, 13 Dec 2012
"12/13/2012".to_date # => ArgumentError: invalid date
```

# [String#to_datetime][to_datetime]

Converts a string to a DateTime value.

```ruby
"1-1-2012".to_datetime            # => Sun, 01 Jan 2012 00:00:00 +0000
"01/01/2012 23:59:59".to_datetime # => Sun, 01 Jan 2012 23:59:59 +0000
"2012-12-13 12:50".to_datetime    # => Thu, 13 Dec 2012 12:50:00 +0000
"12/13/2012".to_datetime          # => ArgumentError: invalid date
```

  [to_time]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/conversions.rb#L19-L36
  [to_date]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/conversions.rb#L44-L46
  [to_datetime]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/conversions.rb#L54-L56

## Core Extensions: String Exclusion
# [String#exclude?][exclude]

The inverse of `String#include?`

```ruby
"hello".exclude? "lo" # => false
"hello".exclude? "ol" # => true
"hello".exclude? ?h   # => false
```

  [exclude]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/exclude.rb#L8-L10

## Core Extensions: String Filters
# [String#squish][squish]

Returns a version of the given string without leading or trailing whitespace, and combines all consecutive whitespace in the interior to single spaces. Destructive version `squish!` operates directly on the string instance.

Handles both ASCII and Unicode whitespace.

```ruby
%{ Multi-line
   string }.squish                   # => "Multi-line string"
" foo   bar    \n   \t   boo".squish # => "foo bar boo"
```

# [String#remove][remove]

Returns a new string with all occurrences of the patterns removed. Destructive version `remove!` operates directly on the given string.

```ruby
str = "foo bar test"
str.remove(" test")                 # => "foo bar"
str.remove(" test", /bar/)          # => "foo "
```

# [String#truncate][truncate]

Returns a copy of a given string truncated at a given length if the string is longer than the length.

```ruby
'Once upon a time in a world far far away'.truncate(27)
# => "Once upon a time in a wo..."
```

Pass a string or regexp `:separator` to truncate at a natural break

```ruby
'Once upon a time in a world far far away'.truncate(27, separator: ' ')
# => "Once upon a time in a..."

'Once upon a time in a world far far away'.truncate(27, separator: /\s/)
# => "Once upon a time in a..."
```

# [String#truncate_words][words]

Returns a string truncated after a given number of words.

```ruby
'Once upon a time in a world far far away'.truncate_words(4)
# => "Once upon a time..."
```

Pass a string or regexp to specify a different separator of words

```ruby
'Once<br>upon<br>a<br>time<br>in<br>a<br>world'.truncate_words(5, separator: '<br>')
# => "Once<br>upon<br>a<br>time<br>in..."
```

The last characters will be replaced with the `:omission` string (defaults to "...")

```ruby
'And they found that many people were sleeping better.'.truncate_words(5, omission: '... (continued)')
# => "And they found that many... (continued)"
```

# [String#strip_heredoc][heredoc]

Strips indentation in heredocs. Looks for the least-indented non-empty line and removes that amount of leading whitespace.

```ruby
if options[:usage]
  puts <<-USAGE.strip_heredoc
    This command does such and such.

    Supported options are:
      -h         This message
      ...
  USAGE
end
```

the user would see

```
This command does such and such.

Supported options are:
-h         This message
...
```

  [squish]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/filters.rb#L11-L13
  [remove]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/filters.rb#L30-L32
  [truncate]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/filters.rb#L64-L77
  [words]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/filters.rb#L93-L101
  [heredoc]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/strip.rb#L20-L22

## Core Extensions: String Inflection
# [String#pluralize][pluralize]

Returns of plural form of the string. Optionally takes a `count` parameter and returns singular form if `count == 1`. Also accepts a `locale` parameter for language-specific pluralization.

```ruby
'post'.pluralize             # => "posts"
'octopus'.pluralize          # => "octopi"
'sheep'.pluralize            # => "sheep"
'words'.pluralize            # => "words"
'the blue mailman'.pluralize # => "the blue mailmen"
'CamelOctopus'.pluralize     # => "CamelOctopi"
'apple'.pluralize(1)         # => "apple"
'apple'.pluralize(2)         # => "apples"
'ley'.pluralize(:es)         # => "leyes"
'ley'.pluralize(1, :es)      # => "ley"
```

# [String#singularize][singularize]

Returns the singular form of the string. Accepts an optional `locale` parameter.

```ruby
'posts'.singularize            # => "post"
'octopi'.singularize           # => "octopus"
'sheep'.singularize            # => "sheep"
'word'.singularize             # => "word"
'the blue mailmen'.singularize # => "the blue mailman"
'CamelOctopi'.singularize      # => "CamelOctopus"
'leyes'.singularize(:es)       # => "ley"
```

# [String#constantize][constantize]

Tries to find a declared constant with the name specified in the string. It raises a `NameError` when the name is not in CamelCase or is not initialized.

```ruby
'Module'.constantize  # => Module
'Class'.constantize   # => Class
'blargle'.constantize # => NameError: wrong constant name blargle
```

# [String#safe_constantize][safe]

Performs a `constantize` but returns `nil` instead of raising `NameError`.

```ruby
'Module'.safe_constantize  # => Module
'Class'.safe_constantize   # => Class
'blargle'.safe_constantize # => nil
```

# [String#camelize][camelize]

Converts strings to UpperCamelCase by default, if `:lower` is given as param converts to lowerCamelCase instead.

alias: `camelcase`

**Note:** will also convert `/` to `::` which is useful for converting paths to namespaces.

```ruby
'active_record'.camelize                # => "ActiveRecord"
'active_record'.camelize(:lower)        # => "activeRecord"
'active_record/errors'.camelize         # => "ActiveRecord::Errors"
'active_record/errors'.camelize(:lower) # => "activeRecord::Errors"
```

# [String#titleize][titleize]

Capitalizes all the words and replaces some characters in the string to create a nicer looking title.

alias: `titlecase`

```ruby
'man from the boondocks'.titleize # => "Man From The Boondocks"
'x-men: the last stand'.titleize  # => "X Men: The Last Stand"
```

# [String#underscore][underscore]

Makes an underscored, lowercase form from the expression in the string. The reverse of `camelize`.

**Note:** `underscore` will also change `::` to `/` to convert namespaces to paths.

```ruby
'ActiveModel'.underscore         # => "active_model"
'ActiveModel::Errors'.underscore # => "active_model/errors"
```

# [String#dasherize][dasherize]

Replaces underscores with dashes in the string.

```ruby
'puni_puni'.dasherize # => "puni-puni"
```

# [String#demodulize][demod]

Removes the module part from the constant expression in the string.

```ruby
'ActiveRecord::CoreExtensions::String::Inflections'.demodulize # => "Inflections"
'Inflections'.demodulize                                       # => "Inflections"
'::Inflections'.demodulize                                     # => "Inflections"
''.demodulize                                                  # => ''
```

# [String#deconstantize][deconstantize]

Removes the rightmost segment from the constant expression in the string.

```ruby
'Net::HTTP'.deconstantize   # => "Net"
'::Net::HTTP'.deconstantize # => "::Net"
'String'.deconstantize      # => ""
'::String'.deconstantize    # => ""
''.deconstantize            # => ""
```

# [String#parameterize][param]

Replaces special characters in a string so that it may be used as part of a 'pretty' URL.

```ruby
"Donald E. Knuth".parameterize # => "donald-e-knuth"
```

Preserve the case of the characters in a string with the `:preserve_case` argument.

```ruby
"Donald E. Knuth".parameterize(preserve_case: true) # => "Donald-E-Knuth"
```

A very common use-case for `parameterize` is to override the `to_param` method of an ActiveRecord model to support more descriptive url slugs.

```ruby
class Person < ActiveRecord::Base
  def to_param
    "#{id}-#{name.parameterize}"
  end
end

Person.find(1).to_param # => "1-donald-e-knuth"
```

# [String#tableize][tableize]

Creates the name of a table like Rails does for models to table names. Pluralizes the last word in the string.

```ruby
'RawScaledScorer'.tableize # => "raw_scaled_scorers"
'ham_and_egg'.tableize     # => "ham_and_eggs"
'fancyCategory'.tableize   # => "fancy_categories"
```

# [String#classify][classify]

Returns a class name string from a plural table name like Rails does for table names to models.

```ruby
'ham_and_eggs'.classify # => "HamAndEgg"
'posts'.classify        # => "Post"
```

# [String#humanize][humanize]

Capitalizes the first word, turns underscores into spaces, and strips a trailing `_id` if present.

```ruby
'employee_salary'.humanize              # => "Employee salary"
'author_id'.humanize                    # => "Author"
'author_id'.humanize(capitalize: false) # => "author"
'_id'.humanize                          # => "Id"
```

# [String#upcase_first][upcase_first]

Converts just the first character to uppercase.

```ruby
'what a Lovely Day'.upcase_first # => "What a Lovely Day"
'w'.upcase_first                 # => "W"
''.upcase_first                  # => ""
```

# [String#foreign_key][foreign_key]

Creates a foreign key name from a class name. Pass `false` param to disable adding `_` between name and `id`.

```ruby
'Message'.foreign_key        # => "message_id"
'Message'.foreign_key(false) # => "messageid"
'Admin::Post'.foreign_key    # => "post_id"
```

  [pluralize]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L31-L38
  [singularize]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L54-L56
  [constantize]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L65-L67
  [safe]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L76-L78
  [camelize]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L89-L97
  [titleize]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L107-L110
  [underscore]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L118-L120
  [dasherize]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L125-L127
  [demod]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L137-L139
  [deconstantize]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L150-L152
  [param]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L181-L187
  [tableize]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L195-L197
  [classify]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L205-L207
  [humanize]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L221-L223
  [upcase_first]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L230-L232
  [foreign_key]: https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L241-L243

