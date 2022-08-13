---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Concatenation and interpolation
You can use the plus (`+`) operator to concatenate strings:

    'Dart ' + 'is ' + 'fun!'; // 'Dart is fun!'

You can also use adjacent string literals for concatenation:

    'Dart ' 'is ' 'fun!';    // 'Dart is fun!'

You can use `${}` to interpolate the value of Dart expressions within strings. The curly braces can be omitted when evaluating identifiers:

    var text = 'dartlang';
    '$text has ${text.length} letters'; // 'dartlang has 8 letters'

## Valid strings
A string can be either single or multiline. Single line strings are written using matching single or double quotes, and multiline strings are written using triple quotes. The following are all valid Dart strings:

    'Single quotes';
    "Double quotes";
    'Double quotes in "single" quotes';
    "Single quotes in 'double' quotes";
    
    '''A
    multiline
    string''';
    
    """
    Another
    multiline
    string""";

## Building from parts
Programmatically generating a String is best accomplished with a [StringBuffer][1]. A StringBuffer doesn't generate a new String object until `toString()` is called.

    var sb = new StringBuffer();
    
    sb.write("Use a StringBuffer");
    sb.writeAll(["for ", "efficient ", "string ", "creation "]);
    sb.write("if you are ")
    sb.write("building lots of strings");
    
    // or you can use method cascades:
    
    sb
      ..write("Use a StringBuffer")
      ..writeAll(["for ", "efficient ", "string ", "creation "])
      ..write("if you are ")
      ..write("building lots of strings");
    
    var fullString = sb.toString();
    
    print(fullString); 
    // Use a StringBufferfor efficient string creation if you are building lots of strings

    sb.clear(); // all gone!


  [1]: http://api.dartlang.org/docs/releases/latest/dart_core/StringBuffer.html

