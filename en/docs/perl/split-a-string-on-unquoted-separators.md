---
title: "Split a string on unquoted separators"
slug: "split-a-string-on-unquoted-separators"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## parse_line()
Using `parse_line()` of [Text::ParseWords](http://perldoc.perl.org/Text/ParseWords.html):

    use 5.010;
    use Text::ParseWords;
    
    my $line = q{"a quoted, comma", word1, word2};
    my @parsed = parse_line(',', 1, $line);
    say for @parsed;

Output:

    "a quoted, comma"
     word1
     word2

## Text::CSV or Text::CSV_XS
    use Text::CSV; # Can use Text::CSV which will switch to _XS if installed
    $sep_char = ",";
    my $csv = Text::CSV->new({sep_char => $sep_char});
    my $line = q{"a quoted, comma", word1, word2};
    $csv->parse($line);
    my @fields = $csv->fields();
    print join("\n", @fields)."\n";

Output:

    a quoted, comma
     word1
     word2

# NOTES

* By default, Text::CSV does not strip whitespace around separator character, the way `Text::ParseWords` does. However, adding `allow_whitespace=>1` to constructor attributes achieves that effect.

      my $csv = Text::CSV_XS->new({sep_char => $sep_char, allow_whitespace=>1});  

   Output:

      a quoted, comma
      word1
      word2

* The library supports escaping special characters (quotes, separators)

* The library supports configurable separator character, quote character, and escape character

Documentatoin: http://search.cpan.org/perldoc/Text::CSV   


