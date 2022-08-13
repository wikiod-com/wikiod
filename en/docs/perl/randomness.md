---
title: "Randomness"
slug: "randomness"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Documentation for perl's rand() function: http://perldoc.perl.org/functions/rand.html

## Accessing an array element at random
    my @letters = ( 'a' .. 'z' );                # English ascii-bet
    
    print $letters[ rand @letters ] for 1 .. 5;  # prints 5 letters at random
 
***How it works***

- [`rand EXPR`][1] expects a scalar value, so `@letters` is evaluated in scalar context
- An array in scalar context returns the number of elements it contains (26 in this case)
- `rand 26` returns a random fractional number in the interval `0 ≤ VALUE < 26`. (It can never be `26`)
- Array indices are always integers, so `$letters[rand @letters]` ≡ `$letters[int rand @letters]`
- Perl arrays are zero-indexed, so `$array[rand @array]` returns `$array[0]`, `$array[$#array]` or an element in between

---

***(The same principle applies to hashes)***

    my %colors = ( red   => 0xFF0000,
                   green => 0x00FF00,
                   blue  => 0x0000FF,
                 );

    print ( values %colors )[rand keys %colors];

  [1]: http://perldoc.perl.org/functions/rand.html

## Generate a random integer between 0 and 9
Cast your random floating-point number as an int.

Input:

    my $range = 10;

    # create random integer as low as 0 and as high as 9
    my $random = int(rand($range));   # max value is up to but not equal to $range
    
    print $random . "\n";
Output: 

A random integer, like...

    0

See also the [perldoc for rand][1].


  [1]: http://perldoc.perl.org/functions/rand.html

## Generate a random number between 0 and 100
Pass an upper limit as an argument to the rand() function.

Input:

    my $upper_limit = 100;
    my $random = rand($upper_limit);
    
    print $random . "\n";
Output: 

A random floating-point number, like...

    45.8733038119139

