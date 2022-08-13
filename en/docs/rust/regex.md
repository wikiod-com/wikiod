---
title: "Regex"
slug: "regex"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

Rust's standard library does not contain any regex parser/matcher, but the [`regex`](https://doc.rust-lang.org/regex/regex/index.html) crate (which is in the [rust-lang-nursery](https://github.com/rust-lang-nursery) and hence semi-official) provides a regex parser. This section of the documentation will provide an overview of how to use the `regex` crate in common situations, along with installation instructions and any other useful remarks which are needed while using the crate.

## Simple match and search
Regular expression support for tust is provided by the `regex` crate, add it to your `Cargo.toml`:
```
[dependencies]
regex = "0.1"
```
The main interface of the `regex` crate is `regex::Regex`:
```
extern crate regex;
use regex::Regex;

fn main() {
    //"r" stands for "raw" strings, you probably
    // need them because rustc checks escape sequences,
    // although you can always use "\\" withour "r"
    let num_regex = Regex::new(r"\d+").unwrap();
    // is_match checks if string matches the pattern
    assert!(num_regex.is_match("some string with number 1"));

    let example_string = "some 123 numbers";
    // Regex::find searches for pattern and returns Option<(usize,usize)>,
    // which is either indexes of first and last bytes of match
    // or "None" if nothing matched
    match num_regex.find(example_string) {
        // Get the match slice from string, prints "123"
        Some(x) => println!("{}", &example_string[x.0 .. x.1]),
        None    => unreachable!()
    }
}
```

## Capture groups
```
extern crate regex;
use regex::Regex;

fn main() {
    let rg = Regex::new(r"was (\d+)").unwrap();
    // Regex::captures returns Option<Captures>,
    // first element is the full match and others
    // are capture groups
    match rg.captures("The year was 2016") {
        // Access captures groups via Captures::at
        // Prints Some("2016")
        Some(x) => println!("{:?}", x.at(1)),
        None    => unreachable!()
    }

    // Regex::captures also supports named capture groups
    let rg_w_named = Regex::new(r"was (?P<year>\d+)").unwrap();
    match rg_w_named.captures("The year was 2016") {
        // Named capures groups are accessed via Captures::name
        // Prints Some("2016")
        Some(x) => println!("{:?}", x.name("year")),
        None    => unreachable!()
    }

}
```

## Replacing
```
extern crate regex;
use regex::Regex;

fn main() {
    let rg = Regex::new(r"(\d+)").unwrap();

    // Regex::replace replaces first match 
    // from it's first argument with the second argument
    // => Some string with numbers (not really)
    rg.replace("Some string with numbers 123", "(not really)");

    // Capture groups can be accesed via $number
    // => Some string with numbers (which are 123)
    rg.replace("Some string with numbers 123", "(which are $1)");

    let rg = Regex::new(r"(?P<num>\d+)").unwrap();
    
    // Named capture groups can be accesed via $name
    // => Some string with numbers (which are 123)
    rg.replace("Some string with numbers 123", "(which are $num)");

    // Regex::replace_all replaces all the matches, not only the first
    // => Some string with numbers (not really) (not really)
    rg.replace_all("Some string with numbers 123 321", "(not really)");
}
```

