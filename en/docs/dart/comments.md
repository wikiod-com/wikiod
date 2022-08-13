---
title: "Comments"
slug: "comments"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - // Single-line comment
 - /* Multi-line/In-line comment */
 - /// Dartdoc comment

It is good practice to add comments to your code to explain why something is done or to explain what something does. This helps any future readers of your code to more easily understand your code.

Related topic(s) not on StackOverflow:
- [Effective Dart: Documentation](https://www.dartlang.org/guides/language/effective-dart/documentation)


## Documentation using Dartdoc
Using a doc comment instead of a regular comment enables [dartdoc][1] to find it and generate documentation for it.

    /// The number of characters in this chunk when unsplit.
    int get length => ...

You are allowed to use most [markdown][2] formatting in your doc comments and dartdoc will process it accordingly using the [markdown package][3].

    /// This is a paragraph of regular text.
    ///
    /// This sentence has *two* _emphasized_ words (i.e. italics) and **two**
    /// __strong__ ones (bold).
    ///
    /// A blank line creates another separate paragraph. It has some `inline code`
    /// delimited using backticks.
    ///
    /// * Unordered lists.
    /// * Look like ASCII bullet lists.
    /// * You can also use `-` or `+`.
    ///
    /// Links can be:
    ///
    /// * http://www.just-a-bare-url.com
    /// * [with the URL inline](http://google.com)
    /// * [or separated out][ref link]
    ///
    /// [ref link]: http://google.com
    ///
    /// # A Header
    ///
    /// ## A subheader

  [1]: https://github.com/dart-lang/dartdoc
  [2]: https://daringfireball.net/projects/markdown/
  [3]: https://pub.dartlang.org/packages/markdown


## End of Line Comment
Everything to the right of `//` in the same line is commented.

    int i = 0; // Commented out text

## Multi-Line Comment
Everything between `/*` and `*/` is commented.

    void main() {
      for (int i = 0; i < 5; i++) {
        /* This is commented, and
        will not affect code */
        print('hello ${i + 1}');
      }   
    }

