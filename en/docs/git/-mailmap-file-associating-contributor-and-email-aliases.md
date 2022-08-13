---
title: ".mailmap file Associating contributor and email aliases"
slug: "mailmap-file-associating-contributor-and-email-aliases"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
* \# Only replace email addresses  
  \<primary@example.org> \<alias@example.org>
* \# Replace name by email address  
  Contributor \<primary@example.org>
* \# Merge multiple aliases under one name and email  
  \# Note this will not associate 'Other \<alias2@example.org>'.  
  Contributor \<primary@example.org> \<alias1@example.org> Contributor \<alias2@example.org>


A `.mailmap` file may be created in any text editor and is just a plain text file containing optional contributor names, primary email addresses, and their aliases. it has to be placed in the project's root, next to the `.git` directory.

Keep in mind that this just modifies the visual output of commands like `git shortlog` or `git log --use-mailmap`. This will **not** rewrite commit history or prevent commits with varying names and/or email addresses.

To prevent commits based on information such as email addresses, you should use [git hooks][1] instead.

[1]:https://www.wikiod.com/git/hooks

## Merge contributers by aliases to show commit count in shortlog.
When contributors add to a project from different machines or operating systems, it may happen that they use different email addresses or names for this, which will fragment contributor lists and statistics.

Running `git shortlog -sn` to get a list of contributors and the number of commits by them could result in the following output:

    Patrick Rothfuss 871
    Elizabeth Moon 762
    E. Moon 184
    Rothfuss, Patrick 90

This fragmentation/disassociation may be adjusted by providing a plain text file `.mailmap`, containing email mappings.

All names and email addresses listed in one line will be associated to the first named entity respectively.

For the example above, a mapping could look like this:

    Patrick Rothfuss <fussy@kingkiller.com> Rothfuss, Patrick <fussy@kingkiller.com>
    Elizabeth Moon <emoon@marines.mil> E. Moon <emoon@scifi.org>

Once this file exists in the project's root, running `git shortlog -sn` again will result in a condensed list:

    Patrick Rothfuss 961
    Elizabeth Moon 946

