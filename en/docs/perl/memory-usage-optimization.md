---
title: "Memory usage optimization"
slug: "memory-usage-optimization"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Reading files: foreach vs. while
When reading a potentially large file, a `while` loop has a significant memory advantage over `foreach`. The following will read the file record by record (by default, "record" means "a line", as specified by `$/`), assigning each one to `$_` as it is read:

    while(<$fh>) {
        print;
    }

The *diamond operator* does some magic here to make sure the loop only terminates at end-of-file and not e.g. on lines that contain only a "0" character.

The following loop seems to work just the same, however it evaluates the diamond operator in list context, causing the entire file to be read in one go:

    foreach(<$fh>) {
        print;
    }

If you are operating on one record at a time anyway, this can result in a huge waste of memory and should thus be avoided.

## Processing long lists
If you have a list in memory already, the straightforward and usually sufficient way to process it is a simple `foreach` loop:

    foreach my $item (@items) {
        ...
    }

This is fine e.g. for the common case of doing some processing on `$item` and then writing it out to a file without keeping the data around. However, if you build up some other data structure from the items, a `while` loop is more memory efficient:

    my @result;
    while(@items) {
        my $item = shift @items;
        push @result, process_item($item);
    }

Unless a reference to `$item` directly ends up in your result list, items you shifted off the `@items` array can be freed and the memory reused by the interpreter when you enter the next loop iteration.

