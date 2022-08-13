---
title: "GNU C Extensions"
slug: "gnu-c-extensions"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The GNU C compiler comes with some cool features that are not specified by the C standards. These extensions are heavily used in system software and are a great tool for performance optimization.

## Attribute packed
*packed* is a variable attribute that is used with structures and unions in order to minimize the memory requirements.

    #include <stdio.h>
    struct foo {
        int a;
        char c;
    };

    struct __attribute__((__packed__))foo_packed {
        int a;
        char c;
    };

    int main()
    {
        printf("Size of foo: %d\n", sizeof(struct foo));
        printf("Size of packed foo: %d\n", sizeof(struct foo_packed));
        return 0;
    }

On my 64 bit Linux,

 - Size of struct foo = 8 bytes
 - Size of struct foo_packed = 5 bytes

*packed* attribute curbs the [structure padding][1] that the compiler performs to maintain memory alignment.

 


  [1]: http://www.geeksforgeeks.org/structure-member-alignment-padding-and-data-packing/

