---
title: "GNU Pattern Rules"
slug: "gnu-pattern-rules"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Targets matching multiple Pattern Rules
If a target matches multiple pattern rules, make will use the one whose prerequisites exist or can be built. For example:

    %.o: %.c
        $(CC) $(CFLAGS) -c $< -o $@
    %.o: %.s
        $(AS) $(ASFLAGS) $< -o $@

Will compile `foo.c` to `foo.o` or assemble `foo.s` to `foo.o`, depending on which one of `foo.c` or `foo.s` exists.

If multiple rules have prerequisites that exist or can be built, make will use the rule that matches to the shortest stem. For example:

    f%r:
        @echo Stem is: $*
    fo%r:
        @echo Stem is: $*

Will use the second rule to make the target `foo.bar`, echoing `Stem is: o.ba`.

If multiple rules match to the shortest stem, make will use the first one in the Makefile.

## Basic Pattern Rule
A pattern rule is indicated by a single `%` character in the target. The `%` matches a non-empty string called the **stem**. The stem is then substituted for every `%` that appears in the prerequisite list.

For example, this rule:

    %.o: %.c
        $(CC) $(CFLAGS) -c $< -o $@

Will match any target ending in `.o`. If the target was `foo.o`, the stem would be `foo` and it would compile `foo.c` to `foo.o`. Targets and prerequisites can be accessed using automatic variables.

## Directories in Pattern Rules
If the target pattern doesn't contain slashes, make will remove the directory part from the target it's trying to build before matching. The directory will then be put in front of the stem. When the stem is used to build the target name and prerequisites, the directory part is stripped from it, the stem is substituted in place of the `%` and finally the directory is put in front of the string. For example:

    foo%.o: %.c
        $(CC) $(CFLAGS) -c $< -o $@

Will match `lib/foobar.o`, with:

 - Stem (`$*`): `lib/bar`
 - Target name (`$@`): `lib/foobar.o`
 - Prerequisites (`$<`, `$^`): `lib/foobar.c`

In this example, a `lib/foo%.o` rule would take precedence over the `foo%.o` rule because it matches to a shorter stem.

## Pattern Rules with multiple targets
Pattern rules can have multiple targets but, unlike normal rules, the recipe is responsible for making all the targets. For example:

    debug/%.o release/%.o: %.c
        $(CC) $(CFLAGS_DEBUG) -c $< -o debug/$*.o
        $(CC) $(CFLAGS_RELEASE) -c $< -o release/$*.o

Is a valid rule, which will build both debug and release objects when one of them has to be built. If we wrote something like:

    debug/%.o release/%.o: %.c
        $(CC) $(CFLAGS) -c $< -o $@

It would work when only one of `debug/*.o` or `release/*.o` is built, but it would only build the first target (and consider the second one to be up-to-date) when both have to be built.

