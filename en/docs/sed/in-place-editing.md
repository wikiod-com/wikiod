---
title: "In-Place Editing"
slug: "in-place-editing"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Syntax
- sed -I _extension_ - FreeBSD sed (continuous line-counter)
- sed -I[_extension_] - NetBSD and Illumos sed (continuous line-counter)
- sed -i _extension_ - FreeBSD sed
- sed -i[_extension_] - NetBSD, OpenBSD, Illumos, BusyBox and GNU sed
- sed --in-place[=_extension_] - Illumos, BusyBox, and GNU sed

## Parameters
Parameter   | Details
------------|--------
`extension` | Save a backup file with the specified extension, or no backup file when `extension` is a zero-length string.

In-place editing is a common but non-standard extension present in the majority of recent systems.

From a [BSD `sed` manual][obsed]  

(a section like this appears in all current BSD `sed` manuals, and those of their derivatives)

> It is not recommended to give a zero length extension when in place editing files,
> as it risks corruption or partial content in situations where disk space is
> exhausted, etc.

# Don't forget the mighty `ed`

There is definitely a use for `sed` and for in-place editing features of `sed`, but when the UNIX standard is extended, we should always ask why the old UNIX standard did not include that feature. Though UNIX is not perfect, the orthogonality and completeness of the tools has been developed to be quite near to perfection, at least for purposes that where visible around 1970: _Text editing and automated text editing was surely visible around that time._

Actually, the idea of `sed` is not to edit a *file* in place, but to edit a *stream*. That's why the name `sed` is a short form of _stream editor_. Take away the `s`, and you get the tool that was actually designed for *file* editing: `ed`:

    printf 'g/what to replace/s//with what to replace/g\nw\nq\n' | ed file

or `cat file_edit_commands | ed file`.

[obsed]: http://man.openbsd.org/OpenBSD-current/man1/sed.1
[illumos]: http://src.illumos.org/source/diff/illumos-gate/usr/src/cmd/sed/main.c?r2=%2Fillumos-gate%2Fusr%2Fsrc%2Fcmd%2Fsed%2Fmain.c%40e50226eccc6dfcba3cc6f0df38438900e3df225c&r1=%2Fillumos-gate%2Fusr%2Fsrc%2Fcmd%2Fsed%2Fmain.c%40d15978eab6c23a98f0a5474466d5fe9b1be3ca9b

## Portable Use
In-place editing, while common, is a non-standard feature.  A viable alternative would be to use an intermediate file to either store the original, or the output.

    sed 'sed commands' > file.out && mv file.out file
    # or
    mv file file.orig && sed 'sed commands' file.orig > file

To use the `-i` option with both the GNU and FreeBSD syntax an extension must be specified and appended to the `-i` option.  The following will be accepted by both, and produce two files, the original version at `file.orig` and the edited version at `file`:

    sed -i.orig 'sed commands' file

See a basic example given a file `file`:

    $ cat file
    one
    two
    three
    $ sed -i.orig 's/one/XX/' file
    $ cat file                       # the original file has changed its content
    XX
    two
    three
    $ cat file.orig                  # the original content is now in file.orig
    one
    two
    three

A more complex example, replacing each line with line number:

    $ printf 'one\ntwo\n' | tee file1 | tr a-z A-Z > file2
    $ sed -ni.orig = file1 file2
    $ cat file1.orig file2.orig
    one
    two
    ONE
    TWO
    $ cat file1 file2
    1
    2
    1
    2

Why a backup file is required
-

In order to use in-place editing without a backup file, `-i` must be given a zero-length argument and FreeBSD `sed` requires an argument to `-i`, either appended or separate, while the GNU optional argument extension requires the argument be appended to `-i`.  Both support appending the argument to `-i`, but without it being required `-i'' command` is indistinguishable from `-i extension`, and so a zero-length argument can not be appended to `-i`.

## Replacing strings in a file in-place
    sed -i s/"what to replace"/"with what to replace"/g $file

We use `-i` to select in-place editing on the `$file` file.
In some systems it is required to add suffix after `-i` flag which will be used to create backup of original file. You can add empty string like `-i ''` to omit the backup creation. Look at *Remarks* in this topic about `-i` option.

The `g` terminator means do a global find/replace in each line.

    $ cat example 
    one
    two
    three
    total
    $ sed -i s/"t"/"g"/g example 
    $ cat example 
    one
    gwo
    ghree
    gogal

## In-place editing without specifying a backup file overrides read-only permissions
`sed -i -e cmd file` will modify `file` even if its permissions are set to read-only.

This command behaves similarly to

`sed -e cmd file > tmp; mv -f tmp file`

rather than

`sed -e cmd file > tmp; cat tmp > file; rm tmp`

The following example uses gnu `sed`:

    $ echo 'Extremely important data' > input
    $ chmod 400 input  # Protect that data by removing write access
    $ echo 'data destroyed' > input
    -bash: input: Permission denied
    $ cat input  
    Extremely important data (#phew! Data is intact)
    $ sed -i s/important/destroyed/ input
    $ cat input
    Extremely destroyed data (#see, data changed)

This can be mitigated by creating a backup by specifying a `SUFFIX` with the `i` option:

    $ sed -i.bak s/important/destroyed/ input
    $ cat input
    Extremely destroyed data
    $ cat input.bak
    Extremely important data

