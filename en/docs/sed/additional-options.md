---
title: "Additional Options"
slug: "additional-options"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Syntax
 - -a - (BSD sed) Create / Truncate all files written to before processing
 - -E | -r - Use Extended Regular Expressions
 - -i | -I - Refer to the topic on [In-Place Editing][inplace]
 - -l - (BSD sed) Use line-buffered output
 - -l length - (GNU sed) Specify the length for `l` command line-wrapping
 - -s - (GNU sed) Treat files as separate streams
 - -u - Do not buffer the output
 - -z - (GNU sed) Use the NUL character to separate records
 - --quiet | --silent - (GNU sed) Synonyms for `-n`
 - --expression=command - (GNU sed) Synonym for `-e`
 - --file=command_file - (GNU sed) Synonym for `-f`
 - --follow-symlinks - (GNU sed) Follow symlinks
 - --in-place[=extension] - (GNU sed) Synonym for `-i`
 - --line-length=length - (GNU sed) Synonym for `-l`
 - --separate - (GNU sed) Synonym for `-s`
 - --unbuffered - (GNU sed) Synonym for `-u`
 - --null-data - (GNU sed) Synonym for `-z`
 - --help - (GNU sed) Print usage
 - --version - (GNU sed) Print version

[inplace]: https://www.wikiod.com/sed/in-place-editing

The `-E` option is to be standardized in the next major version, see [the relevant issue](http://austingroupbugs.net/view.php?id=528).



## Delay Creation/Truncation of Files
Files written to with the `w` command are created/truncated before any commands are run.

    $ sed 'w created-file' < /dev/null && ls created-file && rm created-file
    created-file

From the standard:

> Each wfile shall be created before processing begins. Implementations shall support at least ten wfile arguments in the script; the actual number (greater than or equal to 10) that is supported by the implementation is unspecified. The use of the wfile parameter shall cause that file to be initially created, if it does not exist, or shall replace the contents of an existing file.

---

BSD `sed` provides the `-a` option to delay creating/truncating files until they are written to with the `w` command.

    $ if sed -a 'w created-file' < /dev/null && [ ! -e created-file ]; then
    >       echo The file was not created
    > fi
    The file was not created


## 'l' Line-Wrapping
The length of line-wrapping when using the `l` command is implementation defined.

From the standard:

> Long lines shall be folded, with the point of folding indicated by writing a <backslash> followed by a <newline>; the length at which folding occurs is unspecified, but should be appropriate for the output device.

---

GNU `sed` provides the `-l` option to specify the length at which to split long lines when printing with the `l` command, defaulting to seventy characters.

    $ yes | head -c100 | tr '\n' ' ' | sed -n l | head -n1 | wc -c
          71
    $ yes | head -c100 | tr '\n' ' ' | sed -nl50 l | head -n1 | wc -c
          51

---

BSD `sed` splits long lines at the number provided by the environment variable `COLUMNS`, if `COLUMNS` is not provided then it splits at the terminal width, and if `COULMNS` is not provided and the output is not a terminal then it defaults to sixty characters.

    $ yes | head -c100 | tr '\n' ' ' | sed -n l | head -n1 | wc -c
          61
    $ yes | head -c100 | tr '\n' ' ' | COLUMNS=50 sed -n l | head -n1 | wc -c
          51
    $ yes | head -c100 | tr '\n' ' ' | sed -n l | head -n1
    y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y\
     y y y y y y y y y y $


