---
title: "BSDmacOS Sed vs. GNU Sed vs. the POSIX Sed specification"
slug: "bsdmacos-sed-vs-gnu-sed-vs-the-posix-sed-specification"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

To quote from @SnoringFrog's topic-creation request:

"One of the biggest gotchas using sed is scripts that fail (or succeed in an unexpected way) because they were written for one and not the other. Simple run-down of the more major differences would be good."

**_macOS_ uses the _BSD_ version of `sed`<sup>[1]</sup>, which differs in many respects from the _GNU_ `sed` version that comes with _Linux_ distros.**

Their **common denominator** is the functionality decreed by **POSIX**: see [the POSIX `sed` spec.](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sed.html)

The **most portable approach** is to **use POSIX features only**, which, however, **limits functionality**:

* Notably, POSIX specifies **support only for _basic_ regular expressions**, which have many limitations (e.g., no support for `|` (alternation) at all, no direct support for `+` and `?`) and different escaping requirements.

  * Caveat: **GNU `sed` (without `-r`), _does_ support `\|`, `\+` and `\?`, which is NOT POSIX-compliant; use `--posix` to disable** (see below).

* **To use POSIX features only**:

  * (both versions): use _only_ the `-n` and `-e` options (notably, do not use `-E` or `-r` to turn on support for _extended_ regular  expressions)
 
  * GNU `sed`: add option `--posix` to ensure POSIX-only functionality (you don't strictly need this, but without it you could end up inadvertently using non-POSIX features without noticing; _caveat_: `--posix` _itself_ is _not_ POSIX-compliant)
 
  * Using POSIX-only features means stricter formatting requirements (forgoing many conveniences available in GNU `sed`):
     * Control-character sequences such as `\n` and `\t` are generally NOT supported.
     * Labels and branching commands (e.g., `b`) _must_ be followed by an _actual_ newline or continuation via a separate `-e` option.
     * See below for details.

**However, _both_ versions implement _extensions_ to the POSIX standard:**

* **_what_ extensions they implement differs** (GNU `sed` implements more).
* even those **extensions they _both_ implement _partially differ in syntax_**.

**If you need to support BOTH platforms (discussion of differences):**

* **Incompatible** features:

  * Use of the **`-i` option _without_ an argument** (in-place updating without backup) is incompatible:
     * BSD `sed`: MUST use `-i ''`
     * GNU `sed`: MUST use just `-i` (equivalent: `-i''`) - using `-i ''` does NOT work.
 
  * **`-i` sensibly turns on _per-input-file_ line numbering** in _GNU_ `sed` and _recent_ versions of _BSD_ `sed` (e.g., on FreeBSD 10), but does **NOT on macOS as of 10.12**.  
Note that in the absence of `-i` *all* versions number lines *cumulatively* across input files.

  * If the **_last_ input line does _not_ have a trailing newline** (and is printed):
     * BSD `sed`: _always appends a newline_ on output, even if the input line doesn't end in one.
     * GNU `sed`: _preserves the trailing-newline status_, i.e., it appends a newline only if the input line ended in one.

<!-- -->
 
* **Common** features:

  * If you restrict your `sed` scripts to what BSD `sed` supports, they will generally work in GNU `sed` too - with the notable exception of using platform-specific *extended* regex features with `-E`. Obviously, you'll also forgo extensions that are specific to the GNU version. See next section.

----------


**Guidelines for *cross-platform support* (OS X/BSD, Linux), driven by the stricter requirements of the BSD version**:

<sup>Note that that the shorthands _macOS_ and _Linux_ are occasionally used below to refer to the BSD and GNU versions of `sed`, respectively, because they are the _stock_ versions on each platform. However, it is possible to install GNU `sed` on macOS, for instance, using [Homebrew](http://brew.sh/) with `brew install gnu-sed`.
</sup>

**Note**: **_Except when the `-r` and `-E` flags are used_** (_extended_ regexes), the instructions below amount to writing **POSIX-compliant** `sed` scripts.

* For POSIX compliance, you must restrict yourself to [POSIX BREs (*basic* regular expressions)](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html#tag_09_03), which are, unfortunately, as the name suggests, quite basic.  
**Caveat**: do not assume that `\|`, `\+` and `\?` are supported: While GNU `sed` supports them (unless `--posix` is used), BSD `sed` does not - these features are _not_ POSIX-compliant.  
While **`\+` and `\?` _can_ be emulated** in POSIX-compliant fashion :  
 `\{1,\}` for `\+`,  
 `\{0,1\}` for `\?`,  
 **`\|` (alternation) _cannot_**, unfortunately.

* For more powerful regular expressions, **use `-E`** (rather than `-r`) to support **EREs (_extended_ regular expressions)** (GNU `sed` doesn't document `-E`, but it does work there as an alias of `-r`; _newer_ version of BSD `sed`, such as on FreeBSD 10, now also support `-r`, but the macOS version as of 10.12 does _not_).  
**Caveat**: Even though use of `-r` / `-E` means that your command is by definition _not_ POSIX-compliant, you must still **restrict yourself to [POSIX EREs (extended regular expressions)](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html#tag_09_04)**. Sadly, this means that you won't be able to use several useful constructs, notably:
   * word-boundary assertions, because they're _platform-specific_ (e.g., `\<` on Linux, `[[:<]]` on OS X).
   * back-references _inside regular expressions_ (as opposed to the "back-references" to capture-group matches in the replacement string of `s` function calls), because BSD `sed` doesn't support them in _extended_ regexes (but, curiously, does so in _basic_ ones, where they are POSIX-mandated).

* **Control-character escape sequences such as `\n` and `\t`:**

  * In **regexes** (both in patterns for line selection and the first argument to the `s` function), assume that only `\n` is recognized as an escape sequence (rarely used, since the pattern space is usually a _single_ line (without terminating `\n`), but not inside a _character class_, so that, e.g., `[^\n]` doesn't work; (if your input contains no control chars. other than `\t`, you can emulate `[^\n]` with `[[:print:][:blank:]]`; otherwise, splice control chars. in as _literals_<sup>[2]</sup>)  - **generally, include control characters as _literals_, either via spliced-in *ANSI C-quoted strings* (e.g., `$'\t'`) in shells that support it (`bash, `ksh, `zsh`), or via *command substitutions using `printf`* (e.g., `"$(printf '\t')"`)**.
     * Linux only:  
     `sed 's/\t/-/' <<<$'a\tb' # -> 'a-b'`
     * OSX _and_ Linux:  
     `sed 's/'$'\t''/-/' <<<$'a\tb'            # ANSI C-quoted string`  
     `sed 's/'"$(printf '\t')"'/-/' <<<$'a\tb' # command subst. with printf`

   * In **replacement strings** used with the `s` command, **assume that NO control-character escape sequences are supported**, so, again, include control chars. as _literals_, as above.
     * Linux only:  
   `sed 's/-/\t/' <<<$'a-b' # -> 'a<tab>b'`
     * macOS _and_ Linux:  
   `sed 's/-/'$'\t''/' <<<'a-b'`  
   `sed 's/-/'"$(printf '\t')"'/' <<<'a-b'`

   * Ditto for the **text arguments** to the **`i` and `a` functions**: **do not use control-character sequences** - see below.

* **Labels and branching**: labels as well as the label-name _argument_ to the `b` and `t` functions **must be followed by either by a _literal_ newline or a spliced-in `$'\n'`**. Alternatively, use multiple `-e` options and terminate each right after the label name.
  * Linux only:  
   `sed -n '/a/ bLBL; d; :LBL p' <<<$'a\nb' # -> 'a'`
  * macOS _and_ Linux:
     * EITHER (actual newlines):  
`sed -n '/a/ bLBL
  d; :LBL
  p' <<<$'a\nb'`
     * OR (spliced-in `$\n` instances):  
   `sed -n '/a/ bLBL'$'\n''d; :LBL'$'\n''p' <<<$'a\nb'`
     * OR (multiple `-e` options):  
   `sed -n -e '/a/ bLBL' -e 'd; :LBL' -e 'p' <<<$'a\nb'`

* Functions **`i` and `a` for inserting/appending text**: **follow the function name by `\`, followed either by a _literal_ newline or a spliced-in `$'\n'`** before specifying the text argument.

  * Linux only:  
    `sed '1 i new first line' <<<$'a\nb' # -> 'new first line<nl>a<nl>b'`
  * OSX _and_ Linux:  
    `sed -e '1 i\'$'\n''new first line' <<<$'a\nb'`
  * Note:
     * Without `-e`, the text argument is inexplicably not newline-terminated on output on macOS (bug?).
     * **Do not use control-character escapes** such as `\n` and `\t` in the text argument, as they're only supported on Linux.
     * If the text argument therefore has actual interior newlines, `\`-escape them.
     * If you want to place additional commands after the text argument, you must terminate it with an (unescaped) newline (whether literal or spliced in), or continue with a separate `-e` option (this is a general requirement that applies to all versions).

* Inside **function _lists_** (multiple function calls enclosed in `{...}`), **be sure to also terminate the _last_ function, before the closing `}`, with `;`**.
  * Linux only:
    * `sed -n '1 {p;q}' <<<$'a\nb' # -> 'a'`
  * macOS _and_ Linux:
    * `sed -n '1 {p;q;}' <<<$'a\nb'`

----------

**GNU `sed`-specific features** missing from BSD `sed` altogether:

GNU features you'll miss out on if you need to support both platforms:

* Various **regex-matching and substitution options** (both in patterns for line selection and the first argument to the `s` function):

  * **The `I` option for case-INsensitive regex matching (incredibly, BSD `sed` doesn't support this at all).**
  * The `M` option for multi-line matching (where `^` / `$` match the start / end of _each line_)
  * For additional options that are specific to the `s` function, see https://www.gnu.org/software/sed/manual/sed.html#The-_0022s_0022-Command

* **Escape sequences**

  * Substitution-related escape sequences such as `\u` in the replacement argument of the `s///` function that allow _substring manipulation_, within limits; e.g., `sed 's/^./\u&/' <<<'dog' # -> 'Dog'` - see http://www.gnu.org/software/sed/manual/sed.html#The-_0022s_0022-Command

  * Control-character escape sequences: in addition to `\n`, `\t`, ..., codepoint-based escapes; for instance, all of the following escapes (hex., octal, decimal) represent a single quote (`'`): `\x27`, `\o047`, `\d039` - see https://www.gnu.org/software/sed/manual/sed.html#Escapes

* **Address extensions**, such as `first~step` to match every step-th line, `addr, +N` to match N lines following `addr`, ... - see http://www.gnu.org/software/sed/manual/sed.html#Addresses

----------

<sup>[1] The macOS `sed` version is _older_ than the version on other BSD-like systems such as FreeBSD and PC-BSD. Unfortunately, this means that you cannot assume that features that work in FreeBSD, for instance, will work [the same] on macOS.</sup>

<sup>[2] The ANSI C-quoted string `$'\001\002\003\004\005\006\007\010\011\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037\177'` contains all ASCII control characters except `\n` (and NUL), so you can use it in combination with `[:print:]` for a pretty robust emulation of `[^\n]`:  
`'[[:print:]'$'\001\002\003\004\005\006\007\010\011\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037\177'']`</sup>

## Replace all newlines with tabs
<sup>Note: For brevity, the commands use [here-strings (`<<<`)](http://mywiki.wooledge.org/HereDocument?action=show&redirect=HereString) and [ANSI C-quoted strings (`$'...'`)](http://www.gnu.org/software/bash/manual/bash.html#ANSI_002dC-Quoting). Both these shell features work in `bash`, `ksh`, and `zsh`.</sup>


<!-- language: bash -->

    # GNU Sed
    $ sed ':a;$!{N;ba}; s/\n/\t/g' <<<$'line_1\nline_2\nline_3'
    line_1 line_2 line_3

    # BSD Sed equivalent (multi-line form)
    sed <<<$'line_1\nline_2\nline_3' '
    :a
    $!{N;ba
    }; s/\n/'$'\t''/g'
  
    # BSD Sed equivalent (single-line form, via separate -e options)
    sed -e ':a' -e '$!{N;ba' -e '}; s/\n/'$'\t''/g' <<<$'line 1\nline 2\nline 3'

BSD Sed notes:

* Note the need to terminate labels (`:a`) and branching commands (`ba`) either with actual newlines or with separate `-e` options.

* Since control-character escape sequences such as `\t` aren't supported in the replacement string, an ANSI C-quoted tab _literal_ is spliced into the replacement string.  
(In the _regex_ part, BSD Sed _only_ recognizes `\n` as an escape sequence).



## Append literal text to a line with function 'a'
<sup>Note: For brevity, the commands use [here-strings (`<<<`)](http://mywiki.wooledge.org/HereDocument?action=show&redirect=HereString) and [ANSI C-quoted strings (`$'...'`)](http://www.gnu.org/software/bash/manual/bash.html#ANSI_002dC-Quoting). Both these shell features work in `bash`, `ksh`, and `zsh`.</sup>

<!-- language: bash -->

     # GNU Sed
     $ sed '1 a appended text' <<<'line 1'
     line 1
     appended text

     # BSD Sed (multi-line form)
     sed '1 a\
     appended text' <<<'line 1'

     # BSD Sed (single-line form via a Bash/Ksh/Zsh ANSI C-quoted string)
     sed $'1 a\\\nappended text' <<<'line 1'

Note how BSD Seed requires a `\` followed by an _actual newline_ to pass the text to append.  
The same applies to the related `i` (insert) and `c` (delete and insert) functions.



