---
title: "Ansi- and Wide-character functions"
slug: "ansi--and-wide-character-functions"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Introduction
The Windows API documentation for functions taking one or more *string* as argument will usually look like this:

    BOOL WINAPI CopyFile(
      _In_ LPCTSTR lpExistingFileName,
      _In_ LPCTSTR lpNewFileName,
      _In_ BOOL    bFailIfExists
    );

The datatype for the two string parameters is made of several parts:

 - LP  = Long pointer
 - C   = const
 - T   = TCHAR
 - STR = string

Now what does `TCHAR` mean? This depends on platform chosen for the compilation of program.

`CopyFile` itself is just a macro, defined something like this:

    #ifdef UNICODE
    #define CopyFile CopyFileW
    #else
    #define CopyFile CopyFileA
    #endif

So there are actually two `CopyFile` functions and depending on compiler flags, the `CopyFile` macro will resolve to one or the other.

There core token, `TCHAR` is defined as:

    #ifdef _UNICODE
    typedef wchar_t TCHAR;
    #else
    typedef char TCHAR;
    #endif

So again, depending on the compile flags, TCHAR is a "narrow" or a "wide" (2 bytes) character.

So when UNICODE is defined, `CopyFile` is defined to be `CopyFileW`, which will use 2-byte character arrays as their parameter, which are expected to be UTF-16 encoded.

If UNICODE isn't defined, `CopyFile` is defined to be `CopyFileA` which uses single-byte character arrays which are expected to be encoded in the default ANSI encoding of the current user.

There are two similar macros:  `UNICODE` makes the Windows APIs expect wide strings and `_UNICODE` (with a leading underscore) which enables similar features in the C runtime library.

These defines allow us to write code that compiles in both ANSI and in Unicode-mode.

It is important to know that the ANSI encoding may be a single-byte encoding (i.e. latin-1) a multi-byte encoding (i.e. shift jis), although utf-8 is, unfortunately, not well supported.

This means that neither the ANSI, nor the Wide-character variant of these functions can be assumed to work with fixed width encodings.

