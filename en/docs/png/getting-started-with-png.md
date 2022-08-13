---
title: "Getting started with png"
slug: "getting-started-with-png"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## libpng version 1.6.21
Libpng was written as a companion to PNG specification as a way of reducing the amount of time and effort it takes to support the PNG file format in application programs.

Libpng was designed to handle multiple sessions at one time, to be easily modifiable, to be portable to the vast majority of machines (ANSI, K&R, 16-, 32-, and 64-bit) available, and to be easy to use. The ultimate goal is to promote acceptance of PNG format in whatever way possible. While there is still work to be done (see the TODO file), libpng should cover the majority of user needs.

**User limits:**

PNG specification allows the width and height of an image to be as large as 2^31-1 (0x7fffffff), or about 2.147 billion rows and columns. For safety libpng imposes a default limit of 1 million rows and columns. Larger images will be rejected immediately with a `png_error()` call. Libpng may reject very wide images because of potential buffer overflow conditions, but you can set your own limits with:

    png_set_user_limits(png_ptr, width_max, height_max);


Put this statement after creating the PNG structure and before calling `png_read_info()`, `png_read_png()`, or `png_process_data()`.

When writing a PNG datastream, put this statement before calling `png_write_info()` or `png_write_png()`.

To retrieve the limits that are being applied, use

    width_max = png_get_user_width_max(png_ptr);
    height_max = png_get_user_height_max(png_ptr);

The PNG specification sets no limit on the number of ancillary chunks allowed in a PNG datastream. By default libpng imposes a limit of a total of 1000 sPLT, tEXt, iTXt, zTXt, and unknown chunks to be stored. IF both `info_ptr` and `end_info_ptr` are set, the limit applies separately to each. Change the limit on the total number of such chunks to be stored with:

    png_set_chunk_cache_max(png_ptr, user_chunk_cache_max);

where `0x7fffffffL` means unlimited. You can retrieve this limit with:

    chunk_cache_max = png_get_chunk_cache_max(png_ptr);

Libpng imposes a limit of 8 Megabytes (8,000,000 bytes) on the amount of memory that a compressed chunk other than IDAT can occupy, when decompressed. You can change this limit with:

    png_set_chunk_malloc_max(png_ptr, user_chunk_malloc_max);

and you can retrieve the limit with:

    chunk_malloc_max = png_get_chunk_malloc_max(png_ptr);

Any chunks that would cause either of these limits to be exceeded will be ignored.

**Detecting libpng version:**

The `png_get_io_ptr()` function has been present since libpng-0.88, has never
changed, and is unaffected by conditional compilation macros.  It is the
best choice for use in configure scripts for detecting the presence of any
libpng version since 0.88.  In an autoconf "configure.in" you could use

    AC_CHECK_LIB(png, png_get_io_ptr, ...



