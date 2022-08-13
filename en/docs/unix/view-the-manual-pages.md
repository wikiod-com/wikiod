---
title: "View the Manual Pages"
slug: "view-the-manual-pages"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Viewing the Manual Page for a System Command
    man <command>

This will show the manual page for the specified command.

For example, `man ping` will show:

>
>     PING(8)                   BSD System Manager's Manual                  PING(8)
>     
>     NAME
>          ping -- send ICMP ECHO_REQUEST packets to network hosts
>     
>     SYNOPSIS
>          ping [-AaCDdfnoQqRrv] [-b boundif] [-c count] [-G sweepmaxsize]
>               [-g sweepminsize] [-h sweepincrsize] [-i wait] [-k trafficclass]
>               [-l preload] [-M mask | time] [-m ttl] [-P policy] [-p pattern]
>               [-S src_addr] [-s packetsize] [-t timeout] [-W waittime] [-z tos]
>               host
>          ping [-AaDdfLnoQqRrv] [-b boundif] [-c count] [-I iface] [-i wait]
>               [-k trafficclass] [-l preload] [-M mask | time] [-m ttl] [-P policy]
>               [-p pattern] [-S src_addr] [-s packetsize] [-T ttl] [-t timeout]
>               [-W waittime] [-z tos] mcast-group
>     
>     DESCRIPTION
>          The ping utility uses the ICMP protocol's mandatory ECHO_REQUEST datagram
>          to elicit an ICMP ECHO_RESPONSE from a host or gateway.  ECHO_REQUEST
>          datagrams (``pings'') have an IP and ICMP header, followed by a ``struct
>          timeval'' and then an arbitrary number of ``pad'' bytes used to fill out
>          the packet.  The options are as follows:
> 
>     ...

While viewing the manpage it can be searched. Typing a slash (`/`) followed by the search term will jump to the first occurence of the term. 
Example: `/ping` 

Pressing `N` afterwards will skip to the next occurrence. `Shift+N` will jump to the previous ocurrence.

## Search for a Manual Page
You can search for `man` pages containing a particular string in their description using:

    man -k <string>

For example:

    man -k unzip

Might return:

>     man -k unzip
>     IO::Uncompress::Bunzip2(3pm) - Read bzip2 files/buffers
>     IO::Uncompress::Gunzip(3pm) - Read RFC 1952 files/buffers
>     IO::Uncompress::Unzip(3pm) - Read zip files/buffers
>     PerlIO::gzip(3pm)        - Perl extension to provide a PerlIO layer to gzip/gunzip
>     gzip(1), gunzip(1), zcat(1) - compress or expand files
>     IO::Uncompress::Bunzip2(3pm) - Read bzip2 files/buffers
>     IO::Uncompress::Gunzip(3pm) - Read RFC 1952 files/buffers
>     IO::Uncompress::Unzip(3pm) - Read zip files/buffers
>     PerlIO::gzip(3pm)        - Perl extension to provide a PerlIO layer to gzip/gunzip
>     bzip2(1), bunzip2(1)     - a block-sorting file compressor, v1.0.6 bzcat - decompresses files to stdout bzip2recover - recovers data from damaged bzip2 files
>     funzip(1)                - filter for extracting from a ZIP archive in a pipe
>     unzip(1)                 - list, test and extract compressed files in a ZIP archive
>     unzipsfx(1)              - self-extracting stub for prepending to ZIP archives



## Find a man page in a different section
Sometimes a term is defined in multiple sections of the manual.  By default, `man` will only display the first page it finds, which can be annoying for programmers because C functions are documented in a later section than commands and system calls.  Use the following to display all pages that match a name:

    $ man -wa printf
    /usr/share/man/man1/printf.1.gz
    /usr/share/man/man1p/printf.1p.gz
    /usr/share/man/man3/printf.3.gz
    /usr/share/man/man3p/printf.3p.gz

To view the page from a specific section, simply place it before the term:

    man 3 printf
    


## Read a manual file with man
This is same as reading a manual for a command:

    man /path/to/man/file

## Get the File Path for a Manual Page
    $ man -w find
    /usr/share/man/man1/find.1.gz

    $ man -w printf
    /usr/share/man/man1/printf.1.gz

    $ man -w man
    /usr/share/man/man1/man.1.gz


