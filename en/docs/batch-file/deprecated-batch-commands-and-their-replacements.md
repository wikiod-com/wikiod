---
title: "Deprecated batch commands and their replacements"
slug: "deprecated-batch-commands-and-their-replacements"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## BITSADMIN
[`BITSADMIN`][1] was used to transfer documents, download website, download files from websites, etc... This command is a deprecated command, and may be removed on next Windows updates. To prevent this issue, use the new [Powershell `BIT cmdlet`][2].

---

Here is a sample code utilizing `BITSADMIN`.

    @echo off
    Bitsadmin /create /download Stackoverflow.com
    rem download the website StackOverflow.com


  [1]: https://ss64.com/nt/bitsadmin.html
  [2]: https://ss64.com/ps/bits.html

## DEBUG
[`DEBUG`][1] command was used to create binaries/executabled from batch file. The command is still available in 32bit versions of windows , but is able to create binaries only with 16bit instructions with makes it unusable for 64bit machines. Now [CERTUTIL][2] is used for that purpose which allows to decode/encode binary/media files from HEX or BASE64 formats.Example with a file that creates icon file:

    @echo off
    
    del /q /f pointer.jpg >nul 2>nul
    certutil -decode "%~f0" pointer.jpg
    hh.exe pointer.jpg
    exit /b %errorlevel%
    
    -----BEGIN CERTIFICATE-----
    /9j/4AAQSkZJRgABAgAAZABkAAD/7AARRHVja3kAAQAEAAAAMgAA/+4ADkFkb2Jl
    AGTAAAAAAf/bAIQACAYGBgYGCAYGCAwIBwgMDgoICAoOEA0NDg0NEBEMDg0NDgwR
    DxITFBMSDxgYGhoYGCMiIiIjJycnJycnJycnJwEJCAgJCgkLCQkLDgsNCw4RDg4O
    DhETDQ0ODQ0TGBEPDw8PERgWFxQUFBcWGhoYGBoaISEgISEnJycnJycnJycn/8AA
    EQgACgAKAwEiAAIRAQMRAf/EAFsAAQEBAAAAAAAAAAAAAAAAAAAGBwEBAQAAAAAA
    AAAAAAAAAAAAAAEQAAIBAwQDAAAAAAAAAAAAAAEDAgARBSExIwQSIhMRAQEBAAAA
    AAAAAAAAAAAAAAARIf/aAAwDAQACEQMRAD8A13PZ5eIX3gO8ktKZfFPksvQ8r4uL
    ecJmx1BMSbm8D6UVKVcg/9k=
    -----END CERTIFICATE-----

Here's the same with hex format:

    @echo off
    
    (echo 0000    ff d8 ff e0 00 10 4a 46  49 46 00 01 02 00 00 64)   >pointer.hex
    (echo 0010    00 64 00 00 ff ec 00 11  44 75 63 6b 79 00 01 00)   >>pointer.hex
    (echo 0020    04 00 00 00 32 00 00 ff  ee 00 0e 41 64 6f 62 65)  >>pointer.hex
    (echo 0030    00 64 c0 00 00 00 01 ff  db 00 84 00 08 06 06 06)   >>pointer.hex
    (echo 0040    06 06 08 06 06 08 0c 08  07 08 0c 0e 0a 08 08 0a)   >>pointer.hex
    (echo 0050    0e 10 0d 0d 0e 0d 0d 10  11 0c 0e 0d 0d 0e 0c 11)   >>pointer.hex
    (echo 0060    0f 12 13 14 13 12 0f 18  18 1a 1a 18 18 23 22 22)   >>pointer.hex
    (echo 0070    22 23 27 27 27 27 27 27  27 27 27 27 01 09 08 08)   >>pointer.hex
    (echo 0080    09 0a 09 0b 09 09 0b 0e  0b 0d 0b 0e 11 0e 0e 0e)   >>pointer.hex
    (echo 0090    0e 11 13 0d 0d 0e 0d 0d  13 18 11 0f 0f 0f 0f 11)   >>pointer.hex
    (echo 00a0    18 16 17 14 14 14 17 16  1a 1a 18 18 1a 1a 21 21)   >>pointer.hex
    (echo 00b0    20 21 21 27 27 27 27 27  27 27 27 27 27 ff c0 00)   >>pointer.hex
    (echo 00c0    11 08 00 0a 00 0a 03 01  22 00 02 11 01 03 11 01)   >>pointer.hex
    (echo 00d0    ff c4 00 5b 00 01 01 01  00 00 00 00 00 00 00 00)   >>pointer.hex
    (echo 00e0    00 00 00 00 00 00 06 07  01 01 01 00 00 00 00 00)   >>pointer.hex
    (echo 00f0    00 00 00 00 00 00 00 00  00 00 01 10 00 02 01 03)   >>pointer.hex
    (echo 0100    04 03 00 00 00 00 00 00  00 00 00 00 01 03 02 00)   >>pointer.hex
    (echo 0110    11 05 21 31 23 04 12 22  13 11 01 01 01 00 00 00)   >>pointer.hex
    (echo 0120    00 00 00 00 00 00 00 00  00 00 00 11 21 ff da 00)   >>pointer.hex
    (echo 0130    0c 03 01 00 02 11 03 11  00 3f 00 d7 73 d9 e5 e2)   >>pointer.hex
    (echo 0140    17 de 03 bc 92 d2 99 7c  53 e4 b2 f4 3c af 8b 8b)   >>pointer.hex
    (echo 0150    79 c2 66 c7 50 4c 49 b9  bc 0f a5 15 29 57 20 ff)   >>pointer.hex
    (echo 0160    d9                                              )   >>pointer.hex
    
    certutil -decodehex "pointer.hex" pointer.jpg
    hh.exe pointer.jpg
    exit /b %errorlevel%

 As you can see hex requires additional temp file and hex format expansions is bigger


  [1]: http://www.robvanderwoude.com/debug.php
  [2]: https://ss64.org/viewtopic.php?id=1562

## APPEND
[`APPEND`][1] was command in msdos that allowed to use resource/media files like they are in the same directory.The command is still available in 32bit versions of windows but seems is not working. In some sources (including microsofts') it is pointed that the command is replaced by DPATH ,but it is not entirely true. Despite the DPATH help message points to APPEND command it's syntnax is the same as [`PATH`][2].The directories listed in DPATH can be used [with input redirection][3] or [type command][4] :

    @echo off
    dpath %windir%
    
    set /p var=<win.ini
    echo using dpath with input redirection:
    echo %var%
    echo.
    echo using dpath with type command:
    type win.ini


  [1]: https://technet.microsoft.com/en-us/library/bb491049.aspx
  [2]: https://ss64.com/nt/path.html
  [3]: https://ss64.org/viewtopic.php?id=1875
  [4]: https://ss64.org/viewtopic.php?id=1876


