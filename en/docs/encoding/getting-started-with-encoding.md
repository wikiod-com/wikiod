---
title: "Getting started with encoding"
slug: "getting-started-with-encoding"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## How to detect the encoding of a text file with Python?
There is a useful package in Python - chardet, which helps to detect the encoding used in your file. Actually there is no program that can say with 100% confidence which encoding was used - that's why chardet gives the encoding with the highest probability the file was encoded with. Chardet can detect following encodings: 

 - ASCII, UTF-8, UTF-16 (2 variants), UTF-32 (4 variants)
 - Big5, GB2312, EUC-TW, HZ-GB-2312, ISO-2022-CN (Traditional and Simplified Chinese)
 - EUC-JP, SHIFT_JIS, CP932, ISO-2022-JP (Japanese)
 - EUC-KR, ISO-2022-KR (Korean)
 - KOI8-R, MacCyrillic, IBM855, IBM866, ISO-8859-5, windows-1251 (Cyrillic)
 - ISO-8859-2, windows-1250 (Hungarian)
 - ISO-8859-5, windows-1251 (Bulgarian)
 - windows-1252 (English)
 - ISO-8859-7, windows-1253 (Greek)
 - ISO-8859-8, windows-1255 (Visual and Logical Hebrew)
 - TIS-620 (Thai)

You can install chardet with a [pip][1] command: 

    pip install chardet

Afterward you can use chardet either in the command line:

    % chardetect somefile someotherfile
    somefile: windows-1252 with confidence 0.5
    someotherfile: ascii with confidence 1.0

or in python: 

    import chardet    
    rawdata = open(file, "r").read()
    result = chardet.detect(rawdata)
    charenc = result['encoding']

  [1]: https://www.wikiod.com/python/getting-started-with-python-language#Installing external modules using pip

## Installation or Setup
Detailed instructions on getting encoding set up or installed.

