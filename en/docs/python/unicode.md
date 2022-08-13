---
title: "Unicode"
slug: "unicode"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Encoding and decoding
Always *encode* from unicode to bytes.  In this direction, **you get to choose the encoding**.

    >>> u'🐍'.encode('utf-8')
    '\xf0\x9f\x90\x8d'

The other way is to *decode* from bytes to unicode.   In this direction, **you have to know what the encoding is**.

    >>> b'\xf0\x9f\x90\x8d'.decode('utf-8')
    u'\U0001f40d'



