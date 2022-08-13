---
title: "Base Conversion"
slug: "base-conversion"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Excel gives functions that will assist in converting from decimal to one of binary, octal and hexadecimal, and back again.

Note that there are no leading `0` or `0x` in the functions.

## Base Conversion
with the number `100` in cell A1, the results of these calculations

    =DEC2BIN(A1)
    =DEC2OCT(A1)
    =DEC2HEX(A1)
    =BIN2DEC(A1)
    =OCT2DEC(A1)
    =HEX2DEC(A1)

is

    1100100
    144
    64
           4
          64
         256

note that the first 3 functions are left justified, as they are strings, and the last 3 are right justified as they are numeric.


