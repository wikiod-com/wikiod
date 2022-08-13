---
title: "Intrinsic Functions"
slug: "intrinsic-functions"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

Intrinsic Functions are included in the COBOL standard as a set of functions that return values from a specific algorithm, given zero or more arguments. These intrinsic functions are provided as a facility of the compiler and runtime system.  The return items are temporary COBOL fields, and can be character data, bit fields, or numeric values.

Examples include trigonometric functions, date time routines, data type conversions, standard deviation, and other support algorithms.

COBOL 2014 lists the following standard Intrinsic Functions:

    ======================================== ==========
    Intrinsic Function                       Parameters
    ======================================== ==========
    FUNCTION ABS                             1
    FUNCTION ACOS                            1
    FUNCTION ANNUITY                         2
    FUNCTION ASIN                            1
    FUNCTION ATAN                            1
    FUNCTION BOOLEAN-OF-INTEGER              2
    FUNCTION BYTE-LENGTH                     1
    FUNCTION CHAR                            1
    FUNCTION CHAR-NATIONAL                   1
    FUNCTION COMBINED-DATETIME               2
    FUNCTION COS                             1
    FUNCTION CURRENCY-SYMBOL                 0
    FUNCTION CURRENT-DATE                    0
    FUNCTION DATE-OF-INTEGER                 1
    FUNCTION DATE-TO-YYYYMMDD                Variable
    FUNCTION DAY-OF-INTEGER                  1
    FUNCTION DAY-TO-YYYYDDD                  Variable
    FUNCTION DISPLAY-OF                      Variable
    FUNCTION E                               0
    FUNCTION EXCEPTION-FILE                  0
    FUNCTION EXCEPTION-FILE-N                0
    FUNCTION EXCEPTION-LOCATION              0
    FUNCTION EXCEPTION-LOCATION-N            0
    FUNCTION EXCEPTION-STATEMENT             0
    FUNCTION EXCEPTION-STATUS                0
    FUNCTION EXP                             1
    FUNCTION EXP10                           1
    FUNCTION FACTORIAL                       1
    FUNCTION FORMATTED-CURRENT-DATE          1
    FUNCTION FORMATTED-DATE                  2
    FUNCTION FORMATTED-DATETIME              Variable
    FUNCTION FORMATTED-TIME                  Variable
    FUNCTION FRACTION-PART                   1
    FUNCTION HIGHEST-ALGEBRAIC               1
    FUNCTION INTEGER                         1
    FUNCTION INTEGER-OF-BOOLEAN              1
    FUNCTION INTEGER-OF-DATE                 1
    FUNCTION INTEGER-OF-DAY                  1
    FUNCTION INTEGER-OF-FORMATTED-DATE       2
    FUNCTION INTEGER-PART                    1
    FUNCTION LENGTH                          1
    FUNCTION LENGTH-AN                       1
    FUNCTION LOCALE-COMPARE                  Variable
    FUNCTION LOCALE-DATE                     2
    FUNCTION LOCALE-TIME                     2
    FUNCTION LOCALE-TIME-FROM-SECONDS        2
    FUNCTION LOG                             1
    FUNCTION LOG10                           1
    FUNCTION LOWER-CASE                      1
    FUNCTION LOWEST-ALGEBRAIC                1
    FUNCTION MAX                             Variable
    FUNCTION MEAN                            Variable
    FUNCTION MEDIAN                          Variable
    FUNCTION MIDRANGE                        Variable
    FUNCTION MIN                             Variable
    FUNCTION MOD                             2
    FUNCTION MODULE-CALLER-ID                0
    FUNCTION MODULE-DATE                     0
    FUNCTION MODULE-FORMATTED-DATE           0
    FUNCTION MODULE-ID                       0
    FUNCTION MODULE-PATH                     0
    FUNCTION MODULE-SOURCE                   0
    FUNCTION MODULE-TIME                     0
    FUNCTION MONETARY-DECIMAL-POINT          0
    FUNCTION MONETARY-THOUSANDS-SEPARATOR    0
    FUNCTION NATIONAL-OF                     Variable
    FUNCTION NUMERIC-DECIMAL-POINT           0
    FUNCTION NUMERIC-THOUSANDS-SEPARATOR     0
    FUNCTION NUMVAL                          1
    FUNCTION NUMVAL-C                        2
    FUNCTION NUMVAL-F                        1
    FUNCTION ORD                             1
    FUNCTION ORD-MAX                         Variable
    FUNCTION ORD-MIN                         Variable
    FUNCTION PI                              0
    FUNCTION PRESENT-VALUE                   Variable
    FUNCTION RANDOM                          Variable
    FUNCTION RANGE                           Variable
    FUNCTION REM                             2
    FUNCTION REVERSE                         1
    FUNCTION SECONDS-FROM-FORMATTED-TIME     2
    FUNCTION SECONDS-PAST-MIDNIGHT           0
    FUNCTION SIGN                            1
    FUNCTION SIN                             1
    FUNCTION SQRT                            1
    FUNCTION STANDARD-COMPARE                Variable
    FUNCTION STANDARD-DEVIATION              Variable
    FUNCTION STORED-CHAR-LENGTH              1
    FUNCTION SUM                             Variable
    FUNCTION TAN                             1
    FUNCTION TEST-DATE-YYYYMMDD              1
    FUNCTION TEST-DAY-YYYYDDD                1
    FUNCTION TEST-FORMATTED-DATETIME         2
    FUNCTION TEST-NUMVAL                     1
    FUNCTION TEST-NUMVAL-C                   2
    FUNCTION TEST-NUMVAL-F                   1
    FUNCTION TRIM                            2
    FUNCTION UPPER-CASE                      1
    FUNCTION VARIANCE                        Variable
    FUNCTION WHEN-COMPILED                   0
    FUNCTION YEAR-TO-YYYY                    Variable
    ======================================== ==========

GnuCOBOL adds

    ======================================== ==========
    FUNCTION CONCATENATE                     Variable
    FUNCTION SUBSTITUTE                      Variable
    FUNCTION SUBSTITUTE-CASE                 Variable
    ======================================== ==========

The keyword `FUNCTION` is required unless source (or compile time option)
includes

    ENVIRONMENT DIVISION.
    CONFIGURATION SECTION.
    REPOSITORY.
        FUNCTION ALL INTRINSIC.

Where `ALL INTRINSIC` can be a list of functions to be used without the
`FUNCTION` prefix in `PROCEDURE DIVISION` statements.

The `LENGTH` function has a sorted history.  Some compilers include a `LENGTH`
reserved word.  For GnuCOBOL, this reserved word is only recognized when used
in the phrase `LENGTH OF`, the `OF` token is required to disambiguate the
function from the older reserved word extension.


## FUNCTION TRIM example
    01 some-string PIC X(32).

    ...

    MOVE "    a string literal" TO some-string

    DISPLAY ":" some-string ":"
    DISPLAY ":" FUNCTION TRIM(some-string) ":"
    DISPLAY ":" FUNCTION TRIM(some-string LEADING) ":"
    DISPLAY ":" FUNCTION TRIM(some-string TRAILING) ":"

Showing

    :    a string literal            :
    :a string literal:
    :a string literal            :
    :    a string literal:


## UPPER-CASE
<!-- language: lang-cobol -->

    MOVE FUNCTION UPPER-CASE("Hello World!") TO SOME-FIELD
    DISPLAY SOME-FIELD

**Output**

<!-- language: lang-none -->

    HELLO WORLD!

## LOWER-CASE function
<!-- language: lang-cobol -->

    MOVE FUNCTION LOWER-CASE("HELLO WORLD!") TO SOME-FIELD
    DISPLAY SOME-FIELD

**Output**

<!-- language: lang-none -->

    hello world!

