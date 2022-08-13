---
title : cobol Tutorial
slug : cobol-tutorial
weight : 9756
draft : false
images : []
type : docs
---

COBOL is the **CO**mmon **B**usiness **O**riented programming **L**anguage.

Even though it has become a pronounceable name, COBOL is still treated as an acronym by the standards committee, and COBOL is the preferred spelling by the ISO and INCITS standards bodies.

Standard Specification
----------------------

The current specification is

ISO/IEC 1989:2014 Information technology – Programming languages, their environments and system software interfaces – Programming language COBOL

That document was published in May of 2014 and can be purchased from various branches of standard bodies, officially homed at 

http://www.iso.org/iso/home/store/catalogue_tc/catalogue_detail.htm?csnumber=51416

Principal field of use
----------------------

Business oriented.  That usually means transaction processing.  Banking, government agencies, and the insurance industry are major areas of COBOL application deployments. IBM mainframe systems usually have a COBOL compiler installed.  There are upwards of 300 COBOL dialects in existence, with perhaps 10 or so versions taking the lion's share of deployments.  Most of these compilers are proprietary systems, but free software COBOL is also available.

Category
--------

COBOL is a procedural, imperative, compiled programming language.  As of the COBOL 2002 spec, Object Oriented features were added to the standard.

By design intent, COBOL is a very verbose programming language.  Although algebraic form is allowed:

    COMPUTE I = R * B

the initial intent was to use full words for computational descriptions and data manipulation:

    MULTIPLY INTEREST-RATE BY BALANCE GIVING CURRENT-INTEREST ROUNDED MODE IS NEAREST-EVEN

This design decision has both champions and detractors.  Some feel it is too verbose, while others argue that the syntax allows for greater readability in a business environment.

### Decimal Math

COBOL is designed around decimal arithmetic, unlike most languages that use a binary internal representation.  The COBOL spec calls for very precise fixed point decimal calculations, an aspect of the language that has been well regarded in financial sectors.  *COBOL also allows for USAGE BINARY, but leans towards decimal (base-10) representations.*

History
-------

COBOL dates back to the late 1950s, with initial implementations published in 1960.

U.S. Navy Rear Admiral Grace Hopper is often associated with COBOL, and championed on behalf of the language during the early stages of development.  She was not the only person involved in the design and development of COBOL, by any means, but is often referred to as the Mother of COBOL.

Due to early backing by governments and large corporations, COBOL has been in wide use for many decades. It remains a point of pride for some, and a thorn for others, who see it as outdated.  The truth likely lies somewhere in between these extreme views.  When applied to transaction processing, COBOL is at home.  When applied to modern web screens and networking applications it may not feel as comfortable.

Structure
---------

COBOL programs are written in four separate divisions.

- IDENTIFICATION DIVISION
- ENVIRONMENT DIVISION
- DATA DIVISION
- PROCEDURE DIVISION

Data Descriptions
-----------------

Being designed to handle decimal data, COBOL allows for PICTURE based data descriptions, in grouped hierarchies.

    01 record-group.
       05 balance        pic s9(8)v99.
       05 rate           pic 999v999.
       05 show-balance   pic $Z(7)9.99.

That defines `balance` as a signed eight digit value with two digits assumed after the decimal point.  `rate` is three digits before and three digits after an assumed decimal point.  `show-balance` is a numeric-edit field that will have a leading dollar sign, seven digits (zero suppressed) with at least one digit shown preceding two digits after a decimal point.

`balance` can be used in calculations, `show-balance` is only for display purposes and cannot be used in computational instructions.

Procedural statements
---------------------

COBOL is a reserved keyword heavy language.  MOVE, COMPUTE, MULTIPLY, PERFORM style long form words make up most of the standard specification.  Over 300 keywords and 47 operational statements in the COBOL 2014 spec.  Many compiler implementations add even more to the reserved word list.

