---
title: "Types conversion in Talend"
slug: "types-conversion-in-talend"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

A list of type conversion in talend with some examples.

## Table of Conversions
    ╔═══════════╦════════════╦═══════════════════════════════════════════════════════════════════╗
    ║ From      ║ To         ║ Example                                                           ║
    ╠═══════════╬════════════╬═══════════════════════════════════════════════════════════════════╣
    ║String     ║ Integer    ║ Integer.parseInt(str) OR Integer.valueOf(str).intValue()          ║
    ║String     ║ Date       ║ TalendDate.parseDate("dd-MM-yyyy", str)                           ║
    ║String     ║ BigDecimal ║ new BigDecimal(str)                                               ║
    ║String     ║ Float      ║ Float.parseFloat(str) OR Float.valueOf(str).floatValue();         ║
    ║String     ║ Long       ║ Long.parseLong(str) OR long l = Long.valueOf(str).longValue()     ║
    ║String     ║ Double     ║ double d = Double.valueOf(str).doubleValue()                      ║
    ║Date       ║ String     ║ TalendDate.formatDate("yy-MM-dd", row1.myDate)                    ║
    ║Float      ║ String     ║ row1.myFloat.toString()                                           ║
    ║Float      ║ BigDecimal ║ new BigDecimal(Float.toString(row1.myFloat))                      ║
    ║Float      ║ Double     ║ (float)d                                                          ║
    ║Float      ║ Integer    ║ First round : Math.round(), Math.ceil(), Math.floor() then cast   ║
    ║           ║            ║ the result to Integer                                             ║
    ║Long       ║ Int        ║ (int)( row1.var + 0) The max possible value is 2147483647         ║
    ║Long       ║ String     ║ row1.myLong.toString                                              ║
    ║Integer    ║ Long       ║ row1.myInteger.longValue()                                        ║
    ║Integer    ║ BigDecimal ║ new BigDecimal(row1.myInteger)                                    ║
    ║Integer    ║ Float      ║ new Float(row1.myInteger)                                         ║
    ║Integer    ║ String     ║ variable+"" OR variable.toString()                                ║
    ║BigDecimal ║ Integer    ║ As with Float, BigDecimal can have decimal places, so will need   ║
    ║           ║            ║ to be rounded prior to casting to Integer                         ║
    ║BigDecimal ║ String     ║ row1.myBigDecimal.toString()                                      ║
    ║ Double    ║ String     ║ String str = Double.toString(d)                                   ║
    ║ Double    ║ Float      ║ double d = f                                                      ║
    ╚═══════════╩════════════╩═══════════════════════════════════════════════════════════════════╝




