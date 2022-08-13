---
title: "NumberFormat"
slug: "numberformat"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## NumberFormat
Different countries have different number formats and considering this we can have different formats using Locale of java. Using locale can help in formatting

    Locale locale = new Locale("en", "IN");
    NumberFormat numberFormat = NumberFormat.getInstance(locale);

using above format you can perform various tasks
1) Format Number

    `numberFormat.format(10000000.99);`


2) Format Currency

    `NumberFormat currencyFormat = NumberFormat.getCurrencyInstance(locale);
     currencyFormat.format(10340.999);`


3) Format Percentage

    `NumberFormat percentageFormat = NumberFormat.getPercentInstance(locale);
     percentageFormat.format(10929.999);`

4) Control Number of Digits


    numberFormat.setMinimumIntegerDigits(int digits)
    numberFormat.setMaximumIntegerDigits(int digits)
    numberFormat.setMinimumFractionDigits(int digits)
    numberFormat.setMaximumFractionDigits(int digits)

