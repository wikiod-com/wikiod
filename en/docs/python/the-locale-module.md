---
title: "The locale Module"
slug: "the-locale-module"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

Python 2 Docs: [https://docs.python.org/2/library/locale.html#locale.currency][1]


  [1]: https://docs.python.org/2/library/locale.html#locale.currency

## Currency Formatting US Dollars Using the locale Module
    import locale
    
    locale.setlocale(locale.LC_ALL, '')
    Out[2]: 'English_United States.1252'
    
    locale.currency(762559748.49)
    Out[3]: '$762559748.49'
    
    locale.currency(762559748.49, grouping=True)
    Out[4]: '$762,559,748.49'

