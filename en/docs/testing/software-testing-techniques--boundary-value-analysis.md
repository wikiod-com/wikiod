---
title: "Software Testing Techniques - Boundary Value Analysis"
slug: "software-testing-techniques---boundary-value-analysis"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

This technique should be used whenever you have boundaries defined into a spec. It is a great idea to apply it to any rule based on time, values, any kind of counting or scale to be triggered.

It also ensure and helps finding n + 1 errors. And yes, it is an expansion from Equivalence Partition concepts. Your decision to apply this or the other should be the boundaries existing and being clearly defined.

Using the EP

          June   |          July         |    August
    ... 28 29 30 | 01 02 03 ... 29 30 31 | 01 02 03 ...
        ^                    ^                   ^

Using the BVA

          June   |          July         |    August
    ... 28 29 30 | 01 02 03 ... 29 30 31 | 01 02 03 ...
               ^ | ^                   ^ | ^


## Time-boxed offers
**The rule**

An e-commerce have a 20% off offer valid for entire month of July.

**How to apply the technique**

Using the Equivalence Partition technique we could divide this as
- Inside July
- Is outside July

But we can do a little better with this case. We can use the boundaries of the partition to ensure we are covering the set as follows:
- June 30th (Out)
- July 1st (In)
- July 31st (In)
- August 1st (Out)

Using this specific inputs into our test cases design, we are going to ensure that the offer is valid inside July, and not outside it.

## Progressive discount
**The rule**

Another common case we usually find and where it is useful to apply this technique are progressive rates based on values.

An e-commerce has an offer that is like:
- 10% off for orders above US$ 80.00
- 15% off for orders above US$ 150.00
- 25% off for orders above US$ 200.00

**How to apply the technique**

Rule of thumb: we always will pick the two sides of each boundary. For this case we will need to test with orders with the following values:

- US$ 79.99 (no discount)
- US$ 80.00 (10% off)
- US$ 149.99 (10% off)
- US$ 150.00 (15% off)
- US$ 199.99 (15% off)
- US$ 200.00 (25% off)

This is enough to cover all intervals with a great confidence level.

