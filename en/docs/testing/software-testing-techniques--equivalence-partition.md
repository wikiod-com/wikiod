---
title: "Software Testing Techniques - Equivalence Partition"
slug: "software-testing-techniques---equivalence-partition"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

This technique divides input data into data classes to reduce test cases amount to validate a rule. The idea is that given a set of possible equivalent values, using just one of those values will be enough to design a test case.

An advantage of this approach is reduction in the time required for testing a software due to lesser number of test cases.

It strives to find errors that may arise based on information classes and reduce to the minimum the effort needed in terms of test case design as well as test data mass.

## Login validation
**The Rule**:

The user will input credentials (email and password).

If credentials are valid, redirect to home page. Show an error message otherwise.

**How to apply technique**:

You can have a huge valid list of emails that could be used:
- admin@email.com
- foo@system.com
- bar@example.com
- ...

As well as a huge list of invalid emails
- xxx123
- foobar
- noreply@example.com
- ...

For passwords, you can have:
- a valid password for a specific and valid user
- non-valid password (one that does not exists on the system)

Equivalence Partition means that any element on a given individual set should be enough to design a test case. 

So for represent a valid email we can choose **bar@example.com**. There is no need to design test cases using **admin@email.com** or another from the set of valid emails to ensure a good coverage.

For represent an invalid email we can choose **xxx123**

Same logic applies to password

## Shipping fee based on zipcode
**The Rule**

An e-commerce have a shipping rule based on zipcodes. All shipping to California is eligible to free shipping. All other west cost are US$ 10.00 and The rest of USA is US$ 20.00

**How to apply technique**

We all know there are lots of zipcodes by state, and we are not going to use them all. For this case, we need:

1. One valid zipcode in California
2. One valid zipcode in West Coast, other than California
3. One valid zipcode in outside west coast
4. One invalid zipcode *

Thus, we reduce from thousands of zipcodes to only 4. A lot better now.

Despite it is not outlined in the rule, I have put the 4th one, the invalid zipcode.

This is because this is the **most important test**: testing the spec itself. In this case, thinking about dividing data into categories, we found a class that is not in the spec, and has no behaviour or expectations clearly set. I then include this class here and ask for partners/stakeholders/business analysts, what should be the behaviour for this case and then amend the spec or I can review and rip off this data class from my tests.

