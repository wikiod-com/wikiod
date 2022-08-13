---
title: "Software Testing Techniques - State Transition"
slug: "software-testing-techniques---state-transition"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

This technique should be used when you have any workflow in place, and should consider positive test cases (transitions that can happen), as well as negative test cases (transitions that are not allowed).

Any rule that can be described, thinked, scratched as a state transition diagram, workflow, lifecycle can have their test cases designed using this technique.

This technique can be also works to find completeness problems inside worflows and diagrams during documentation analysis.

**HOT TIP**

If a **state transition like rule** is provided as a series of statements instead of a table or graphic diagram, you can proceed as following: 
1. Make yourself a state transition table as in the ticket system example
2. Add question marks for unclear transitions
3. Add an extra column at the end for broad discussion with business - raise questions and take notes

This is a way common situation we face on daily basis, and this practice can be handful to gain confidence on what is being developed and how it should be tested

## Order phases
**The rule**

An order lifecycle at any e-commerce follows roughly the workflow:

[![enter image description here][1]][1]

**How to apply the technique**

From the diagram we see that it has the following allowed (positive) transitions:

- From New to Cancelled
- From New to Approved
- From Approved to Cancelled
- From Approved to Shipped
- From Shipped to Delivered

And all the other unmentioned transitions should be treated as invalid (negative) transtitions

- From New to Shipped (negative)
- From New to Delivered (negative)
- From Approved to New (negative)
- From Approved to Delivered (negative)
- From Shipped to New (negative)
- From Shipped to Approved (negative)
- From Shipped to Cancelled (negative)
- From Delivered to New (negative)
- From Delivered to Approved (negative)
- From Delivered to Shipped (negative)
- From Delivered to Cancelled (negative)
- From Cancelled to New (negative)
- From Cancelled to Approved (negative)
- From Cancelled to Shipped (negative)
- From Cancelled to Delivered (negative)

  [1]: https://i.stack.imgur.com/LAxT6.png

## Ticket system
**The rule**

A ticket system have their valid transitions documented in the following table, where **O** represents allowed and **X** represents not allowed.

| From \ To | Reported |  Open  | In Progress | In Review | Delivered | Rejected | Reopen |
| --------- | -------- | ------ | ----------- | --------- | --------- | -------- | -------- |
| Reported    | - | O | X | X | X | O | X |
| Open        | X | - | O | X | X | O | X |
| In Progress | X | X | - | O | X | O | X |
| In Review   | X | O | O | - | O | O | X |
| Delivered   | X | X | X | X | - | X | O |
| Rejected    | X | X | X | X | X | - | X |
| Reopen      | X | X | O | X | X | X | - |


**How to apply technique**

There is no much secret applying this technique to design test cases.
1. Each transition show be be represented by one test case
2. There is no transition from one state to itself (diagonal line with dots)
3. The number of test cases is always equals to ( (n * n) - n )

For this case we have 7 states (n = 7). So we will have 42 test cases, as follows:

1. From Reported to Open (positive)
2. From Reported to In Progress (negative)
3. From Reported to In Review (negative)
4. From Reported to Delivered (negative)
5. From Reported to Rejected (positive)
6. From Reported to Reopen (negative)
7. From Open to Reported (negative)
8. From Open to In Progress (positive)
9. From Open to In Review (negative)
10. From Open to Delivered (negative)
11. From Open to Rejected (positive)
12. From Open to Reopen (negative)
13. From In Progress to Reported (negative)
14. From In Progress to Open (negative)
15. From In Progress to In Review (positive)
16. From In Progress to Delivered (negative)
17. From In Progress to Rejected (positive)
18. From In Progress to Reopen (negative)
19. From In Review to Reported (negative)
20. From In Review to Open (positive)
21. From In Review to In Progress (positive)
22. From In Review to Delivered (positive)
23. From In Review to Rejected (positive)
24. From In Review to Reopen (negative)
25. From Delivered to Reported (negative)
26. From Delivered to Open (negative)
27. From Delivered to In Progress (negative)
28. From Delivered to In Review (negative)
29. From Delivered to Rejected (negative)
30. From Delivered to Reopen (positive)
31. From Rejected to Reported (negative)
32. From Rejected to Open (negative)
33. From Rejected to In Progress (negative)
34. From Rejected to In Review (negative)
35. From Rejected to Delivered (negative)
36. From Rejected to Reopen (negative)
37. From Reopen to Reported (negative)
38. From Reopen to Open (negative)
39. From Reopen to In Progress (positive)
40. From Reopen to In Review (negative)
41. From Reopen to Delivered (negative)
42. From Reopen to Rejected (negative)


