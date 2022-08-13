---
title: "Sequence points"
slug: "sequence-points"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

**International Standard ISO/IEC 9899:201x Programming languages — C**

> Accessing a volatile object, modifying an object, modifying a file, or
> calling a function that does any of those operations are all *side
> effects*, which are changes in the state of the execution environment.
 
> The presence of a *sequence point* between the evaluation of expressions
> A and B implies that every value computation and side effect
> associated with A is sequenced before every value computation and side
> effect associated with B.

Here is the complete list of sequence points from Annex C of the [online 2011 pre-publication draft](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf) of the C language standard:

<blockquote>
<strong>Sequence points</strong><br><br>
1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The following are the sequence points described in 5.1.2.3:<br><br>
<ul><li>Between the evaluations of the function designator and actual arguments in a function
call and the actual call. (6.5.2.2).
<li>Between the evaluations of the first and second operands of the following operators:
logical AND <strong><code>&&</code></strong> (6.5.13); logical OR <strong><code>||</code></strong> (6.5.14); comma <strong><code>,</code></strong> (6.5.17).
<li>Between the evaluations of the first operand of the conditional <strong><code>? :</code></strong> operator and
whichever of the second and third operands is evaluated (6.5.15).
<li>The end of a full declarator: declarators (6.7.6);
<li>Between the evaluation of a full expression and the next full expression to be
evaluated. The following are full expressions: an initializer that is not part of a
compound literal (6.7.9); the expression in an expression statement (6.8.3); the
controlling expression of a selection statement (<strong><code>if</code></strong> or <strong><code>switch</code></strong>) (6.8.4); the
controlling expression of a <strong><code>while</code></strong> or <strong><code>do</code></strong> statement (6.8.5); each of the (optional)
expressions of a <strong><code>for</code></strong> statement (6.8.5.3); the (optional) expression in a <strong><code>return</code></strong>
statement (6.8.6.4).
<li>Immediately before a library function returns (7.1.4).
<li>After the actions associated with each formatted input/output function conversion
specifier (7.21.6, 7.29.2).
<li>Immediately before and immediately after each call to a comparison function, and
also between any call to a comparison function and any movement of the objects
passed as arguments to that call (7.22.5).
</ul>
</blockquote>

## Unsequenced expressions
<!-- if version [gte C11] --> 
The following expressions are *unsequenced*:

    a + b;
    a - b;
    a * b;
    a / b;
    a % b;
    a & b;
    a | b;

In the above examples, the expression `a` may be evaluated before or after the expression `b`, `b` may be evaluated before `a`, or they may even be intermixed if they correspond to several instructions.

A similar rule holds for function calls:

    f(a, b);

Here not only `a` and `b` are unsequenced (i.e. the `,` operator in a function call *does not* produce a sequence point) but also `f`, the expression that determines the function that is to be called.

Side effects may be applied immediately after evaluation or deferred until a later point. 

Expressions like

    x++ & x++;
    f(x++, x++); /* the ',' in a function call is *not* the same as the comma operator */
    x++ * x++;
    a[i] = i++;

or 

    x++ & x;
    f(x++, x);
    x++ * x;
    a[i++] = i;


will yield *undefined behavior* because

 - a modification of an object and any other access to it must be sequenced
 - the order of evaluation and the order in which *side effects*<sup>1</sup> are applied is not specified.

<!-- end version if --> 
____________________

<sub>1 Any changes in the state of the execution environment.</sub> 



## Sequenced expressions
The following expressions are *sequenced*:

    a && b
    a || b
    a , b
    a ? b : c
    for ( a ; b ; c ) { ... }
    
In all cases, the expression `a` is fully evaluated and *all side effects are applied* before either `b` or `c` are evaluated.  In the fourth case, only one of `b` or `c` will be evaluated.  In the last case, `b` is fully evaluated and all side effects are applied before `c` is evaluated.  

In all cases, the evaluation of expression `a` is *sequenced before* the evaluations of `b` or `c` (alternately, the evaluations of `b` and `c` are *sequenced after* the evaluation of `a`).  

Thus, expressions like

    x++ && x++
    x++ ? x++ : y++ 
    (x = f()) && x != 0
    for ( x = 0; x < 10; x++ ) { ... }
    y = (x++, x++);

have well defined behavior.  



## Indeterminately sequenced expressions
Function calls as `f(a)` always imply a sequence point between the evaluation of the arguments and the designator (here `f` and `a`) and the actual call. If two such calls are unsequenced, the two function calls are indeterminately sequenced, that is, one is executed before the other, and order is unspecified.


    unsigned counter = 0;

    unsingned account(void) {
       return counter++;
    }

    int main(void) {
       printf("the order is %u %u\n", account(), account());
    }

This implicit twofold modification of `counter` during the evaluation of the `printf` arguments is valid, we just don't know which of the calls comes first. As the order is unspecified, it may vary and cannot be depended on. So the printout could be:

> the order is 0 1

or

> the order is 1 0


The analogous statement to the above without intermediate function call

       printf("the order is %u %u\n", counter++, counter++); // undefined behavior

has undefined behavior because there is no sequence point between the two modifications of `counter`.



