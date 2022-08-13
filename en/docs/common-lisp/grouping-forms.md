---
title: "Grouping Forms"
slug: "grouping-forms"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Prog1 and Prog2
Often times, it is helpful to evaluate multiple expressions and to return the result from the first or second form rather than the last.  This is easy to accomplish using **let** and, for instance:

    (let ((form1-result form1))
      form2
      form3
      ;; ...
      form-n-1
      form-n
      form1-result)

Because this form is common in some applications, Common Lisp includes [**prog1** and **prog2**][1] that are like **progn**, but return the result of the first and second forms, respectively.  For instance:

    (prog1
      42
      (print 'hello)
      (print 'goodbye))
    ;; => 42

<!-- -->

    (prog2
      (print 'hello)
      42
      (print 'goodbye))
    ;; => 42

An important distinction between **prog1**/**prog2** and **progn**, however, is that **progn** returns *all* the values of the last form, whereas **prog1** and **prog2** only return the primary value of the first and second form.  For instance:

    (progn
      (print 'hello)
      (values 1 2 3))
    ;;=> 1, 2, 3
    
    (prog1
      (values 1 2 3)
      (print 'hello))
    ;;=> 1              ; not 1, 2, 3

For multiple values with **prog1** style evaluation, use [**multiple-value-prog1**][2] instead.  There is no similar **multiple-value-prog2**, but it is not difficult to implement if you need it.


  [1]: http://www.lispworks.com/documentation/HyperSpec/Body/m_prog1c.htm#prog1
  [2]: http://www.lispworks.com/documentation/HyperSpec/Body/s_mult_1.htm#multiple-value-prog1

## Block
The special operator [**block**][1] allows grouping of several Lisp forms (like an implicit `progn`) and it also takes a *name* to name the block.   When the forms within the block are evaluated, the special operator [**return-from**][3] can be used to leave the block.  For instance:

    (block foo
      (print 'hello)     ; evaluated
      (return-from foo)
      (print 'goodbye))  ; not evaluated
    ;;=> NIL

**return-from** can also be provided with a return value:

    (block foo
      (print 'hello)     ; evaluated
      (return-from foo 42)
      (print 'goodbye))  ; not evaluated
    ;;=> 42

Named blocks are useful when a chunk of code has a meaningful name, or when blocks are nested. In some context, only the ability to return from a block early is important.  In that case, you can use **nil** as the block name, and [**return**][2].  **Return** is just like **return-from**, except that the block name is always **nil**.

Note: enclosed forms are not top-level forms. That's different from `progn`, where the enclosed forms of a top-level `progn` form are still considered *top-level* forms. 


  [1]: http://www.lispworks.com/documentation/HyperSpec/Body/s_block.htm
  [2]: http://www.lispworks.com/documentation/HyperSpec/Body/m_return.htm#return
  [3]: http://www.lispworks.com/documentation/HyperSpec/Body/s_ret_fr.htm#return-from

## When is grouping needed?
In some places in Common Lisp, a series of forms are evaluated in order.  For instance, in the body of a **defun** or **lambda**, or the body of a **dotimes**.  In those cases, writing multiple forms in order works as expected.  In a few places, however, such as the *then* and *else* parts of an **if** expressions, only a single form is allowed.  Of course, one may want to actually evaluate multiple expressions in those places.  For those situations, some kind of implicit of explicit grouping form is needed.

## Progn
The general purpose special operator [**progn**][1] is used for evaluating zero or more forms.  The value of the last form is returned.  For instance, in the following, **(print 'hello)** is evaluated (and its result is ignored), and then **42** is evaluated and its result (**42**) is returned:

    (progn 
      (print 'hello)
      42)
    ;=> 42

If there are no forms within the **progn**, then **nil** is returned:

    (progn)
    ;=> NIL

In addition to grouping a series of forms, **progn** also has the important property that if the **progn** form is a [*top-level form*][2], then all the forms within it are processed as top level forms.  This can be important when writing macros that expand into multiple forms that should all be processed as top level forms.

**Progn** is also valuable in that it returns *all* the values of the last form.  For instance, 

    (progn
      (print 'hello)
      (values 1 2 3))
    ;;=> 1, 2, 3

In contrast, some grouping expressions only return the *primary value* of the result-producing form.
## Implicit Progns

Some forms use *implicit progns* to describe their behavior.  For instance, the [**when** and **unless**][3] macros, which are essentially one-sided **if** forms, describe their behavior in terms of an *implicit progn*.  This means that a form like

    (when (foo-p foo)
      form1
      form2)

is evaluated and the condition **(foo-p foo)** is true, then the *form1* and *form2* are grouped as though they were contained within a **progn**.  The expansion of the **when** macro is essentially:

    (if (foo-p foo)
      (progn
        form1
        form2)
      nil)

  [1]: http://www.lispworks.com/documentation/HyperSpec/Body/s_progn.htm
  [2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#top_level_form
  [3]: http://www.lispworks.com/documentation/HyperSpec/Body/m_when_.htm#when

## Tagbody
For lots of control in a group forms, the [**tagbody**][1] special operator can be very helpful.  The forms inside a **tagbody** form are either [*go tags*][2] (which are just symbols or integers) or forms to execute.  Within a **tagbody**, the [**go**][3] special operator is used to transfer execution to a new location.  This type of programming can be considered fairly low-level, as it allows arbitrary execution paths.  The following is a verbose example of what a for-loop might look like when implemented as a **tagbody**:

    (let (x)               ; for (x = 0; x < 5; x++) { print(hello); }
      (tagbody
         (setq x 0)
       prologue
         (unless (< x 5)
           (go end))
       begin
         (print (list 'hello x))
       epilogue
         (incf x)
         (go prologue)
       end))

While **tagbody** and **go** are not commonly used, perhaps due to "GOTO considered harmful", but can be helpful when implementing complex control structures like state machines.  Many iteration constructs also expand into an *implicit tagbody*.  For instance, the body of a [**dotimes**][4] is specified as a series of tags and forms.


  [1]: http://www.lispworks.com/documentation/HyperSpec/Body/s_tagbod.htm
  [2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#go_tag
  [3]: http://www.lispworks.com/documentation/HyperSpec/Body/s_go.htm#go
  [4]: http://www.lispworks.com/documentation/HyperSpec/Body/m_dotime.htm

## Which form to use?
When writing macros that expand into forms that might involve grouping, it is worthwhile spending some time considering what grouping construction to expand into.

For definition style forms, for instance, a **define-widget** macro that will usually appear as a top-level form, and that several **defun**s, **defstruct**s, etc., it usually makes sense to use a **progn**, so that child forms are processed as top-level forms.  For iteration forms, an implicit **tagbody** is more common.

For instance, the body of [**dotimes**][1], [**dolist**][2], and [**do**][3] each expand into an implicit **tagbody**.

For forms that define a named "chunk" of code, an implicit **block** is often useful.  For instance, while the body of a [**defun**][4] is inside an implicit **progn**, that implicit **progn** is within a block sharing the name of the function.  That means that **return-from** can be used to exit from the function.  Such a comp


  [1]: http://www.lispworks.com/documentation/HyperSpec/Body/m_dotime.htm
  [2]: http://www.lispworks.com/documentation/HyperSpec/Body/m_dolist.htm#dolist
  [3]: http://www.lispworks.com/documentation/HyperSpec/Body/m_do_do.htm#do
  [4]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm#defun

