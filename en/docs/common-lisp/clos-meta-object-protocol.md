---
title: "CLOS Meta-Object Protocol"
slug: "clos-meta-object-protocol"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Obtain the slot names of a Class
Lets say we have a class as

    (defclass person ()
      (name email age))

To obtain the names of the slots of the class we use the function class-slots. This can be found in the closer-mop package, provided by the closer-mop system. To load it the running lisp image we use `(ql:quickload :closer-mop)`. We also have to make sure the class is finalized before calling class-slots.

    (let ((class (find-class 'person)))
      (c2mop:ensure-finalized class)
      (c2mop:class-slots class))

which returns a list of *effective slot definition* objects:

    (#<SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION S/TRANSFORMATIONS::NAME>
     #<SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION S/TRANSFORMATIONS::EMAIL>
     #<SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION S/TRANSFORMATIONS::AGE>)

## Update a slot when another slot is modified
The CLOS MOP provides the hook slot-value-using-class, that is called when a slot is value is accessed, read or modified. Because we only care for modifications in this case we define a method for `(setf slot-value-using-class)`.

    (defclass document ()
      ((id :reader id :documentation "A hash computed with the contents of every other slot")
       (title :initarg :title :accessor title)
       (body :initarg :body :accessor body)))
    
    (defmethod (setf c2mop:slot-value-using-class) :after
        (new class (object document) (slot c2mop:standard-effective-slot-definition))
      ;; To avoid this method triggering a call to itself, we check that the slot
      ;; the modification occurred in is not the slot we are updating.
      (unless (eq (slot-definition-name slot) 'id)
        (setf (slot-value object 'id) (hash-slots object))))

Note that because at instance creation `slot-value` is not called it may be necessary to duplicate the code in the `initialize-instance :after` method
    
    (defmethod initialize-instance :after ((obj document) &key)
      (setf (slot-value obj 'id)
            (hash-slots obj)))

