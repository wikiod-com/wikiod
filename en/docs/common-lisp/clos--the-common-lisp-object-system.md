---
title: "CLOS - the Common Lisp Object System"
slug: "clos---the-common-lisp-object-system"
draft: false
images: []
weight: 9936
type: docs
toc: true
---

## Mixins and Interfaces
Common Lisp does not have interfaces in the sense that some languages (e.g., Java) do, and there is less need for that type of interface given that Common Lisp supports multiple inheritance and generic functions.  However, the same type of patterns can be realized easily using mixin classes.  This example shows the specification of a collection interface with several corresponding generic functions.

    ;; Specification of the COLLECTION "interface"
    
    (defclass collection () ()
      (:documentation "A collection mixin."))
    
    (defgeneric collection-elements (collection)
      (:documentation "Returns a list of the elements in the collection."))
    
    (defgeneric collection-add (collection element)
      (:documentation "Adds an element to the collection."))
    
    (defgeneric collection-remove (collection element)
      (:documentation "Removes the element from the collection, if it is present."))
    
    (defgeneric collection-empty-p (collection)
      (:documentation "Returns whether the collection is empty or not."))
    
    (defmethod collection-empty-p ((c collection))
      "A 'default' implementation of COLLECTION-EMPTY-P that tests
    whether the list returned by COLLECTION-ELEMENTS is the empty
    list."
      (endp (collection-elements c)))

An implementation of the interface is just a class that has the mixin as one of its super classes, and definitions of the appropriate generic functions.  (At this point, notice that the mixin class is really only for signalling the intent that the class implements the "interface".  This example would work just as well with a few generic functions and documentation that states that there are methods on the function for the class.)
    
    ;; Implementation of a sorted-set class
    
    (defclass sorted-set (collection)
      ((predicate
        :initarg :predicate
        :reader sorted-set-predicate)
       (test
        :initarg :test
        :initform 'eql
        :reader sorted-set-test)
       (elements
        :initform '()
        :accessor sorted-set-elements
        ;; We can "implement" the COLLECTION-ELEMENTS function, that is,
        ;; define a method on COLLECTION-ELEMENTS, simply by making it
        ;; a reader (or accessor) for the slot.
        :reader collection-elements)))
    
    (defmethod collection-add ((ss sorted-set) element)
      (unless (member element (sorted-set-elements ss)
                      :test (sorted-set-test ss))
        (setf (sorted-set-elements ss)
              (merge 'list
                     (list element)
                     (sorted-set-elements ss)
                     (sorted-set-predicate ss)))))
    
    (defmethod collection-remove ((ss sorted-set) element)
      (setf (sorted-set-elements ss)
            (delete element (sorted-set-elements ss))))

Finally, we can see what using an instance of the **sorted-set** class looks like when using the "interface" functions:
    
    (let ((ss (make-instance 'sorted-set :predicate '<)))
      (collection-add ss 3)
      (collection-add ss 4)
      (collection-add ss 5)
      (collection-add ss 3)
      (collection-remove ss 5)
      (collection-elements ss))
    ;; => (3 4)



## Creating a basic CLOS class without parents
A CLOS class is described by:

* a name
* a list of superclasses
* a list of slots
* further options like documentation

Each slot has:

* a name
* an initialization form (optional)
* an initialization argument (optional)
* a type (optional)
* a documentation string (optional)
* accessor, reader and/or writer functions (optional)
* further options like allocation 

Example:

    (defclass person ()
      ((name
        :initform      "Erika Mustermann" 
        :initarg       :name 
        :type          string
        :documentation "the name of a person"
        :accessor      person-name)
       (age
        :initform      25
        :initarg       :age
        :type          number
        :documentation "the age of a person"
        :accessor      person-age))
      (:documentation "a CLOS class for persons with name and age"))
     
     
A default print method:

    (defmethod print-object ((p person) stream)
      "The default print-object method for a person"
      (print-unreadable-object (p stream :type t :identity t)
        (with-slots (name age) p
          (format stream "Name: ~a, age: ~a" name age))))
    
 Creating instances:
   
    CL-USER > (make-instance 'person)
    #<PERSON Name: Erika Mustermann, age: 25 4020169AB3>
    
    CL-USER > (make-instance 'person :name "Max Mustermann" :age 24)
    #<PERSON Name: Max Mustermann, age: 24 4020169FEB>



