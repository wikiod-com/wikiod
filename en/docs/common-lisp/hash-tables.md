---
title: "Hash tables"
slug: "hash-tables"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Creating a hash table
Hash tables are created by `make-hash-table`:

    (defvar *my-table* (make-hash-table))

The function may take keyword parameters to further specify the behavior of the resulting hash table:

 - `test`: Selects the function used to compare keys for equality. Maybe a designator for one of the functions `eq`, `eql`, `equal` or `equalp`. The default is `eq`.
 - `size`: A hint to the implementation about the space that may initially be required.
 - `rehash-size`: If an integer (>= 1), then when doing a rehash, the hash table will increase its capacity by the specified number. If otherwise an float (> 1.0), then the hash table will increase its capacity to the product of the `rehash-size` and the previous capacity.
 - `rehash-threshold`: Specifies how full the hash table has to be in order to trigger a rehash.

## Iterating over the entries of a hash table with loop
The [**loop**][1] macro supports iteration over the keys, the values, or the keys and values of a hash table.  The following examples show possibilities, but the full **loop** syntax allows more combinations and variants.

### Over keys and values

    (let ((ht (make-hash-table)))
      (setf (gethash 'a ht) 1
            (gethash 'b ht) 2)
      (loop for k being each hash-key of ht
         using (hash-value v)
         collect (cons k v)))
    ;;=> ((A . 1) (B . 2))

<!-- -->

    (let ((ht (make-hash-table)))
      (setf (gethash 'a ht) 1
            (gethash 'b ht) 2)
      (loop for v being each hash-value of ht
         using (hash-key k)
         collect (cons k v)))
    ;;=> ((A . 1) (B . 2))

### Over keys

    (let ((ht (make-hash-table)))
      (setf (gethash 'a ht) 1
            (gethash 'b ht) 2)
      (loop for k being each hash-key of ht
           collect k))
    ;;=> (A B)

### Over values

    (let ((ht (make-hash-table)))
      (setf (gethash 'a ht) 1
            (gethash 'b ht) 2)
      (loop for v being each hash-value of ht
           collect v))
    ;;=> (1 2)


  [1]: http://www.lispworks.com/documentation/lw51/CLHS/Body/m_loop.htm

## Iterating over the entries of a hash table with maphash
    (defun print-entry (key value)
      (format t "~A => ~A~%" key value))
    
    (maphash #'print-entry *my-table*) ;; => NIL

Using `maphash` allows to iterate over the entries of a hash table. The order of iteration is unspecified. The first argument is a function accepting two parameters: the key and the value of the current entry.

`maphash` always returns `NIL`.

## Iterating over the entries of a hash table with a hash table iterator
The keys and values of a hash table can be iterated over using the macro [**with-hash-table-iterator**][1].  This may be a bit more complex than **[maphash][2]** or **[loop][3]**, but it could be used to implement the iteration constructs used in those methods.  **with-hash-table-iterator** takes a name and a hash table and binds the name within a body such that successive calls to the name produce multiple values: (i) a boolean indicating whether a value is present; (ii) the key of the entry; and (iii) the value of the entry.

    (let ((ht (make-hash-table)))
      (setf (gethash 'a ht) 1
            (gethash 'b ht) 2)
      (with-hash-table-iterator (iterator ht)
        (print (multiple-value-list (iterator)))
        (print (multiple-value-list (iterator)))
        (print (multiple-value-list (iterator)))))
    
    ;; (T A 1) 
    ;; (T B 2) 
    ;; (NIL) 


  [1]: http://www.lispworks.com/documentation/HyperSpec/Body/m_w_hash.htm#with-hash-table-iterator
  [2]: https://www.wikiod.com/common-lisp/hash-tables#Iterating over the entries of a hash table with maphash
  [3]: https://www.wikiod.com/common-lisp/hash-tables#Iterating over the entries of a hash table with loop

