---
title: "Update Operators"
slug: "update-operators"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
- { $set: { \<field1>:\<value1>, \<field2>:\<value2>, ... } }

## Parameters
| parameters  | Meaning |
| ------ | ------ |
| *fieldName*   | Field will be updated :{***name***: 'Tom'}   |
| *targetVaule*   | Value will be assigned to the field :{name: ***'Tom'***}   |

Reference for $set operator:
[$set on offical website][1]


  [1]: https://docs.mongodb.com/manual/reference/operator/update/set/

## $set operator to update specified field(s) in document(s)
# I.Overview
A significant difference between MongoDB & RDBMS is MongoDB has many kinds of operators. One of them is update operator, which is used in update statements.

# II.What happen if we don't use update operators?
Suppose we have a **student** collection to store student information(Table view):
[![enter image description here][1]][1]

One day you get a job that need to change Tom's gender from "M" to "F". That's easy, right? So you write below statement very quickly based on your RDBMS experience:

    db.student.update(
        {name: 'Tom'}, // query criteria
        {sex: 'F'} // update action
    );

Let's see what is the result:
[![enter image description here][2]][2]

We lost Tom's age & name! From this example, we can know that **the whole document will be overrided** if without any update operator in update statement. This is the default behavior of MongoDB.

# III.$set operator
If we want to change only the 'sex' field in Tom's document, we can use `$set` to specify which field(s) we want to update:

    db.student.update(
        {name: 'Tom'}, // query criteria
        {$set: {sex: 'F'}} // update action
    );

The value of `$set` is an object, its fields stands for those fields you want to update in the documents, and the values of these fields are the target values.

So, the result is correct now:
[![enter image description here][3]][3]

Also, if you want to change both 'sex' and 'age' at the same time, you can append them to `$set` :

    db.student.update(
        {name: 'Tom'}, // query criteria
        {$set: {sex: 'F', age: 40}} // update action
    );


  [1]: http://i.stack.imgur.com/u0vd0.png
  [2]: http://i.stack.imgur.com/ykECO.png
  [3]: http://i.stack.imgur.com/6Fxmv.png

