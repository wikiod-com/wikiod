---
title: "Working with objects"
slug: "working-with-objects"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## .has
Determine if an object has (or contains) a key. If the key to search for is expressed as a path (with dot notation) it will traverse nested object structures to determine if the key exists.

    var obj = {
      a: 2,
      b: 3,
      c: {
        dd:40,
        ee:{
          fff:500
        }
      }
    };
    
    var res1 = _.has(obj, "a");          // true
    var res2 = _.has(obj, "a.b");        // false
    var res3 = _.has(obj, "c");          // true
    var res4 = _.has(obj, "c.ee");       // true
    var res5 = _.has(obj, "c.fff");      // false
    var res6 = _.has(obj, "c.dd.fff");   // false
    var res7 = _.has(obj, "c.ee.fff");   // true

Arrays can be used to split up parts of the path instead of strings

    var res8 = _.has(obj, ["a", "b"]);   // false, same as res2
    var res9 = _.has(obj, ["c", "ee"]);  // true, same as res4

## Note
`_.has` will only look at the direct properties (aka. owned properties) of the object.

