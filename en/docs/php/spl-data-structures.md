---
title: "SPL data structures"
slug: "spl-data-structures"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## SplFixedArray
# Difference from PHP Array

PHP's default Array type is actually implemented as ordered hash maps, which allow us to create arrays that consist of key/value pairs where values can be of any type and keys can be either numbers or strings. This is not traditionally how arrays are created, however.

[![Traditional PHP Array Figure 1][1]][1]

So as you can see from this illustration a normal PHP array can be viewed more like an an ordered set of key/value pairs, where each key can map to any value. Notice in this array we have keys that are both numbers and strings, as well as values of different types and the key has no bearing on the order of the elements.

    $arr = [
        9     => "foo",
        1     => 4.2,
        "bar" => null,
    ];
    
    foreach($arr as $key => $value) {
        echo "$key => $value\n";
    }

So the above code would give us exactly what we'd expect.

<pre>
9 => foo
1 => 4.2
bar => 
</pre>

Regular PHP arrays are also dynamically sized for us. They grow and shrink as we push and pop values to and from the array, automatically.

---

However, in a traditional array the size is fixed and consists entirely of the same type of value. Also, rather than keys each value is access by its index, which can be deduced by its offset in the array.

[![SplFixedArray Figure 2][2]][2]

Since we would know the size of a given type and the fixed size of the array an offset is then the `type size * n` were `n` represents the value's position in the array. So in the example above `$arr[0]` gives us `1`, the first element in the array and `$arr[1]` gives us `2`, and so on.

SplFixedArray, however, doesn't restrict the type of values. It only restricts the keys to number types. It's also of a fixed size.

This makes SplFixedArrays more efficient than normal PHP arrays in one particular way. They are more compact so they require less memory.

# Instantiating the array

SplFixedArray is implemented as an object, but it can be accessed with the same familiar syntax that you access a normal PHP array since they implement the `ArrayAccess` interface. They also implement `Countable` and `Iterator` interfaces so they behave the same way you'd be used to arrays behaving in PHP (i.e. things like `count($arr)` and `foreach($arr as $k => $v)` work the same way for SplFixedArray as they do normal arrays in PHP.

The SplFixedArray constructor takes one argument, which is the size of the array.

    $arr = new SplFixedArray(4);
    
    $arr[0] = "foo";
    $arr[1] = "bar";
    $arr[2] = "baz";
    
    foreach($arr as $key => $value) {
        echo "$key => $value\n";
    }

This gives you what you would expect.

<pre>
0 => foo
1 => bar
2 => baz
3 => 
</pre>

This also works as expected.

    var_dump(count($arr));

Gives us...

<pre>
int(4)
</pre>

Notice in SplFixedArray, unlike a normal PHP Array, the key does depict the order of the element in our array, because it is a *true index* and not just a *map*.

# Resizing the array

Just keep in mind that because the array is of a fixed size, count will always return the same value. So while `unset($arr[1])` will result in `$arr[1] === null`, `count($arr)` still remains `4`.

So to resize the array you will need to call on the `setSize` method.

    $arr->setSize(3);

    var_dump(count($arr));
    
    foreach($arr as $key => $value) {
        echo "$key => $value\n";
    }

Now we get...

<pre>
int(3)
0 => foo
1 => 
2 => baz
</pre>

# Import to SplFixedArray & Export from SplFixedArray

You can also import/export a normal PHP Array into and out of an SplFixedArray with the `fromArray` and `toArray` methods.

    $array      = [1,2,3,4,5];
    $fixedArray = SplFixedArray::fromArray($array);
    
    foreach($fixedArray as $value) {
        echo $value, "\n";
    }

<pre>
1
2
3
4
5
</pre>

Going the other way.

    $fixedArray = new SplFixedArray(5);
    
    $fixedArray[0] = 1;
    $fixedArray[1] = 2;
    $fixedArray[2] = 3;
    $fixedArray[3] = 4;
    $fixedArray[4] = 5;
    
    $array = $fixedArray->toArray();
    
    foreach($array as $value) {
        echo $value, "\n";
    }

<pre>
1
2
3
4
5
</pre>

  [1]: http://i.stack.imgur.com/vWcnk.png
  [2]: http://i.stack.imgur.com/rW8gh.png




