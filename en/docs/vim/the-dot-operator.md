---
title: "The dot operator"
slug: "the-dot-operator"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## Basic Usage
The dot operator repeats the last action performed, for instance:

file `test.tx`

```
helo, World!
helo, David!
```

(cursor at [1][1])   
Now perform a `cwHello<Esc>` (Change Word `helo` to `Hello`)  
Now the buffer looks like that:

```
Hello, World!
helo, David!
```
(cursor at [1][5])   
After typing `j_` the cursor is at [2][1].

Now enter the `.` and the last action is performed again:

```
Hello, World!
Hello, David!
```

(cursor at [2][5])


 

## Set indent
This is very useful when setting indent of your code

    if condition1
    if condition2
    # some commands here
    endif
    endif

move your cursor to the 2nd line, then `>>`, the code will indent to right.

Now you can repeat your action by continue to 3rd line, then hit `.` twice, the result will be

    if condition1
        if condition2
            # some commands here
    endif
    endif

