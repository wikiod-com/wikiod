---
title: "Join Strings"
slug: "join-strings"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Using String Interpolation
```
iex(1)> [x, y] = ["String1", "String2"]
iex(2)> "#{x} #{y}"
# "String1 String2"
```

## Using IO List
```
["String1", " ", "String2"] |> IO.iodata_to_binary 
# "String1 String2"
```
This will gives some performances boosts as strings not duplicated in memory.  
Alternative method:
```
iex(1)> IO.puts(["String1", " ", "String2"])
# String1 String2
```

## Using Enum.join
```
Enum.join(["String1", "String2"], " ")  
# "String1 String2"
```

