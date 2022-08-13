---
title: "Lua Scripting"
slug: "lua-scripting"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Redis provides a couple of mechanisms for extending the functionality of the database.  One mechanism is through the use of server-side LUA scripts that can be executed to manipulate data.  Lua scripts can be useful to perform expensive operations or to implement atomic operations that require logic.

## Commands For Scripting
Redis provides seven different operations for working with scripts:
* Eval operations (EVAL, EVALSHA)
* SCRIPT operations (DEBUG, EXISTS, FLUSH, KILL, LOAD)

The EVAL command evaluates a script provided as a string argument to the server.  Scripts can access the specified Redis keys named as arguments to the command and and additional string parameters that the user wants to pass to the script.  

For example, the command:
```
EVAL "return {KEYS[1],KEYS[2],ARGV[1],ARGV[2]}" 2 key1 key2 first second
```
causes the execution of a user defined Lua script that simply returns the values supplied.  The call is involved with 2 Redis keys (key1 and key2) and two parameters.

Another way to execute a Lua script is to first load it into the database then execute it using a SHA hash of the script.:
```
> script load "return {KEYS[1],KEYS[2],ARGV[1],ARGV[2]}"
"a42059b356c875f0717db19a51f6aaca9ae659ea"
> evalsha "a42059b356c875f0717db19a51f6aaca9ae659ea" 2 key1 key2 foo bar
1) "key1"
2) "key2"
3) "foo"
4) "bar"
```
The script load command loads the script and stores it in the database.  A sha signature of the script is returned so it can be referenced by future calls.  The EVALSHA function takes the sha and executes the corresponding script from the database.

