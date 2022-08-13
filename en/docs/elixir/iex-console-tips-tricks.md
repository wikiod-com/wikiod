---
title: "IEx Console Tips & Tricks"
slug: "iex-console-tips--tricks"
draft: false
images: []
weight: 9812
type: docs
toc: true
---

## Get value from last command with `v`
```
iex(1)> 1 + 1
2
iex(2)> v
2
iex(3)> 1 + v
3
```

See also: https://www.wikiod.com/elixir/iex-console-tips--tricks#Get the value of a previous command with `v`

## Persistent history
By default, user input history in `IEx` do not persist across different sessions.

`erlang-history` adds history support to both the Erlang shell and `IEx`:

    git clone git@github.com:ferd/erlang-history.git
    cd erlang-history
    sudo make install

You can now access your previous inputs using the up and down arrow keys, even across different `IEx` sessions.

## Recompile project with `recompile`
```
iex(1)> recompile
Compiling 1 file (.ex)
:ok
```

## Have your aliases ready when you start IEx
If you put your commonly used aliases into an `.iex.exs` file at the root of your app, IEx will load them for you on startup.

    alias App.{User, Repo}

## See information with `i`
```
iex(1)> i :ok
Term
  :ok
Data type
  Atom
Reference modules
  Atom
iex(2)> x = "mystring"
"mystring"
iex(3)> i x
Term
  "mystring"
Data type
  BitString
Byte size
  8
Description
  This is a string: a UTF-8 encoded binary. It's printed surrounded by
  "double quotes" because all UTF-8 encoded codepoints in it are printable.
Raw representation
  <<109, 121, 115, 116, 114, 105, 110, 103>>
Reference modules
  String, :binary
```

## See documentation with `h`
```
iex(1)> h List.last

                                 def last(list)

Returns the last element in list or nil if list is empty.

Examples

┃ iex> List.last([])
┃ nil
┃
┃ iex> List.last([1])
┃ 1
┃
┃ iex> List.last([1, 2, 3])
┃ 3
```

## Get the value of a previous command with `v`
```
iex(1)> a = 10
10
iex(2)> b = 20
20
iex(3)> a + b
30
```

You can get a specific row passing the index of the row:

```
iex(4)> v(3)
30
```

You can also specify an index relative to the current row:

```
iex(5)> v(-1) # Retrieves value of row (5-1) -> 4
30
iex(6)> v(-5) # Retrieves value of row (5-4) -> 1
10
```

The value can be reused in other calculations:

```
iex(7)> v(2) * 4
80
```

If you specify a non-existing row, `IEx` will raise an error:

```
iex(7)> v(100) 
** (RuntimeError) v(100) is out of bounds
    (iex) lib/iex/history.ex:121: IEx.History.nth/2
    (iex) lib/iex/helpers.ex:357: IEx.Helpers.v/1
```


## break out of incomplete expression
When you have entered something into IEx which expects a completion, such as a multiline string, IEx will change the prompt to indicate that it is waiting for you finish by changing the prompt to have an ellipsis (`...`) rather than `iex`.

If you find that IEx is waiting for you to finish an expression but you aren't sure what it needs to terminate the expression, or you simply want to abort this line of input, enter `#iex:break` as the console input. This will cause IEx to throw a `TokenMissingError` and cancel waiting for any more input, returning you to a standard "top-level" console input.

```
iex:1> "foo"
"foo"
iex:2> "bar
...:2> #iex:break
** (TokenMissingError) iex:2: incomplete expression
```


More info is available at [the IEx documentation](http://elixir-lang.org/docs/stable/iex/IEx.html#module-expressions-in-iex).

## Exit IEx console
1. Use Ctrl + C, Ctrl + C to exit
```
iex(1)>
BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
```

2. Use `Ctrl+ \` to immediately exit

## Creating PID
This is useful when you didn't store the PID from a previous command
```
iex(1)> self()
#PID<0.138.0>
iex(2)> pid("0.138.0")
#PID<0.138.0>
iex(3)> pid(0, 138, 0)
#PID<0.138.0>
```

## When Elixir console is stuck...
Sometimes you might accidentally run something in the shell that ends up waiting forever, and thus blocking the shell:

```
iex(2)> receive do _ -> :stuck end
```

In that case, press Ctrl-g.  You'll see:

`User switch command`
  
Enter these commands in order:

* `k` (to kill the shell process)
* `s` (to start a new shell process)
* `c` (to connect to the new shell process)

You'll end up in a new Erlang shell:

```
Eshell V8.0.2  (abort with ^G)
1>
```

To start an Elixir shell, type:

```
'Elixir.IEx.CLI':local_start().
```

(don't forget the final dot!)

Then you'll see a new Elixir shell process coming up:

```
Interactive Elixir (1.3.2) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> "I'm back"
"I'm back"
iex(2)>
```

To escape from “awaiting-for-more-input” mode (due to unclosed quotation mark, bracket etc,) type **`#iex:break`**, followed by carriage return (<kbd>⏎</kbd>):

```
iex(1)> "Hello, "world"
...(1)>
...(1)> #iex:break
** (TokenMissingError) iex:1: incomplete expression
    
iex(1)>
```
the above is specifically useful when copy-pasting a relatively huge snippet turns the console to “awaiting-for-more-input” mode.

## Load a module or script into the IEx session
If you have an elixir file; a script or a module and want to load it into the current IEx session, you can use the `c/1` method:

    iex(1)> c "lib/utils.ex"
    iex(2)> Utils.some_method

This will compile and load the module in IEx, and you'll be able to call all of it's public methods.

For scripts, it will immediately execute the contents of the script:

    iex(3)> c "/path/to/my/script.exs"
    Called from within the script!


