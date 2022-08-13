---
title: "Coroutines"
slug: "coroutines"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Syntax
 - coroutine.create(function) returns a coroutine (type(coroutine) == 'thread') containing the function.

 - coroutine.resume(co, ...) resume, or start the coroutine. Any additional arguments given to resume are returned from the coroutine.yield() that previously paused the coroutine. If the coroutine had not been started the additional arguments become the arguments of the function.

 - coroutine.yield(...) yields the currently running coroutine. Execution picks back up after the call to coroutine.resume() that started that coroutine. Any arguments given to yield are returned from the corresponding coroutine.resume() that started the coroutine.

 - coroutine.status(co) returns the status of the coroutine, which can be :

      - "dead" : the function in the coroutine has reached it's end and the coroutine cannot be resumed anymore
      - "running" : the coroutine has been resumed and is running
      - "normal" : the coroutine has resumed another coroutine
      - "suspended" : the coroutine has yielded, and is waiting to be resumed

 - coroutine.wrap(function) returns a function that when called resumes the coroutine that would have been created by coroutine.create(function).

The coroutine system has been implemented in lua to emulate multithreading existing in other languages. It works by switching at extremely high speed between different functions so that the human user think they are executed at the same time.

## Create and use a coroutine
All functions to interact with coroutines are avaliable in the **coroutine** table. A new coroutine is created by using the **coroutine.create** function with a single argument: a function with the code to be executed:

    thread1 = coroutine.create(function()
                print("honk")
            end)

    print(thread1)
    -->> thread: 6b028b8c

A coroutine object returns value of type **thread**, representing a new coroutine. When a new coroutine is created, its initial state is suspended:

    print(coroutine.status(thread1))
    -->> suspended

To resume or start a coroutine, the function **coroutine.resume** is used, the first argument given is the thread object:
    
    coroutine.resume(thread1)
    -->> honk

Now the coroutine executes the code and terminates, changing its state to **dead**, wich cannot be resumed.

    print(coroutine.status(thread1))
    -->> dead

Coroutines can suspend its execution and resume it later thanks to the **coroutine.yield** function:

    thread2 = coroutine.create(function()
            for n = 1, 5 do
                print("honk "..n)
                coroutine.yield()
            end
        end)

As you can see, **coroutine.yield()** is present inside the for loop, now when we resume the coroutine, it will execute the code until it reachs a coroutine.yield:

    coroutine.resume(thread2)
    -->> honk 1
    coroutine.resume(thread2)
    -->> honk 2

After finishing the loop, the thread status becomes **dead** and cannot be resumed. Coroutines also allows the exchange between data:

    thread3 = coroutine.create(function(complement)
        print("honk "..complement)
        coroutine.yield()
        print("honk again "..complement)
    end)
    coroutine.resume(thread3, "stackoverflow")
    -->> honk stackoverflow

If the coroutine is executed again with no extra arguments, the *complement* will still 
the argument from the first resume, in this case "stackoverflow":

    coroutine.resume(thread3)
    -->> honk again stackoverflow

Finally, when a coroutine ends, any values returned by its function go to the corresponding resume:

    thread4 = coroutine.create(function(a, b)
        local c = a+b
        coroutine.yield()
        return c
    end)
    coroutine.resume(thread4, 1, 2)
    print(coroutine.resume(thread4))
    -->> true, 3

Coroutines are used in this function to pass values back to a calling thread from deep within a recursive call.

    local function Combinations(l, r)
        local ll = #l
        r = r or ll
        local sel = {}
        local function rhelper(depth, last)
            depth = depth or 1
            last = last or 1
            if depth > r then
                coroutine.yield(sel)
            else
                for i = last, ll - (r - depth) do
                    sel[depth] = l[i]
                    rhelper(depth+1, i+1)
                end
            end
        end
        return coroutine.wrap(rhelper)
    end
    
    for v in Combinations({1, 2, 3}, 2) do
        print("{"..table.concat(v, ", ").."}")
    end
    --> {1, 2}
    --> {1, 3}
    --> {2, 3}

Coroutines can also be used for lazy evaluation.

    -- slices a generator 'c' taking every 'step'th output from the generator
    -- starting at the 'start'th output to the 'stop'th output
    function slice(c, start, step, stop)
        local _
        return coroutine.wrap(function()
            for i = 1, start-1 do
                _ = c()
            end
            for i = start, stop do
                if (i - start) % step == 0 then
                    coroutine.yield(c())
                else
                    _ = c()
                end
            end
        end)
    end


    local alphabet = {}
    for c = string.byte('a'), string.byte('z') do
        alphabet[#alphabet+1] = string.char(c)
    end
    -- only yields combinations 100 through 102
    -- requires evaluating the first 100 combinations, but not the next 5311633
    local s = slice(Combinations(alphabet, 10), 100, 1, 102)
    for i in s do
        print(table.concat(i))
    end
    --> abcdefghpr
    --> abcdefghps
    --> abcdefghpt
    
Coroutines can be used for piping constructs as described in [Programming In Lua][1]. The author of PiL, Roberto Ierusalimschy, has also published a [paper][2] on using coroutines to implement more advanced and general flow control mechanics like continuations.


  [1]: https://www.lua.org/pil/9.2.html
  [2]: http://www.inf.puc-rio.br/~roberto/docs/MCC15-04.pdf

