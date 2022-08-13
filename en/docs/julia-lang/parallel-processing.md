---
title: "Parallel Processing"
slug: "parallel-processing"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## @async and @sync
According to the documentation under `?@async`,  "`@async` wraps an expression in a Task."  What this means is that for whatever falls within its scope, Julia will start this task running but then proceed to whatever comes next in the script without waiting for the task to complete.  Thus, for instance, without the macro you will get:

    julia> @time sleep(2)
      2.005766 seconds (13 allocations: 624 bytes)

But with the macro, you get:

    julia> @time @async sleep(2)
      0.000021 seconds (7 allocations: 657 bytes)
    Task (waiting) @0x0000000112a65ba0

    julia> 

Julia thus allows the script to proceed (and the `@time` macro to fully execute) without waiting for the task (in this case, sleeping for two seconds) to complete.  

The `@sync` macro, by contrast, will "Wait until all dynamically-enclosed uses of `@async`, `@spawn`, `@spawnat` and `@parallel` are complete." (according to the documentation under `?@sync`).  Thus, we see:

    julia> @time @sync @async sleep(2)
      2.002899 seconds (47 allocations: 2.986 KB)
    Task (done) @0x0000000112bd2e00

In this simple example then, there is no point to including a single instance of `@async` and `@sync` together.  But, where `@sync` can be useful is where you have `@async` applied to multiple operations that you wish to allow to all start at once without waiting for each to complete.  

For example, suppose we have multiple workers and we'd like to start each of them working on a task simultaneously and then fetch the results from those tasks.  An initial (but incorrect) attempt might be:

    addprocs(2)
    @time begin
        a = cell(nworkers())
        for (idx, pid) in enumerate(workers())
            a[idx] = remotecall_fetch(pid, sleep, 2)
        end
    end
    ## 4.011576 seconds (177 allocations: 9.734 KB)

The problem here is that the loop waits for each `remotecall_fetch()` operation to finish, i.e. for each process to complete its work (in this case sleeping for 2 seconds) before continuing to start the next `remotecall_fetch()` operation.  In terms of a practical situation, we're not getting the benefits of parallelism here, since our processes aren't doing their work (i.e. sleeping) simultaneously.

We can correct this, however, by using a combination of the `@async` and `@sync`  macros:

    @time begin
        a = cell(nworkers())
        @sync for (idx, pid) in enumerate(workers())
            @async a[idx] = remotecall_fetch(pid, sleep, 2)
        end
    end
    ## 2.009416 seconds (274 allocations: 25.592 KB)

Now, if we count each step of the loop as a separate operation, we see that there are two separate operations preceded by the `@async` macro.  The macro allows each of these to start up, and the code to continue (in this case to the next step of the loop) before each finishes.  But, the use of the `@sync` macro, whose scope encompasses the whole loop, means that we won't allow the script to proceed past that loop until all of the operations preceded by `@async` have completed.

It is possible to get an even more clear understanding of the operation of these macros by further tweaking the above example to see how it changes under certain modifications.  For instance, suppose we just have the `@async` without the `@sync`:

    @time begin
        a = cell(nworkers())
        for (idx, pid) in enumerate(workers())
            println("sending work to $pid")
            @async a[idx] = remotecall_fetch(pid, sleep, 2)
        end
    end
    ## 0.001429 seconds (27 allocations: 2.234 KB)

Here, the `@async` macro allows us to continue in our loop even before each `remotecall_fetch()` operation finishes executing.  But, for better or worse, we have no `@sync` macro to prevent the code from continuing past this loop until all of the `remotecall_fetch()` operations finish.

Nevertheless, each `remotecall_fetch()` operation is still running in parallel, even once we go on.  We can see that because if we wait for two seconds, then the array a, containing the results, will contain:

    sleep(2)
    julia> a
    2-element Array{Any,1}:
     nothing
     nothing

(The "nothing" element is the result of a successful fetch of the results of the sleep function, which does not return any values)

We can also see that the two `remotecall_fetch()` operations start at essentially the same time because the `print` commands that precede them also execute in rapid succession (output from these commands not shown here).  Contrast this with the next example where the `print` commands execute at a 2 second lag from each other:

If we put the `@async` macro on the whole loop (instead of just the inner step of it), then again our script will continue immediately without waiting for the `remotecall_fetch()` operations to finish.  Now, however, we only allow for the script to continue past the loop as a whole.  We don't allow each individual step of the loop to start before the previous one finished.  As such, unlike in the example above, two seconds after the script proceeds after the loop, the `results` array still has one element as `#undef` indicating that the second `remotecall_fetch()` operation still has not completed.

    @time begin
        a = cell(nworkers())
        @async for (idx, pid) in enumerate(workers())
            println("sending work to $pid")
            a[idx] = remotecall_fetch(pid, sleep, 2)
        end
    end
    # 0.001279 seconds (328 allocations: 21.354 KB)
    # Task (waiting) @0x0000000115ec9120
    ## This also allows us to continue to

    sleep(2)

    a
    2-element Array{Any,1}:
        nothing
     #undef    

And, not surprisingly, if we put the `@sync` and `@async` right next to each other, we get that each `remotecall_fetch()` runs sequentially (rather than simultaneously) but we don't continue in the code until each has finished.  In other words, this would be essentially the equivalent of if we had neither macro in place, just like `sleep(2)` behaves essentially identically to `@sync @async sleep(2)`

    @time begin
        a = cell(nworkers())
        @sync @async for (idx, pid) in enumerate(workers())
            a[idx] = remotecall_fetch(pid, sleep, 2)
        end
    end
    # 4.019500 seconds (4.20 k allocations: 216.964 KB)
    # Task (done) @0x0000000115e52a10

Note also that it is possible to have more complicated operations inside the scope of the `@async` macro.  The [documentation][1] gives an example containing an entire loop within the scope of `@async`.

Recall that the help for the sync macros states that it will "Wait until all dynamically-enclosed uses of `@async`, `@spawn`, `@spawnat` and `@parallel` are complete."  For the purposes of what counts as "complete" it matters how you define the tasks within the scope of the `@sync` and `@async` macros.  Consider the below example, which is a slight variation on one of the examples given above:

    @time begin
        a = cell(nworkers())
        @sync for (idx, pid) in enumerate(workers())
            @async a[idx] = remotecall(pid, sleep, 2)
        end
    end
    ## 0.172479 seconds (93.42 k allocations: 3.900 MB)

    julia> a
    2-element Array{Any,1}:
     RemoteRef{Channel{Any}}(2,1,3)
     RemoteRef{Channel{Any}}(3,1,4)

The earlier example took roughly 2 seconds to execute, indicating that the two tasks were run in parallel and that the script waiting for each to complete execution of their functions before proceeding.  This example, however, has a much lower time evaluation.  The reason is that for the purposes of `@sync` the `remotecall()` operation has "finished" once it has sent the worker the job to do. (Note that the resulting array, a, here, just contains `RemoteRef` object types, which just indicate that there is something going on with a particular process which could in theory be fetched at some point in the future).  By contrast, the `remotecall_fetch()` operation has only "finished" when it gets the message from the worker that its task is complete. 

Thus, if you are looking for ways to ensure that certain operations with workers have completed before moving on in your script (as for instance is discussed in [this post](http://stackoverflow.com/questions/32143159/waiting-for-a-task-to-be-completed-on-remote-processor-in-julia?lq=1)) it is necessary to think carefully about what counts as "complete" and how you will measure and then operationalize that in your script.

[1]: http://julia.readthedocs.org/en/latest/manual/parallel-computing/

## @parallel
@parallel can be used to parallellize a loop, dividing steps of the loop up over different workers.  As a very simple example:

    addprocs(3)

    a = collect(1:10)

    for idx = 1:10
        println(a[idx])
    end

For a slightly more complex example, consider: 

    @time begin
        @sync begin
            @parallel for idx in 1:length(a)
                sleep(a[idx])
            end
        end
    end
    27.023411 seconds (13.48 k allocations: 762.532 KB)
    julia> sum(a)
    55

Thus, we see that if we had executed this loop without `@parallel` it would have taken 55 seconds, rather than 27, to execute.  

We can also supply a reduction operator for the `@parallel` macro.  Suppose we have an array, we want to sum each column of the array and then multiply these sums by each other:

    A = rand(100,100);

    @parallel (*) for idx = 1:size(A,1)
        sum(A[:,idx])
    end


There are several important things to keep in mind when using `@parallel` to avoid unexpected behavior.

**First:** if you want to use any functions in your loops that are not in base Julia (e.g. either functions you define in your script or that you import from packages), then you must make those functions accessible to the workers.  Thus, for example, the following would *not* work:

    myprint(x) = println(x)
    for idx = 1:10
        myprint(a[idx])
    end

Instead, we would need to use:

    @everywhere begin
        function myprint(x) 
            println(x)
        end
    end

    @parallel for idx in 1:length(a)
        myprint(a[idx])
    end

**Second** Although each worker will be able to access the objects in the scope of the controller, they will *not* be able to modify them.  Thus

    a = collect(1:10)
    @parallel for idx = 1:length(a)
       a[idx] += 1
    end

    julia> a'
    1x10 Array{Int64,2}:
     1  2  3  4  5  6  7  8  9  10

Whereas, if we had executed the loop wihtout the @parallel it would have successfully modified the array `a`.

TO ADDRESS THIS, we can instead make `a` a `SharedArray` type object so that each worker can access and modify it:

    a = convert(SharedArray{Float64,1}, collect(1:10))
    @parallel for idx = 1:length(a)
        a[idx] += 1
    end
    
    julia> a'
    1x10 Array{Float64,2}:
     2.0  3.0  4.0  5.0  6.0  7.0  8.0  9.0  10.0  11.0




## @spawn and @spawnat
The macros `@spawn` and `@spawnat` are two of the tools that Julia makes available to assign tasks to workers.  Here is an example:

    julia> @spawnat 2 println("hello world")
    RemoteRef{Channel{Any}}(2,1,3)

    julia>  From worker 2:  hello world

Both of these macros will evaluate an [expression](http://docs.julialang.org/en/release-0.4/manual/metaprogramming/#expressions-and-evaluation) on a worker process.  The only difference between the two is that `@spawnat` allows you to choose which worker will evaluate the expression (in the example above worker 2 is specified) whereas with `@spawn` a worker will be automatically chosen, based on availability.  

In the above example, we simply had worker 2 execute the println function.  There was nothing of interest to return or retrieve from this.  Often, however, the expression we sent to the worker will yield something we wish to retrieve.  Notice in the example above, when we called `@spawnat`, before we got the printout from worker 2, we saw the following:

    RemoteRef{Channel{Any}}(2,1,3)

This indicates that the `@spawnat` macro will return a `RemoteRef` type object.  This object in turn will contain the return values from our expression that is sent to the worker.  If we want to retrieve those values, we can first assign the `RemoteRef` that `@spawnat` returns to an object and then, and then use the `fetch()` function which operates on a `RemoteRef` type object, to retrieve the results stored from an evaluation performed on a worker.  

    julia> result = @spawnat 2 2 + 5
    RemoteRef{Channel{Any}}(2,1,26)

    julia> fetch(result)
    7

The key to being able to use `@spawn` effectively is understanding the nature behind the [expressions](http://docs.julialang.org/en/release-0.4/manual/metaprogramming/#expressions-and-evaluation) that it operates on.  Using `@spawn` to send commands to workers is slightly more complicated than just typing directly what you would type if you were running an "interpreter" on one of the workers or executing code natively on them.  For instance, suppose we wished to use `@spawnat` to assign a value to a variable on a worker.  We might try:

    @spawnat 2 a = 5
    RemoteRef{Channel{Any}}(2,1,2)

Did it work?  Well, let's see by having worker 2 try to print `a`.

    julia> @spawnat 2 println(a)
    RemoteRef{Channel{Any}}(2,1,4)

    julia> 

Nothing happened.  Why?  We can investigate this more by using `fetch()` as above.  `fetch()` can be very handy because it will retrieve not just successful results but also error messages as well.  Without it, we might not even know that something has gone wrong.

    julia> result = @spawnat 2 println(a)
    RemoteRef{Channel{Any}}(2,1,5)

    julia> fetch(result)
    ERROR: On worker 2:
    UndefVarError: a not defined

The error message says that `a` is not defined on worker 2.  But why is this?  The reason is that we need to wrap our assignment operation into an expression that we then use `@spawn` to tell the worker to evaluate.  Below is an example, with explanation following:

    julia> @spawnat 2 eval(:(a = 2))
    RemoteRef{Channel{Any}}(2,1,7)

    julia> @spawnat 2 println(a)
    RemoteRef{Channel{Any}}(2,1,8)

    julia>  From worker 2:  2

The `:()` syntax is what Julia uses to designate [expressions](http://docs.julialang.org/en/release-0.4/manual/metaprogramming/#expressions-and-evaluation).  We then use the `eval()` function in Julia, which evaluates an expression, and we use the `@spawnat` macro to instruct that the expression be evaluated on worker 2.

We could also achieve the same result as:

    julia> @spawnat(2, eval(parse("c = 5")))
    RemoteRef{Channel{Any}}(2,1,9)

    julia> @spawnat 2 println(c)
    RemoteRef{Channel{Any}}(2,1,10)

    julia>  From worker 2:  5

This example demonstrates two additional notions.  First, we see that we can also create an expression using the `parse()` function called on a string.  Secondly, we see that we can use parentheses when calling `@spawnat`, in situations where this might make our syntax more clear and manageable.



## pmap
`pmap` takes a function (that you specify) and applies it to all of the elements in an array.  This work is divided up amongst the available workers.  `pmap` then returns places the results from that function into another array.

    addprocs(3)
    sqrts = pmap(sqrt, 1:10)

if you function takes multiple arguments, you can supply multiple vectors to `pmap`

    dots = pmap(dot, 1:10, 11:20)

As with `@parallel`, however, if the function given to `pmap` is not in base Julia (i.e. it is user-defined or defined in a package) then you must make sure that function is available to all workers first:

    @everywhere begin
        function rand_det(n)
            det(rand(n,n))
        end
    end

    determinants = pmap(rand_det, 1:10)

See also [this](http://stackoverflow.com/questions/38515624/julia-run-function-multiple-times-save-results-in-array) SO Q&A.



## When to use @parallel vs. pmap
The Julia [documentation](http://docs.julialang.org/en/release-0.4/manual/parallel-computing/) advises that 

> pmap() is designed for the case where each function call does a large amount of work. In contrast, @parallel for can handle situations where each iteration is tiny, perhaps merely summing two numbers.

There are several reasons for this.  First, `pmap` incurs greater start up costs initiating jobs on workers.  Thus, if the jobs are very small, these startup costs may become inefficient.  Conversely, however, `pmap` does a "smarter" job of allocating jobs amongst workers.  In particular, it builds a queue of jobs and sends a new job to each worker whenever that worker becomes available.  `@parallel` by contrast, divvies up all work to be done amongst the workers when it is called.  As such, if some workers take longer on their jobs than others, you can end up with a situation where most of your workers have finished and are idle while a few remain active for an inordinate amount of time, finishing their jobs.  Such a situation, however, is less likely to occur with very small and simple jobs.  

The following illustrates this:  suppose we have two workers, one of which is slow and the other of which is twice as fast.  Ideally, we would want to give the fast worker twice as much work as the slow worker.  (or, we could have fast and slow jobs, but the principal is the exact same).  `pmap` will accomplish this, but `@parallel` won't.  

For each test, we initialize the following:

    addprocs(2)

    @everywhere begin
        function parallel_func(idx)
            workernum = myid() - 1 
            sleep(workernum)
            println("job $idx")
        end
    end

Now, for the `@parallel` test, we run the following:

    @parallel for idx = 1:12
        parallel_func(idx)
    end

And get back print output:

    julia>     From worker 2:    job 1
        From worker 3:    job 7
        From worker 2:    job 2
        From worker 2:    job 3
        From worker 3:    job 8
        From worker 2:    job 4
        From worker 2:    job 5
        From worker 3:    job 9
        From worker 2:    job 6
        From worker 3:    job 10
        From worker 3:    job 11
        From worker 3:    job 12


It's almost sweet.  The workers have "shared" the work evenly.  Note that each worker has completed 6 jobs, even though worker 2 is twice as fast as worker 3.  It may be touching, but it is inefficient.

For for the `pmap` test, I run the following:

    pmap(parallel_func, 1:12)

and get the output:

    From worker 2:    job 1
    From worker 3:    job 2
    From worker 2:    job 3
    From worker 2:    job 5
    From worker 3:    job 4
    From worker 2:    job 6
    From worker 2:    job 8
    From worker 3:    job 7
    From worker 2:    job 9
    From worker 2:    job 11
    From worker 3:    job 10
    From worker 2:    job 12

Now, note that worker 2 has performed 8 jobs and worker 3 has performed 4.  This is exactly in proportion to their speed, and what we want for optimal efficiency.  `pmap` is a hard task master - from each according to their ability.


## Adding Workers
When you first start Julia, by default, there will only be a single process running and available to give work to.  You can verify this using:

    julia> nprocs()
    1

In order to take advantage of parallel processing, you must first add additional workers who will then be available to do work that you assign to them.  You can do this within your script (or from the interpreter) using: `addprocs(n)` where `n` is the number of processes you want to use.  

Alternatively, you can add processes when you start Julia from the command line using:

    $ julia -p n

where `n` is how many *additional* processes you want to add.  Thus, if we start Julia with

    $ julia -p 2

When Julia starts we will get:

    julia> nprocs()
    3



