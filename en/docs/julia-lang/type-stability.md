---
title: "Type Stability"
slug: "type-stability"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

**Type instability** occurs when a variable's [type][1] can change at runtime, and hence cannot be inferred at compile-time. Type instability often causes performance problems, so being able to write and identify type-stable code is important.


  [1]: https://www.wikiod.com/julia-lang/types

## Write type-stable code
    function sumofsins1(n::Integer)  
        r = 0  
        for i in 1:n  
            r += sin(3.4)  
        end  
        return r  
    end  

    function sumofsins2(n::Integer)  
        r = 0.0  
        for i in 1:n  
            r += sin(3.4)  
        end  
        return r  
    end

Timing the above two functions shows major differences in terms of time and memory allocations.

    julia> @time [sumofsins1(100_000) for i in 1:100];
    0.638923 seconds (30.12 M allocations: 463.094 MB, 10.22% gc time)

    julia> @time [sumofsins2(100_000) for i in 1:100];
    0.163931 seconds (13.60 k allocations: 611.350 KB)

This is because of type-unstable code in `sumofsins1` where the type of `r` needs to be checked for every iteration.   

