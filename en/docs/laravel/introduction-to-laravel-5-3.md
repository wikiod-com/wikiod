---
title: "Introduction to laravel-5.3"
slug: "introduction-to-laravel-53"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

New features, improvements and changes from Laravel 5.2 to 5.3

## The $loop variable
It is known for a while that dealing with loops in Blade has been limited, as of 5.3 there is a variable called `$loop` available

    @foreach($variables as $variable)

        // Within here the `$loop` variable becomes available

        // Current index, 0 based
        $loop->index;

        // Current iteration, 1 based
        $loop->iteration;

        // How many iterations are left for the loop to be complete
        $loop->remaining;

        // Get the amount of items in the loop
        $loop->count;

        // Check to see if it's the first iteration ...
        $loop->first;

        // ... Or last iteration
        $loop->last;

        //Depth of the loop, ie if a loop within a loop the depth would be 2, 1 based counting.
        $loop->depth;

        // Get's the parent `$loop` if the loop is nested, else null
        $loop->parent;

    @endforeach

