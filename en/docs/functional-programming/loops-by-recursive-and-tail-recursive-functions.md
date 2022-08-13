---
title: "Loops by Recursive and Tail Recursive Functions"
slug: "loops-by-recursive-and-tail-recursive-functions"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

As you already know, for the sake of immutability you can't process data using for loops and while loops. So we have recursive functions to rescue.

## non-recursive (where immutability isn't a concern)
    function sum(numbers) {
        var total = 0;
        for (var i = numbers.length - 1; i >= 0; i--) {
            total += numbers[i];
        }
        return total;
    }
It's a procedural code with mutations (over `total`).

## recursive to rescue
    function sum(numbers) {
        if(numbers.length == 0) {
            return 0;
        }
        return numbers[0] + sum(numbers.slice(1));
    }
this is the recursive version. there's no mutation, but we are making a call stack as below which uses extra memory.

sum([10, 5, 6, 7]);

&nbsp;&nbsp;&nbsp;&nbsp; 10 + sum([5, 6, 7]);

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 10 + 5 + sum([6, 7]);

&nbsp;&nbsp;&nbsp;&nbsp;  &nbsp;&nbsp;&nbsp;&nbsp;  &nbsp;&nbsp;&nbsp;&nbsp; 10 + 5 + 6 + sum([7]);

&nbsp;&nbsp;&nbsp;&nbsp;  &nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 10 + 5 + 6 + 7 + sum([]);

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 10 + 5 + 6 + 7 + 0;

## tail recursive to optimize
    function sum(numbers) {
        return tail_sum(numbers, 0);
    }
    
    function tail_sum(numbers, acc) {
        if(numbers.length == 0) {
            return acc;
        }
        return tail_sum(numbers.slice(1), acc + numbers[0]);
    }

in the tail recursive version, function return value does not need to wait till the end to do it its calculations, so there's no huge stack here; only two levels.

sum([10, 5, 6, 7]);

&nbsp;&nbsp;&nbsp;&nbsp; tail_sum([10, 5, 6, 7], 0);

&nbsp;&nbsp;&nbsp;&nbsp; tail_sum([5, 6, 7], 10);

&nbsp;&nbsp;&nbsp;&nbsp; tail_sum([6, 7], 15);

&nbsp;&nbsp;&nbsp;&nbsp; tail_sum([7], 21);

&nbsp;&nbsp;&nbsp;&nbsp; tail_sum([], 28);

&nbsp;&nbsp;&nbsp;&nbsp; 28;



