---
title: "Selections"
slug: "selections"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Syntax
- d3.**select**(selector)
- d3.**selectAll**(selector)
- *selection*.**select**(selector)
- *selection*.**selectAll**(selector)
- *selection*.**filter**(filter)
- *selection*.**merge**(other)


Related Readings:

 - [How Selections Work - Mike Bostock](https://bost.ocks.org/mike/selection/)
 - [d3-selection README](https://github.com/d3/d3-selection/blob/master/README.md#selection)

## The role of placeholders in "enter" selections
**What is an enter selection?**

In D3.js, when one binds data to DOM elements, three situations are possible:

 1. The number of elements and the number of data points are the same;
 2. There are more elements than data points;
 3. There are more data points than elements;

In the situation #3, all the data points without a corresponding DOM element belong to the *enter* selection. Thus, In D3.js, *enter* selections are selections that, after joining elements to the data, contains all the data that don't match any DOM element. If we use an `append` function in an *enter* selection, D3 will create new elements binding that data for us.

This is a Venn diagram explaining the possible situations regarding number of data points/number of DOM elements:

[![enter image description here][1]][1]

As we can see, the *enter* selection is the blue area at the left: data points without corresponding DOM elements.

**The structure of the enter selection**

Typically, an *enter* selection has these 4 steps:

1. `selectAll`: Select elements in the DOM;
2. `data`: Counts and parses the data;
3. `enter`: Comparing the selection with the data, creates new elements;
4. `append`: Append the actual elements in the DOM;

This is a very basic example (look at the 4 steps in the `var divs`):

    var data = [40, 80, 150, 160, 230, 260];

    var body = d3.select("body");
  
    var divs = body.selectAll("div")
        .data(data)
        .enter()
        .append("div");
    
    divs.style("width", function(d) { return d + "px"; })
        .attr("class", "divchart")
        .text(function(d) { return d; });

And this is the result ([jsfiddle here][2]):

[![enter image description here][3]][3]

Notice that, in this case, we used `selectAll("div")` as the first line in our "enter" selection variable. We have a dataset with 6 values, and D3 created 6 divs for us.

**The role of placeholders**

But suppose that we already have a div in our document, something like `<div>This is my chart</div>` at the top. In that case, when we write:

    body.selectAll("div")

we are selecting that existent div. So, our enter selection will have only 5 datum without matching elements. For instance, [in this jsfiddle][4], where there is already a div in the HTML ("This is my chart"), this will be the outcome: 

[![enter image description here][5]][5]

We don't see the value "40" anymore: our first "bar" disappeared, and the reason for that is that our "enter" selection now has only 5 elements.

What we have to understand here is that in the first line of our enter selection variable, `selectAll("div")`, those divs are just *placeholders*. We don't have to select all the `divs` if we are appending `divs`, or all the `circle` if we are appending `circle`. We can select different things. And, if we don't plan to have an "update" or an "exit" selection, we can select *anything*:

    var divs = body.selectAll(".foo")//this class doesn't exist, and never will!
        .data(data)
        .enter()
        .append("div");

Doing this way, we are selecting all the ".foo". Here, "foo" is a class that not only doesn't exist, but also it's never created anywhere else in the code! But it doesn't matter, this is only a placeholder. The logic is this:

> If in your "enter" selection you select something that doesn't exist, your "enter" selection will *always* contain all your data.

Now, selecting `.foo`, our "enter" selection have 6 elements, even if we already have a div in the document:

[![enter image description here][6]][6]

And here is the [corresponding jsfiddle][7].

**Selecting `null`**

By far, the best way to guarantee that you are selecting nothing is selecting `null`. Not only that, but this alternative is way faster than any other.

Thus, for an enter selection, just do:

    selection.selectAll(null)
        .data(data)
        .enter()
        .append(element);
 
Here is a demo fiddle: https://jsfiddle.net/gerardofurtado/th6s160p/

**Conclusion**

When dealing with "enter" selections, take extra care to do not select something that already exists. You can use anything in your `selectAll`, even things that don't exist and will never exist (if you don't plan to have an "update" or an "exit" selection).

The code in the examples is based on this code by Mike Bostock: https://bl.ocks.org/mbostock/7322386


  [1]: http://i.stack.imgur.com/ybk1Rm.png
  [2]: https://jsfiddle.net/qtqzfda7/
  [3]: http://i.stack.imgur.com/NItFz.jpg
  [4]: https://jsfiddle.net/qtqzfda7/1/
  [5]: http://i.stack.imgur.com/5XUmB.jpg
  [6]: http://i.stack.imgur.com/X5ftR.jpg
  [7]: https://jsfiddle.net/qtqzfda7/2/

## Using "this" with an arrow function
Most of functions in D3.js accept an anonymous function as an argument. The common examples are `.attr`, `.style`, `.text`, `.on` and `.data`, but the list is way bigger than that.

In such cases, the anonymous function is evaluated for each selected element, in order, being passed:

1.    The current datum (`d`)
2.    The current index (`i`)
3.    The current group (`nodes`)
4.    `this` as the current DOM element.

The datum, the index and the current group are passed as arguments, the famous first, second and third argument in D3.js (whose parameters are traditionally named `d`, `i` and `p` in D3 v3.x). For using `this`, however, one doesn’t need to use any argument:

    .on("mouseover", function(){
        d3.select(this);
    });

The above code will select `this` when the mouse is over the element. Check it working in this fiddle: https://jsfiddle.net/y5fwgopx/

<h1>The arrow function</h1>

As a new ES6 syntax, an arrow function has a shorter syntax when compared to function expression. However, for a D3 programmer who uses `this` constantly, there is a pitfall: an arrow function doesn’t create its own `this` context. That means that, in an arrow function, `this` has its original meaning from the enclosing context. 

This can be useful in several circumstances, but it is a problem for a coder accustomed to use `this` in D3. For instance, using the same example in the fiddle above, this will not work:

    .on("mouseover", ()=>{
        d3.select(this);
    });

If you doubt it, here is the fiddle: https://jsfiddle.net/tfxLsv9u/

Well, that’s not a big problem: one can simply use a regular, old fashioned function expression when needed. But what if you want to write all your code using arrow functions? Is it possible to have a code with arrow functions **and** still properly use `this` in D3?

<h1>The second and third arguments combined</h1>

The answer is **yes**, because `this` is the same of `nodes[i]`. The hint is actually present all over the D3 API, when it describes this:

> ...with `this` as the current DOM element (`nodes[i]`)

The explanation is simple: since `nodes` is the current group of elements in the DOM and `i` is the index of each element, `nodes[i]` refer to the current DOM element itself. That is, `this`.

Therefore, one can use:

    .on("mouseover", (d, i, nodes) => {
        d3.select(nodes[i]);
    });

And here is the corresponding fiddle: https://jsfiddle.net/2p2ux38s/

## Different selectors
You can select elements with different selectors :
- by tag : `"div"`
- by class : `".class"`
- by id : `"#id"`
- by attribute : `"[color=blue]"`
- multiple selectors (OR): `"div1, div2, class1"`
- multiple selectors (AND): `"div1 div2 class1"`

## Basic selection and modifications
If you are familiar with jQuery and Sizzle syntax, d3 selections should not be much different. d3 mimics the W3C Selectors API to make interacting with elements easier. 

For a basic example, to select all `<p>` and add a change to each of them:

```
d3.selectAll('p')
  .attr('class','textClass') 
  .style('color', 'white');
```

In a nutshell this is relatively the same as doing in jQuery

```
$('p')
  .attr('class','textClass') 
  .css('color, 'white')
```

Generally you will start with a single select to your container div to add an SVG element which will be assigned to a variable (most commonly called svg).
```
var svg = d3.select('#divID').append('svg');
```
From here we can call on `svg` to do our sub-selections of multiple objects (even if they don't yet exist).

```
svg.selectAll('path')
```

## Simple data bounded selection


