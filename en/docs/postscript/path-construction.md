---
title: "Path Construction"
slug: "path-construction"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Drawing (describing) a polygon
This example attempts to mimic the behavior of the built-in path construction operators like `arc`. 

If there is a current point, `poly` first draws a line to (x,y)+(r,0), otherwise it starts by moving to that point.

Instead of `gsave` ... `grestore` (which has the undesirable effect of discarding the very changes to the current path which we want), it saves a copy of the current transformation matrix (CTM) as it exists when the function starts.

Then it does `lineto` to each succeeding point, which by scaling and rotating the matrix is always at (0,1). Finally, it calls `closepath` and then restores the saved matrix as the CTM.

    % x y n radius  poly  -
    % construct a path of a closed n-polygon
    /poly {
        matrix currentmatrix 5 1 roll  % matrix x y n radius
        4 2 roll translate             % matrix n radius
        dup scale                      % matrix n
        360 1 index div exch           % matrix 360/n n
        0 1 {lineto currentpoint moveto}stopped{moveto}if   % start or re-start subpath
        {                              % matrix 360/n
            dup rotate                 % matrix 360/n
            0 1 lineto                 % matrix 360/n
        } repeat                 % matrix 360/n
        pop                      % matrix
        closepath                % matrix
        setmatrix                %
    } def



## Iterating through a path
This snippet dumps the contents of the current path to stdout. It uses the ghostscript procedure `=only` which may not be available on all interpreters. An equivalent procedure on Adobe interpreters is called `=print`.

`pathforall` is a looping operator which takes 4 procedure bodies as arguments which are called for the specific types of path elements, the result of `moveto`, `lineto`, `curveto`, `closepath`, and all other path-contruction operators which boil-down to these elements. 

    { exch =only ( ) print =only ( ) print /moveto =} 
    { exch =only ( ) print =only ( ) print /lineto =}
    { 6 -2 roll exch =only ( ) print =only ( ) print
      4 2 roll exch =only ( ) print =only ( ) print
      exch =only ( ) print =only ( ) print /curveto =}
    { /closepath = }
    pathforall

## Graph Paper
    /in {72 mul} def
    /delta {1 in 10 div} def
    /X 612 def
    /Y 792 def
    0 delta Y {
        0 1 index X exch  % i 0 X i
        moveto exch       % 0 i
        lineto
        stroke
    } for
    0 delta X {
        0 1 index Y  % i 0 i Y
        moveto       % i 0
        lineto
        stroke
    } for
    showpage



