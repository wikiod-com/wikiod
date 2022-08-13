---
title: "Integration"
slug: "integration"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Integral, integral2, integral3
**1 dimensional**

To integrate a one dimensional function

    f = @(x) sin(x).^3 + 1;
within the range

    xmin = 2;
    xmax = 8;
one can call the function

    q = integral(f,xmin,xmax);
it's also possible to set boundarys for relative and absolute errors

    q = integral(f,xmin,xmax, 'RelTol',10e-6, 'AbsTol',10-4);

**2 dimensional**

If one wants to integrate a two dimensional function

    f = @(x,y) sin(x).^y ;
within the range

    xmin = 2;
    xmax = 8;
    ymin = 1;
    ymax = 4;

one calls the function

    q = integral2(f,xmin,xmax,ymin,ymax);
Like in the other case it's possible to limit the tolerances

    q = integral2(f,xmin,xmax,ymin,ymax, 'RelTol',10e-6, 'AbsTol',10-4);

**3 dimensional**

Integrating a three dimensional function

    f = @(x,y,z) sin(x).^y - cos(z) ;
within the range

    xmin = 2;
    xmax = 8;
    ymin = 1;
    ymax = 4;
    zmin = 6;
    zmax = 13;

is performed by calling

    q = integral3(f,xmin,xmax,ymin,ymax, zmin, zmax);
Again it's possible to limit the tolerances

    q = integral3(f,xmin,xmax,ymin,ymax, zmin, zmax, 'RelTol',10e-6, 'AbsTol',10-4);



