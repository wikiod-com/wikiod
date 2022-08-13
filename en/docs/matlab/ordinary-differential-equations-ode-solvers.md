---
title: "Ordinary Differential Equations (ODE) Solvers"
slug: "ordinary-differential-equations-ode-solvers"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Example for odeset
First we initialize our initial value problem we want to solve. 

    odefun = @(t,y) cos(y).^2*sin(t);
    tspan = [0 16*pi];
    y0=1;

We then use the ode45 function without any specified options to solve this problem. To compare it later we plot the trajectory.

    [t,y] = ode45(odefun, tspan, y0);
    plot(t,y,'-o');

We now set a narrow relative and a narrow absolut limit of tolerance for our problem.

    options = odeset('RelTol',1e-2,'AbsTol',1e-2);
    [t,y] = ode45(odefun, tspan, y0, options);
    plot(t,y,'-o');


We set tight relative and narrow absolut limit of tolerance.

    options = odeset('RelTol',1e-7,'AbsTol',1e-2);
    [t,y] = ode45(odefun, tspan, y0, options);
    plot(t,y,'-o');



We set narrow relative and tight absolut limit of tolerance. As in the previous examples with narrow limits of tolerance one sees the trajectory being completly different from the first plot without any specific options.

    options = odeset('RelTol',1e-2,'AbsTol',1e-7);
    [t,y] = ode45(odefun, tspan, y0, options);
    plot(t,y,'-o');



We set tight relative and tight absolut limit of tolerance. Comparing the result with the other plot will underline the errors made calculating with narrow tolerance limits.

    options = odeset('RelTol',1e-7,'AbsTol',1e-7);
    [t,y] = ode45(odefun, tspan, y0, options);
    plot(t,y,'-o');









The following should demonstrate the trade-off between precision and run-time. 

    tic;
    options = odeset('RelTol',1e-7,'AbsTol',1e-7);
    [t,y] = ode45(odefun, tspan, y0, options);
    time1 = toc;
    plot(t,y,'-o');
For comparison we tighten the limit of tolerance for absolute and relative error. We now can see that without large gain in precision it will take considerably longer to solve our initial value problem.

    tic;
    options = odeset('RelTol',1e-13,'AbsTol',1e-13);
    [t,y] = ode45(odefun, tspan, y0, options);
    time2 = toc;
    plot(t,y,'-o');


