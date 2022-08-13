---
title: "Programming Utilities"
slug: "programming-utilities"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Simple timer in MATLAB
The following is a timer that fires at a fixed interval. It's timeout is defined by `Period` and it invokes a callback defined by `Timerfcn` upon timeout.

    t = timer;
    t.TasksToExecute = Inf;
    t.Period = 0.01; % timeout value in seconds
    t.TimerFcn = @(myTimerObj, thisEvent)disp('hello'); % timer callback function
    t.ExecutionMode = 'fixedRate';
    start(t)
    pause(inf);



