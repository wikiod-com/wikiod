---
title: "Ordering tasks"
slug: "ordering-tasks"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

> Please note that `mustRunAfter` and `shouldRunAfter` are marked as "incubating" (as of Gradle 3.0) which means that these are experimental features and their behavior can be changed in future releases.

There are two ordering rules available: 
- `mustRunAfter` 
- `shouldRunAfter`

When you use the `mustRunAfter` ordering rule you specify that taskB must always run after taskA, whenever both taskA and taskB will be run. 

The `shouldRunAfter` ordering rule is similar but less strict as it will be ignored in two situations:
- if using that rule introduces an ordering cycle. 
- when using parallel execution and all dependencies of a task have been satisfied apart from the `shouldRunAfter` task, then this task will be run regardless of whether its `shouldRunAfter` dependencies have been run or not.

## Ordering with the mustRunAfter method
    task A << {
        println 'Hello from A'
    }
    task B << {
        println 'Hello from B'
    }
    
    B.mustRunAfter A

The `B.mustRunAfter A` line tells Gradle to run task after task specified as an argument. 

And the output is:

    > gradle -q B A
    Hello from A
    Hello from B


The ordering rule doesn't introduce [dependency][1] between the A and the B tasks, but  has an effect only when **both tasks are scheduled** for execution.

It means that we can execute tasks A and B independently.

The output is:

    > gradle -q B 
    Hello from B

  [1]: https://www.wikiod.com/gradle/task-dependencies

