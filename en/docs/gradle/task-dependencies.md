---
title: "Task dependencies"
slug: "task-dependencies"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

**doLast**

Note, that in a gradle 3.x more idiomatic way task definition: using **explicit** **doLast{closure}** notation instead "leftShift"(<<) operator preferable.(**leftShift** has been deprecated in a gradle 3.2 is scheduled to be removed in gradle 5.0.)

    task oldStyle << {
        println 'Deprecated style task'
     }

is equivalent to:

    task newStyle {
        doLast {
        println 'Deprecated style task'
        }
     }

## Adding dependencies using task names
We can change the tasks execution order with the `dependsOn` method.

    task A << {
        println 'Hello from A'
    }
    task B(dependsOn: A) << {
        println "Hello from B"
    }

Adding `dependsOn: causes:
- task B depends on task A
- Gradle to execute `A` task everytime **before** the `B` task execution.

And the output is:

    > gradle -q B
    Hello from A
    Hello from B

## Adding dependencies from another project
    project('projectA') {
        task A(dependsOn: ':projectB:B') << {
            println 'Hello from A'
        }
    }
    
    project('projectB') {
        task B << {
            println 'Hello from B'
        }
    }
To refer to a task in another project, you **prefix the name of the task** with the path of the project it belongs to `:projectB:B`. 

And the output is:

    > gradle -q B
    Hello from A
    Hello from B

## Adding dependency using task object
    task A << {
        println 'Hello from A'
    }
    
    task B << {
        println 'Hello from B'
    }
    
    B.dependsOn A

It is an alternative way to define the dependency instead of using the [task name][1].

And the output is the same:

    > gradle -q B
    Hello from A
    Hello from B


  [1]: https://www.wikiod.com/gradle/task-dependencies#Adding dependencies using task names

## Adding multiple dependencies
You can add multiple dependencies.

    task A << {
        println 'Hello from A'
    }
    
    task B << {
        println 'Hello from B'
    }
    
    task C << {
        println 'Hello from C'
    }
    
    task D << {
        println 'Hello from D'
    }

Now you can define a set of dependencies:

    B.dependsOn A
    C.dependsOn B
    D.dependsOn C

The output is:

    > gradle -q D
    Hello from A
    Hello from B
    Hello from C
    Hello from D

Other example:

    B.dependsOn A 
    D.dependsOn B
    D.dependsOn C

The output is:

    > gradle -q D
    Hello from A
    Hello from B
    Hello from C
    Hello from D




## Multiple dependencies with the dependsOn method
You can add multiple dependencies.

    task A << {
        println 'Hello from A'
    }
    
    task B(dependsOn: A) << {
        println 'Hello from B'
    }
    
    task C << {
        println 'Hello from C'
    }
    
    task D(dependsOn: ['B', 'C'] << {
        println 'Hello from D'
    }

The output is:

    > gradle -q D
    Hello from A
    Hello from B
    Hello from C
    Hello from D



