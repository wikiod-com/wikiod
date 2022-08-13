---
title: "Custom Test Rules"
slug: "custom-test-rules"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

There are benefits for either. Extending `ExternalResource` it's convenient, especially if we only require a `before()` to set something up. 

However, we should be aware that, since the `before()` method is executed outside of the `try...finally`, any code that is required to do clean up in `after()` won't get executed if there is an error during the execution of `before()`.

This is how it looks inside `ExternalResource`:

    before();
    try {
        base.evaluate();
    } finally {
        after();
    }

Obviously, if any exception is thrown in the test itself, or by another nested rule, the after will still get executed.

## Custom @TestRule by implementation
This is especially useful if we have a class that we want to extend in the rule. See example below for a more convenient method.
``` java
import org.junit.rules.TestRule;
import org.junit.runners.model.Statement;

public class AwesomeTestRule implements TextRule {
    
    @Override
    public Statement apply(Statement base, Description description) {
        return new AwesomeStatement(base);
    }

    private static class AwesomeStatement extends Statement {
        
        private Statement base;
        
        public AwesomeStatement(Statement base) {
            this.base = base;
        }

        @Override
        public void evaluate() throws Throwable {
            try {
                // do your magic
                base.evaluate(); // this will call Junit to run an individual test
            } finally {
                // undo the magic, if required
            }
        }
    }
    
}
```

## Custom @TestRule by extension
JUnit has an abstract implementation of `@TestRule` that lets you write a rule in a more simpler way. This is called `ExternalResource` and provides two protected methods that can be extended like this:

``` java

public class AwesomeTestRule extends ExternalResource {

    @Override
    protected void before() {
        // do your magic
    }

    @Override
    protected void after() {
        // undo your magic, if needed
    }

}

```

