---
title: "Guice method interceptor"
slug: "guice-method-interceptor"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Simple example
Intercepted method:

    public class ExampleService implements Example {
        @MyAnnotation
        public doHomework() {
            System.out.println("working");
        }
    }

Annotation:
    
        @Retention(RetentionPolicy.RUNTIME) @Target(ElementType.METHOD)
        public @interface MyAnnotation {}

Interceptor:

    public class MyInterceptor implements MethodInterceptor {

        @Override
        public Object invoke(MethodInvocation arg0) throws Throwable {
            System.out.println("***** intercepted *****");
            return arg0.proceed();
        }
    }

Module:

    public class MyModule extends AbstractModule {


        @Override
        protected void configure() {
            bind(CallerService.class).to(Caller.class);
            bind(ExampleService.class).to(Example.class);

            bindInterceptor(Matchers.any(), Matchers.annotatedWith(MyAnnotation.class), new MyInterceptor());
        }
    }

Caller:

    public class CallerService implements Caller {
        @Inject
        private Client client;

        public void call() {
            client.doHomework();
        }
    }

Output:
    
    ***** intercepted *****
    working


