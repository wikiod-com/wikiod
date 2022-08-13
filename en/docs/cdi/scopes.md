---
title: "Scopes"
slug: "scopes"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

**Why do I need these no-args constructors???**

*What happens if a session scoped bean gets injected into an application scoped bean? How does the application scoped bean get the correct session scoped bean instance for each request? Wouldn't the session scoped bean leak out into other requests? How does that work?* In order to facilitate scoping, CDI uses what is known as a proxy. When CDI injects a non-dependent scoped bean into another object, it does not inject the bean directly. Instead, it subclasses that bean to create what is known as a proxy. Whenever a method is called on the proxy, it asks the CDI runtime to look up the correct bean for that particular scope (if it's request scoped, get the bean for that request. If it's session scoped, get the bean for that session. etc.), and then forwards the call to the real object, returning any result for non-void methods. This means that the following is an okay thing to do:

    @ApplicationScoped
    public class ApplicationScopedClass {

        private final RequestScopedClass requestScopedClass;

        @Inject
        public ApplicationScopedClass(RequestScopedClass requestScopedClass) {
            this.requestScopedClass = requestScopedClass;
        }

        public ApplicationScopedClass() {

        }

        public doSomething() {
            requestScopedClass.doSomethingRequestSpecific(); //This works, because of the proxy
        }

    }

However, the in order for a class to legally subclass another class in Java, it must have a valid constructor. And a valid constructor must call either another constructor on the class, or the parent class' constructor (e.g. `super()`). In order to simplify this subclassing, the CDI spec requires any non-dependent scoped bean to provide a public no-args constructor, so that the runtime doesn't have to try to guess at what to do with a class which needs to be proxied. 

## The default scope
    public class DependentScopedClass {

        //This class has no scoping annotations, so a new instance gets created at every injection point.

        @Inject
        public DependentScopedClass(SomeDependency someDependency) {
            doSomethingWith(someDependency);
        }

    }

The default scope for most CDI beans is referred to as dependent scope. A class that does not contain any scope annotation will be treated as dependent scope, **unless** it is a JAX-RS resource or provider (resources default to request scoped, and providers default to application scoped). An instance of a dependent scope class lives as long as the object it is injected into does. Any time a class gets constructed and has a dependent scope class as a dependency, the dependent scoped class gets created and injected *directly* into the object which needs it. The significance of this will become apparent after later examples. 

## @RequestScoped
    @RequestScoped
    public class RequestScopedClass {
        //This class gets constructed once per Servlet request, and is shared among all CDI-managed classes within that request.

        @Inject
        public RequestScopedClass(SomeDependency someDependency) {
            doSomethingWith(someDependency);
        }

        public RequestScopedClass() {
            //Note that it is required that a request scoped class have a public no-args constructor
        }

    }

If a bean is annotated with @RequestScoped, it will be created once for any request. If two objects depend on a request scoped class, they will both get references to the same object.

**Note:** Any request scoped bean must have a public no-args constructor. The reason for this will be explained later on.

## @ApplicationScoped
    @ApplicationScoped
    public class ApplicationScopedClass {
        //This class gets constructed once for the entire life of the application, and is shared among all CDI-managed classes throughout the life of the application.

        @Inject
        public ApplicationScopedClass(SomeDependency someDependency) {
            doSomethingWith(someDependency);
        }

        public ApplicationScopedClass() {
            //Note that it is required that an application scoped class have a public no-args constructor
        }

    }

Classes with @ApplicationScoped are created only once, and each object which depends on the class share the same instance. These classes are 'effectively' singletons, however it should be noted that there is nothing to prevent a coder from manually creating additional instances of the class. Thus, using @ApplicationScoped is useful for sharing context across the entire application, or as a performance optimization (if the class is expensive to construct instances of), but it should not be relied upon as an integrity measure for guaranteeing only one instance of a class exists.

Like request scoped beans, application scoped beans need to have a public no-args constructor.

## @SessionScoped
    @SessionScoped
    public class SessionScopedClass implements Serializable {
        //This class gets constructed once per session, and is shared among all CDI-managed classes within that session. Notice that it implements Serializable, since the instance will get put on the session.

        @Inject
        public SessionScopedClass(SomeDependency someDependency) {
            doSomethingWith(someDependency);
        }

        public SessionScopedClass() {
            //Note that it is required that a session scoped class have a public no-args constructor
        }

    }

Classes annotated with @SessionScoped will be created once per session, and two objects within the same session will share the same instance of the session scoped class.

It is important to notice that the session scoped class should implement Serializable. This requirement exists because the bean will be stored in the servlet container's session for that particular session scoped instance. In general, any object being put into the session needs to be serializable for two reasons: First, sessions may be persisted to disk after a certain amount of inactivity to save memory. This is know as session passivation, and it requires serialization. The second reason is that in high-availability clusters, session replication is often used in order to allow any server in the cluster to service a request for a given session. This also generally requires serialization.

Much like request scoped and application scoped, session scoped classes must have a public no-args constructor. 

