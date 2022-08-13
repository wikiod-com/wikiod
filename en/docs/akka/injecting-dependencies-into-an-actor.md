---
title: "Injecting dependencies into an actor"
slug: "injecting-dependencies-into-an-actor"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Spring-wired actor
Due to very specific way of actor instantiation, injecting dependencies into an actor instance is not trivial. In order to intervene in actor instantiation and allow Spring to inject dependencies one should implement a couple of akka extensions. First of those is an `IndirectActorProducer`:

    import akka.actor.Actor;
    import akka.actor.IndirectActorProducer;
    import java.lang.reflect.Constructor;
    import java.util.Arrays;
    import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
    import org.springframework.context.ApplicationContext;
    
    /**
     * An actor producer that lets Spring autowire dependencies into created actors.
     */
    public class SpringWiredActorProducer implements IndirectActorProducer {
    
        private final ApplicationContext applicationContext;
        private final Class<? extends Actor> actorBeanClass;
        private final Object[] args;
    
        public SpringWiredActorProducer(ApplicationContext applicationContext, Class<? extends Actor> actorBeanClass, Object... args) {
            this.applicationContext = applicationContext;
            this.actorBeanClass = actorBeanClass;
            this.args = args;
        }
    
        @Override
        public Actor produce() {
            Class[] argsTypes = new Class[args.length];
            for (int i = 0; i < args.length; i++) {
                if (args[i] == null) {
                    argsTypes[i] = null;
                } else {
                    argsTypes[i] = args[i].getClass();
                }
            }
            Actor result = null;
            try {
                if (args.length == 0) {
                    result = (Actor) actorBeanClass.newInstance();
                } else {
                    try {
                        result = (Actor) actorBeanClass.getConstructor(argsTypes).newInstance(args);
                    } catch (NoSuchMethodException ex) {
                        // if types of constructor don't match exactly, try to find appropriate constructor
                        for (Constructor<?> c : actorBeanClass.getConstructors()) {
                            if (c.getParameterCount() == args.length) {
                                boolean match = true;
                                for (int i = 0; match && i < argsTypes.length; i++) {
                                    if (argsTypes[i] != null) {
                                        match = c.getParameters()[i].getType().isAssignableFrom(argsTypes[i]);
                                    }
                                }
                                if (match) {
                                    result = (Actor) c.newInstance(args);
                                    break;
                                }
                            }
                        }
                    }
                }
                if (result == null) {
                    throw new RuntimeException(String.format("Cannot find appropriate constructor for %s and types (%s)", actorBeanClass.getName(), Arrays.toString(argsTypes)));
                } else {
                    applicationContext.getAutowireCapableBeanFactory().autowireBeanProperties(result, AutowireCapableBeanFactory.AUTOWIRE_BY_TYPE, true);
                }
            } catch (ReflectiveOperationException e) {
                throw new RuntimeException("Cannot instantiate an action of class " + actorBeanClass.getName(), e);
            }
            return result;
        }
    
        @Override
        public Class<? extends Actor> actorClass() {
            return (Class<? extends Actor>) actorBeanClass;
        }
    
    }

This producer instantiates an actor and injects dependencies before returning the actor instance.

We can prepare `Props` for creation an actor using the `SpringWiredActorProducer` the following way:

    Props.create(SpringWiredActorProducer.class, applicationContext, actorBeanClass, args);

However it would be better to wrap that call into following spring bean:

    import akka.actor.Extension;
    import akka.actor.Props;
    import org.springframework.beans.BeansException;
    import org.springframework.context.ApplicationContext;
    import org.springframework.context.ApplicationContextAware;
    import org.springframework.stereotype.Component;
    
    /**
     * An Akka Extension to inject dependencies to {@link akka.actor.Actor}s with
     * Spring.
     */
    @Component
    public class SpringProps implements Extension, ApplicationContextAware {
    
        private volatile ApplicationContext applicationContext;
    
        /**
         * Creates a Props for the specified actorBeanName using the
         * {@link SpringWiredActorProducer}.
         *
         * @param actorBeanClass The class of the actor bean to create Props for
         * @param args arguments of the actor's constructor
         * @return a Props that will create the named actor bean using Spring
         */
        public Props create(Class actorBeanClass, Object... args) {
            return Props.create(SpringWiredActorProducer.class, applicationContext, actorBeanClass, args);
        }
    
        @Override
        public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
            this.applicationContext = applicationContext;
        }
    
    }

You can autowire `SpringProps` anywhere an actor gets created (even in the actor itself) and create spring-wired actors the following way:

    @Autowired
    private SpringProps springProps;
    //...
    actorSystem.actorOf(springProps.create(ActorClass.class), actorName);
    //or inside an actor
    context().actorOf(springProps.create(ActorClass.class), actorName);

Assuming that `ActorClass` extends `UntypedActor` and has properties annotated with `@Autowired`, those dependencies will be injected right after instantiation.

