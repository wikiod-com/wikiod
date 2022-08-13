---
title: "Dependency Injection"
slug: "dependency-injection"
draft: false
images: []
weight: 9999
type: docs
toc: true
---

CDI's flagship feature is a declarative API for dependency injection. Classes can have dependencies flagged with the `@Inject` annotation, which will indicate to the CDI manager that it needs to provide those dependencies when constructing an instance of the class.

## Constructor Injection
The common case for injecting dependencies into a class is with constructor injection. This involves annotating a constructor on the class with @Inject. The CDI manager will look for a constructor with the @Inject annotation when creating an instance of the class. When it finds an @Inject-annotated constructor, it will use reflection to find which parameters are required by the constructor, construct or obtain instances of those dependencies, then call the constructor with those dependencies.

    public class Spaceship {

        private final PropulsionSystem propulsionSystem;
        private final NavigationSystem navigationSystem;

        @Inject
        public Spaceship(PropulsionSystem propulsionSystem, NavigationSystem navigationSystem) {
            this.propulsionSystem = propulsionSystem;
            this.navigationSystem = navigationSystem;
        }

        public void launch() throws FlightUnavailableException {
            if (propulsionSystem.hasFuel()) {
                propulsionSystem.engageThrust();
            } else {
                throw new FlightUnavailableException("Launch requirements not met. Ship needs fuel.");
            }
        }

    }

Any time a Spaceship instance defined in this manner is created by CDI, it will receive a PropulsionSystem and a NavigationSystem as arguments to its constructor. Because these dependencies are added via the constructor, we have an easy way to provide alternate dependencies in our test harness when unit testing:

    public class SpaceshipTest {

        private Spaceship systemUnderTest;
        private TestPropulsionSystem testPropulsionSystem;
        private TestNavigationSystem testNavigationSystem;

        @Before
        public void setup() {
            setupCollaborators();
            systemUnderTest = new Spaceship(testPropulsionSystem, testNavigationSystem);
        }

        @Test
        public void launchSequenceEngagesThrustIfFueled() {
            //given
            testPropulsionSystem.simulateHavingFuel();

            //when
            systemUnderTest.launch();

            //then
            testPropulsionSystem.ensureThrustWasEngaged();
        }
    
    }

## Field Injection
The same example from above can also be done using what is known as field injection. Instead of annotating the constructor with `@Inject`, we annotate the fields we wish to have injected

    public class Spaceship {
    
        @Inject
        private PropulsionSystem propulsionSystem;
        @Inject
        private NavigationSystem navigationSystem;
    
        public void launch() throws FlightUnavailableException {
            if (propulsionSystem.hasFuel()) {
                propulsionSystem.engageThrust();
            } else {
                throw new FlightUnavailableException("Launch requirements not met. Ship needs fuel.");
            }
        }

    }

Note that getter/setter methods are not required for field injection to work. Also note that the field does not need to be public and can in fact be private, though it cannot be final. Making the field private will, however, make writing tests for the code more difficult, as the tests will have to use reflection to modify the field if it does not have a setter.

Also note that constructor injection and field injection can be used in tandem if desired, though it should be very carefully evaluated whether or not it makes sense to do so on a case by case basis. Perhaps it is necessary when working with legacy code to add a dependency to the class without modifying the constructor signature for some peculiar reason.

    public class Spaceship {
    
        private PropulsionSystem propulsionSystem;
        @Inject
        private NavigationSystem navigationSystem;
    
        @Inject
        public Spaceship(PropulsionSystem propulsionSystem) {
            this.propulsionSystem = propulsionSystem;
        }

        public void launch() throws FlightUnavailableException {
            if (propulsionSystem.hasFuel()) {
                propulsionSystem.engageThrust();
            } else {
                throw new FlightUnavailableException("Launch requirements not met. Ship needs fuel.");
            }
        }
    
    }


