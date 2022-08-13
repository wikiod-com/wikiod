---
title: "Ashley Entity System"
slug: "ashley-entity-system"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

[Ashley Entity System][1] is an Entity System library that's managed under the [LibGDX organization][2] and is and well-suited for game development.  It depends on LibGDX utility classes, but can be used with other Java game frameworks not based on LibGDX with some work.

Entity systems provide a different way to manage data and functionality towards large sets of objects without having to make the object classes rich with inheritance.

Utilizing Ashley might be a helpful approach for those looking for an object modeling approach like Unity provides, but with the scope of a framework instead of game engine.


  [1]: https://github.com/libgdx/ashley
  [2]: https://github.com/libgdx

## Creating a Component
Components are simply instances that implement the Ashley component class.

```
import com.badlogic.ashley.core.Component;
import com.badlogic.ashley.core.ComponentMapper;

public class Position implements Component {
    public static final ComponentMapper<Position> Map = 
                                 ComponentMapper.getFor(Position.class);

    public float x = 0f, 
                 y = 0f;
}
```

Component maps provide a fast way to access components on entities.  Two common ways to manage your component maps is to either keep a static instance within your component's class, or to have a class/enum that contains all the mappers for all your components.  

There's no need to declare a mapper for a component type more than once in your application.

## Creating an Entity System
Entity systems are how you perform functional operations on sets of entities.  Components should typically not have logic associated with them that involves awareness of data or state of other component instances, as that is the job of an entity system.  An entity system can not be registered to more than one engine at a time.

Entity systems should not perform more than one type of function.  A MovementSystem should only handle positioning and the like, while something like a RenderSystem should handle the drawing of entities.

```
import com.badlogic.ashley.core.Entity;
import com.badlogic.ashley.core.EntitySystem;
import com.badlogic.ashley.core.Family;

public class MovementSystem extends EntitySystem {
    //the type of components necessary for entities to have to be operated on
    private static final Family FAMILY = Family.all(Position.class).get();

    public MovementSystem () {
        super();
    }

    /**
     * The update method called every tick.
     * @param deltaTime The time passed since last frame in seconds.
     */
    public void update (float deltaTime) {
        for (Entity e : this.getEngine().getEntitiesFor(FAMILY)) {
            Position pos = Position.Map.get(e);
            
            // do entity movement logic on component
            ...
        }
    }
```

Sometimes it's helpful to extend your EntitySystems with additional functionality like that found in EntityListeners so you keep track of only the types of entities you wish to operate on, instead of iterating over all entities in the engine every cycle.  EntityListeners are triggered whenever an entity is added to the same engine that the system is registered to.

```
import com.badlogic.ashley.core.EntityListener;
import com.badlogic.gdx.utils.Array;

public class MovementSystem extends EntitySystem implements EntityListener {
    Array<Entity> moveables = new Array<>();
    ...

    @Override
    public void entityAdded(Entity entity) {
        if (FAMILY.matches(entity)) {
            moveables.add(entity);
        }
    }

    @Override
    public void entityRemoved(Entity entity) {
        if (FAMILY.matches(entity)) {
            moveables.removeValue(entity, true);
        }
    }

    public void update (float deltaTime) {
        for (Entity e : this.moveables) {
            Position pos = Position.Map.get(e);
            
            // do entity movement logic on component
            ...
        }
    }
}
```

Keeping track of a subset of entities that the system processes at all times can also be optimal, as you can remove a particular entity from that system's processing without having to remove the associating component or removing them from the engine as a whole if so desired.

## Creating a Sorted Entity System
> A simple `EntitySystem` that processes each entity of a given family in the order specified by a `comparator` and calls `processEntity()` for each entity every time the `EntitySystem` is updated. This is really just a convenience class as rendering systems tend to iterate over a list of entities in a sorted manner. Adding entities will cause the entity list to be resorted. Call `forceSort()` if you changed your sorting criteria. For more info, please see [SortedIteratingSystem][1]

In the below code example, the best usage for this the rendering of your sprites in a sorted order by zindex.

    public class SpriteComponent implements Component {
         public TextureRegion region;
         public int z = 0;
    }

    public class Mapper {
         public static ComponentMapper<SpriteComponent> sprite = ComponentMapper.getFor(SpriteComponent.class);
    }

    public class RenderingSystem extends SortedIteratingSystem {

        public RenderingSystem () {
             super(Familly.all(SpriteComponent.class).get(), new ZComparator())
        }

        public void processEntity(Entity entity, float deltaTime) {
             if(checkZIndexHasChangeValue()) {
                  forceSort();
             }
        }

        private static class ZComparator implements Comparator<Entity> {
            @Override
            public int compare(Entity entityA, Entity entityB) {
                return (int)Math.signum(Mapper.sprite.get(entityA).z - Mapper.sprite.get(entityB).z);
            }
        }

    }


  [1]: https://libgdx.badlogicgames.com/ashley/docs/com/badlogic/ashley/systems/SortedIteratingSystem.html

## Creating an Interval Iterating System
> A simple `EntitySystem` that processes a Family of entities not once per frame, but after a given interval. Entity processing logic should be placed in `processEntity(Entity)`. For more info, please see [IntervalIteratingSystem][1]

In the below code example, the best usage for this is the physics [world][2] step. 

    public class Constants {
         public final static float TIME_STEP = 1 / 60.0f; // 60 fps
         public final static int VELOCITY_ITERATIONS = 6;
         public final static int POSITION_ITERATIONS = 2;
    }
    
    public class PhysicsSystem extends IntervalIteratingSystem {
        public PhysicsSystem () {
             super(Family.all(PhysicsComponent.class), Constants.TIME_STEP);
        }

        @Override
        protected void processEntity(Entity entity) {
             // process the physics component here with an interval of 60fps
        }
    
        @Override
        protected void updateInterval() {
            WorldManager.world.step(Constants.TIME_STEP, Constants.VELOCITY_ITERATIONS, Constants.POSITION_ITERATIONS);
            super.updateInterval();
        }
    
    }


  [1]: https://libgdx.badlogicgames.com/ashley/docs/com/badlogic/ashley/systems/IntervalIteratingSystem.html
  [2]: https://libgdx.badlogicgames.com/nightlies/docs/api/com/badlogic/gdx/physics/box2d/World.html

