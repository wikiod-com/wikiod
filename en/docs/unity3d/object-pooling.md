---
title: "Object Pooling"
slug: "object-pooling"
draft: false
images: []
weight: 9896
type: docs
toc: true
---

## Object Pool
Sometimes when you make a game you need to create and destroy a lot of objects of the same type over and over again. You can simply do this by making a prefab and instantiate/destroy this whenever you need to, however, doing this is inefficient and can slow your game down.

One way to get around this issue is object pooling. Basically what this means is that you have a pool (with or without a limit to the amount) of objects that you are going to reuse whenever you can to prevent unnecessary instantiating or destroying.

Below is an example of a simple object pool
<!-- language-all: c# -->

    public class ObjectPool : MonoBehaviour 
    {
        public GameObject prefab;
        public int amount = 0;
        public bool populateOnStart = true;
        public bool growOverAmount = true;

        private List<GameObject> pool = new List<GameObject>();

        void Start() 
        {
            if (populateOnStart && prefab != null && amount > 0) 
            {
                for (int i = 0; i < amount; i++) 
                {
                    var instance = Instantiate(Prefab);
                    instance.SetActive(false);
                    pool.Add(instance);
                }
            }
        }

        public GameObject Instantiate (Vector3 position, Quaternion rotation) 
        {
            foreach (var item in pool) 
            {
                if (!item.activeInHierarchy) 
                {
                    item.transform.position = position;
                    item.transform.rotation = rotation;
                    item.SetActive( true );
                    return item;
                }
            }

            if (growOverAmount) 
            {
                var instance = (GameObject)Instantiate(prefab, position, rotation);
                pool.Add(instance);
                return instance;
            }

            return null;
        }
    }

Let's go over the variables first

    public GameObject prefab;
    public int amount = 0;
    public bool populateOnStart = true;
    public bool growOverAmount = true;

    private List<GameObject> pool = new List<GameObject>();

- `GameObject prefab`: this is the prefab that the object pool will use to instantiate new objects into the pool.
- `int amount`: This is the maximum amount of items that can be in the pool. If you want to instantiate another item and the pool has already reached its limit then another item from the pool will be used.
- `bool populateOnStart`: you can choose to populate the pool on start or not. Doing so will fill up the pool with instances of the prefab so that the first time you call `Instantiate` you will get an already existing object
- `bool growOverAmount`: Setting this to true allows the pool to grow whenever more than the amount are requested in a certain timeframe. You are not always able to accurately predict the amount of items to put in your pool so this will add more to your pool when needed.
- `List<GameObject> pool`: this is the pool, the place where all your instantiated/destroyed objects are stored.

Now let's check out the `Start` function

    void Start() 
    {
        if (populateOnStart && prefab != null && amount > 0) 
        {
            for (int i = 0; i < amount; i++) 
            {
                var instance = Instantiate(Prefab);
                instance.SetActive(false);
                pool.Add(instance);
            }
        }
    }

In the start function we check if we should populate the list on start and do so if the `prefab` has been set and the amount is bigger than 0 (otherwise we would be creating indefinitely).

This is just a simple for loop instantiating new objects and putting them in the pool. One thing to pay attention to is that we set all the instances to inactive. This way they are not visible in the game yet.

Next, there is the `Instantiate` function, which is where most of the magic happens

    public GameObject Instantiate (Vector3 position, Quaternion rotation) 
    {
        foreach (var item in pool) 
        {
            if (!item.activeInHierarchy) 
            {
                item.transform.position = position;
                item.transform.rotation = rotation;
                item.SetActive(true);
                return item;
            }
        }

        if (growOverAmount) 
        {
            var instance = (GameObject)Instantiate(prefab, position, rotation);
            pool.Add(instance);
            return instance;
        }

        return null;
    }

The `Instantiate` function looks just like Unity's own `Instantiate` function, except the prefab has already been provided above as a class member.

The first step of the `Instantiate` function is checking to see if there is an inactive object in the pool right now. This means that we can reuse that object and give it back to the requester. If there is an inactive object in the pool we set the position and the rotation, set it to be active (otherwise it could be reused by accident if you forget to activate it) and return it to the requester.

The second step only happens if there are no inactive items in the pool and the pool is allowed to grow over the initial amount. What happens is simple: another instance of the prefab is created and added to the pool. Allowing growth of the pool helps you in having the right amount of objects in the pool.

The third "step" only happens if there are no inactive items in the pool and the pool is *not* allowed to grow. When this happens the requester will receive a null GameObject which means that nothing was available and should be handled properly to prevent `NullReferenceExceptions`.

**Important!**

To make sure your items get back into the pool you should **not** destroy the game objects. The only thing you need to do is set them to inactive and that will make them available for reusage through the pool.

## Simple object pool
Below is an example of an object pool that allows renting and returning of a given object type. To create the object pool a Func<T> for the create function and an Action<T> to destroy the object are required to give the user flexibility. On requesting an object when the pool is empty a new object will be created and on requesting when the pool has objects then objects are removed from the pool and returned.

**Object Pool**

<!-- language-all: c# -->

    public class ResourcePool<T> where T : class
    {
        private readonly List<T> objectPool = new List<T>();
        private readonly Action<T> cleanUpAction;
        private readonly Func<T> createAction;
    
        public ResourcePool(Action<T> cleanUpAction, Func<T> createAction)
        {
            this.cleanUpAction = cleanUpAction;
            this.createAction = createAction;
        }
    
        public void Return(T resource)
        {
            this.objectPool.Add(resource);
        }
    
        private void PurgeSingleResource()
        {
            var resource = this.Rent();
            this.cleanUpAction(resource);
        }
    
        public void TrimResourcesBy(int count)
        {
            count = Math.Min(count, this.objectPool.Count);
            for (int i = 0; i < count; i++)
            {
                this.PurgeSingleResource();
            }
        }
    
        public T Rent()
        {
            int count = this.objectPool.Count;
            if (count == 0)
            {
                Debug.Log("Creating new object.");
                return this.createAction();
            }
            else
            {
                Debug.Log("Retrieving existing object.");
                T resource = this.objectPool[count-1];
                this.objectPool.RemoveAt(count-1);
                return resource;
            }
        }
    }

**Sample usage**

    public class Test : MonoBehaviour
    {
        private ResourcePool<GameObject> objectPool;
    
        [SerializeField]
        private GameObject enemyPrefab;
    
        void Start()
        {
            this.objectPool = new ResourcePool<GameObject>(Destroy,() => Instantiate(this.enemyPrefab) );
        }
    
        void Update()
        {
            // To get existing object or create new from pool
            var newEnemy = this.objectPool.Rent();
            // To return object to pool
            this.objectPool.Return(newEnemy);
            // In this example the message 'Creating new object' should only be seen on the frame call
            // after that the same object in the pool will be returned.
        }
    }

## Another simple object pool
Another example: a Weapon that shoots out Bullets.

The Weapon acts as an object pool for the Bullets it creates.

<!-- language-all: c# -->

    public class Weapon : MonoBehaviour {
        
        // The Bullet prefab that the Weapon will create
        public Bullet bulletPrefab;

        // This List is our object pool, which starts out empty
        private List<Bullet> availableBullets = new List<Bullet>();

        // The Transform that will act as the Bullet starting position
        public Transform bulletInstantiationPoint;

        // To spawn a new Bullet, this method either grabs an available Bullet from the pool,
        // otherwise Instantiates a new Bullet
        public Bullet CreateBullet () {
            Bullet newBullet = null;

            // If a Bullet is available in the pool, take the first one and make it active
            if (availableBullets.Count > 0) {
                newBullet = availableBullets[availableBullets.Count - 1];

                // Remove the Bullet from the pool
                availableBullets.RemoveAt(availableBullets.Count - 1);

                // Set the Bullet's position and make its GameObject active
                newBullet.transform.position = bulletInstantiationPoint.position;
                newBullet.gameObject.SetActive(true);
            }
            // If no Bullets are available in the pool, Instantiate a new Bullet
            else {
                newBullet newObject = Instantiate(bulletPrefab, bulletInstantiationPoint.position, Quaternion.identity);

                // Set the Bullet's Weapon so we know which pool to return to later on
                newBullet.weapon = this;
            }
            
            return newBullet;
        }

    }

    public class Bullet : MonoBehaviour {
        
        public Weapon weapon;

        // When Bullet collides with something, rather than Destroying it, we return it to the pool
        public void ReturnToPool () {
            // Add Bullet to the pool
            weapon.availableBullets.Add(this);

            // Disable the Bullet's GameObject so it's hidden from view
            gameObject.SetActive(false);
        }

    }

