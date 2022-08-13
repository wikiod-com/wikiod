---
title: "TreeMap and TreeSet"
slug: "treemap-and-treeset"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

`TreeMap` and `TreeSet` are basic Java collections added in Java 1.2. `TreeMap` is a **mutable**, **ordered**, `Map` implementation. Similarly, `TreeSet` is a **mutable**, **ordered** `Set` implementation. 

`TreeMap` is implemented as a Red-Black tree, which provides `O(log n)` access times. 
`TreeSet` is implemented using a `TreeMap` with dummy values.

Both collections are **not** thread-safe.

## TreeMap of a simple Java type
First, we create an empty map, and insert some elements into it: 

<!-- if version [gte Java SE 7] --> 
    TreeMap<Integer, String> treeMap = new TreeMap<>();
<!-- end version if -->
<!-- if version [lt Java SE 7] --> 
    TreeMap<Integer, String> treeMap = new TreeMap<Integer, String>();
<!-- end version if -->
        
    treeMap.put(10, "ten");
    treeMap.put(4, "four");
    treeMap.put(1, "one");
    treeSet.put(12, "twelve");

Once we have a few elements in the map, we can perform some operations:

    System.out.println(treeMap.firstEntry()); // Prints 1=one
    System.out.println(treeMap.lastEntry()); // Prints 12=twelve
    System.out.println(treeMap.size()); // Prints 4, since there are 4 elemens in the map
    System.out.println(treeMap.get(12)); // Prints twelve
    System.out.println(treeMap.get(15)); // Prints null, since the key is not found in the map


We can also iterate over the map elements using either an Iterator, or a foreach loop. Note that the entries are printed according to their [natural ordering][1], not the 
insertion order:

<!-- if version [gte Java SE 7] --> 

    for (Entry<Integer, String> entry : treeMap.entrySet()) {
        System.out.print(entry + " "); //prints 1=one 4=four 10=ten 12=twelve 
    }

<!-- end version if -->



    Iterator<Entry<Integer, String>> iter = treeMap.entrySet().iterator();
    while (iter.hasNext()) {
        System.out.print(iter.next() + " "); //prints 1=one 4=four 10=ten 12=twelve 
    }


  [1]: https://docs.oracle.com/javase/tutorial/collections/interfaces/order.html

## TreeSet of a simple Java Type
First, we create an empty set, and insert some elements into it: 
<!-- if version [gte Java SE 7] --> 
    TreeSet<Integer> treeSet = new TreeSet<>();
<!-- end version if -->

<!-- if version [lt Java SE 7] --> 
    TreeSet<Integer> treeSet = new TreeSet<Integer>();
<!-- end version if -->

    treeSet.add(10);
    treeSet.add(4);
    treeSet.add(1);
    treeSet.add(12);

Once we have a few elements in the set, we can perform some operations:

    System.out.println(treeSet.first()); // Prints 1
    System.out.println(treeSet.last()); // Prints 12
    System.out.println(treeSet.size()); // Prints 4, since there are 4 elemens in the set
    System.out.println(treeSet.contains(12)); // Prints true
    System.out.println(treeSet.contains(15)); // Prints false

We can also iterate over the map elements using either an Iterator, or a foreach loop. Note that the entries are printed according to their [natural ordering][1], not the insertion order:

<!-- if version [gte Java SE 7] --> 

    for (Integer i : treeSet) {
        System.out.print(i + " "); //prints 1 4 10 12
    }

<!-- end version if -->

    Iterator<Integer> iter = treeSet.iterator();
    while (iter.hasNext()) {
        System.out.print(iter.next() + " "); //prints 1 4 10 12
    }


  [1]: https://docs.oracle.com/javase/tutorial/collections/interfaces/order.html

## TreeMap/TreeSet of a custom Java type
Since `TreeMap`s and `TreeSet`s maintain keys/elements according to their [natural ordering][1]. Therefor `TreeMap` keys and `TreeSet` elements have to comparable to one another.

Say we have a custom `Person` class:

    public class Person  {
    
        private int id;
        private String firstName, lastName;
        private Date birthday;

        //... Constuctors, getters, setters and various methods
    }

If we store it as-is in a `TreeSet` (or a Key in a TreeMap):

    TreeSet<Person2> set = ...      
    set.add(new Person(1,"first","last",Date.from(Instant.now())));

Then we'd run into an Exception such as this one:

    Exception in thread "main" java.lang.ClassCastException: Person cannot be cast to java.lang.Comparable
        at java.util.TreeMap.compare(TreeMap.java:1294)
        at java.util.TreeMap.put(TreeMap.java:538)
        at java.util.TreeSet.add(TreeSet.java:255)

To fix that, let's assume that we want to order `Person` instances based on the order of their ids (`private int id`). We could do it in one of two ways:

 1. One solution is to modify `Person` so it would implement the [Comparable interface][2]:

        public class Person implements Comparable<Person> {
            private int id;
            private String firstName, lastName;
            private Date birthday;
        
            //... Constuctors, getters, setters and various methods
    
            @Override
            public int compareTo(Person o) {
                return Integer.compare(this.id, o.id); //Compare by id
            }
        }
2. Another solution is to provide the `TreeSet` with a [Comparator][3]:
<!-- if version [gte Java SE 8] --> 
    TreeSet<Person> treeSet = new TreeSet<>((personA, personB) -> Integer.compare(personA.getId(), personB.getId()));
<!-- end version if -->
    TreeSet<Person> treeSet = new TreeSet<>(new Comparator<Person>(){
        @Override
        public int compare(Person personA, Person personB) {
            return Integer.compare(personA.getId(), personB.getId());
        }
    });


However, there are two caveats to both approaches:
1. It's **very important** not to modify any fields used for ordering once an instance has been inserted into a `TreeSet`/`TreeMap`. In the above example, if we change the `id` of a person that's already inserted into the collection, we might run into unexpected behavior.

2.  It's important to implement the comparison properly and consistently. As per the [Javadoc][2]:

> The implementor must ensure `sgn(x.compareTo(y)) ==
> -sgn(y.compareTo(x))` for all x and y. (This implies that `x.compareTo(y)` must throw an exception iff `y.compareTo(x)` throws an
> exception.)
> 
> The implementor must also ensure that the relation is transitive:
> `(x.compareTo(y)>0 && y.compareTo(z)>0)` implies `x.compareTo(z)>0`.
> 
> Finally, the implementor must ensure that `x.compareTo(y)==0` implies
> that `sgn(x.compareTo(z)) == sgn(y.compareTo(z))`, for all z.

  [1]: https://docs.oracle.com/javase/tutorial/collections/interfaces/order.html
  [2]: https://docs.oracle.com/javase/8/docs/api/java/lang/Comparable.html
  [3]: https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html

## TreeMap and TreeSet Thread Safety
`TreeMap` and `TreeSet` are **not** thread-safe collections, so care must be taken to ensure when used in multi-threaded programs.

Both `TreeMap` and `TreeSet` are safe when read, even concurrently, by multiple threads. So if they have been created and populated by a single thread (say, at the start of the program), and only then read, but not modified by multiple threads, there's no reason for synchronization or locking. 

However, if read and modified concurrently, or modified concurrently by more than one thread, the collection might throw a [ConcurrentModificationException][1] or behave unexpectedly. In these cases, it's imperative to synchronize/lock access to the collection using one of the following approaches:

1. Using `Collections.synchronizedSorted..`:

       SortedSet<Integer> set = Collections.synchronizedSortedSet(new TreeSet<Integer>());
       SortedMap<Integer,String> map = Collections.synchronizedSortedMap(new TreeMap<Integer,String>());

   This will provide a [SortedSet][2]/[SortedMap][3] implementation backed by the actual collection, and synchronized on some mutex object. Note that this will synchronize all read and write access to the collection on a single lock, so even concurrent reads would not be possible.

2. By manually synchronizing on some object, like the collection itself:
          
        TreeSet<Integer> set = new TreeSet<>(); 
    ...

       //Thread 1
       synchronized (set) {
           set.add(4);
       }
    ...

       //Thread 2
       synchronized (set) {
           set.remove(5);
       }        
   
3. By using a lock, such as a [ReentrantReadWriteLock][4]:

        TreeSet<Integer> set = new TreeSet<>(); 
        ReentrantReadWriteLock lock = new ReentrantReadWriteLock();
      ...

        //Thread 1
        lock.writeLock().lock();
        set.add(4);
        lock.writeLock().unlock();
      ...  

        //Thread 2
        lock.readLock().lock();
        set.contains(5);
        lock.readLock().unlock();

As opposed to the previous synchronization methods, using a [ReadWriteLock][5] allows multiple threads to read from the map concurrently.


  [1]: https://docs.oracle.com/javase/8/docs/api/java/util/ConcurrentModificationException.html
  [2]: https://docs.oracle.com/javase/8/docs/api/java/util/SortedSet.html
  [3]: https://docs.oracle.com/javase/8/docs/api/java/util/SortedMap.html
  [4]: https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/locks/ReentrantReadWriteLock.html
  [5]: https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/locks/ReadWriteLock.html

