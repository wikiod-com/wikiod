---
title: "Iterator Pattern"
slug: "iterator-pattern"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## The Iterator Pattern
[![The Iterator Pattern][1]][1]

Collections are one of the most commonly used data structures in software engineering. A Collection is just a group of objects. A collection can be a List, an array, a map, a tree or anything. So, a collection should provide some way to access its elements without exposing its internal structure. We should be able to traverse it in a same irrespective of type of collection it is.

The iterator pattern idea is to take the responsibility of accessing the object of a collection and put it in an iterator object. The iterator object in return will maintain the order of iteration, keep a track of current item and must be having a way to fetch the next element.

Usually, the collection class carries two components: the class itself, and it's `Iterator`.


    public interface Iterator {
       public boolean hasNext();
       public Object next();
    }

    public class FruitsList {
        public String fruits[] = {"Banana", "Apple", "Pear", "Peach", "Blueberry"};

        public Iterator getIterator() {
           return new FruitIterator();
        }

        private class FruitIterator implements Iterator {
           int index;
 
           @Override
           public boolean hasNext() {
               return index < fruits.length;
           }
 
           @Override
           public Object next() {
       
              if(this.hasNext()) {
                return names[index++];
              }
              return null;
           }        
        }
    }


  [1]: http://i.stack.imgur.com/vuEFq.png

