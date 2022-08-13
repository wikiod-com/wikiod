---
title: "Linked List"
slug: "linked-list"
draft: false
images: []
weight: 9902
type: docs
toc: true
---

## XOR Linked List
A **XOR Linked list** is also called a **Memory-Efficient Linked List**. It is another form of a doubly linked list. This is highly dependent on the **XOR** logic gate and its properties.

**<h1>Why is this called the Memory-Efficient Linked List? </h1>**

This is called so because this uses less memory than a traditional doubly linked list.

--- 

**<h1>Is this different from a Doubly Linked List?</h1>**

**Yes**, it is. 

A **Doubly-Linked List** is storing two pointers, which point at the next and the previous node. Basically, if you want to go back, you go to the address pointed by the `back` pointer. If you want to go forward, you go to the address pointed by the `next` pointer. It's like:

[![enter image description here][1]][1]

A **Memory-Efficient Linked List**, or namely the **XOR Linked List** is having only one pointer instead of two. This stores the previous address (addr (prev)) **XOR** (^) the next address (addr (next)). When you want to move to the next node, you do certain calculations, and find the address of the next node. This is the same for going to the previous node. It is like:

[![enter image description here][2]][2]

---

**<h1>How does it work?</h1>**

To understand how the linked list works, you need to know the properties of XOR (^):

    |-------------|------------|------------|
    |    Name     |   Formula  |    Result  |
    |-------------|------------|------------|
    | Commutative |    A ^ B   |    B ^ A   |
    |-------------|------------|------------|
    | Associative | A ^ (B ^ C)| (A ^ B) ^ C|
    |-------------|------------|------------|
    | None (1)    |    A ^ 0   |     A      |
    |-------------|------------|------------|
    | None (2)    |    A ^ A   |     0      |
    |-------------|------------|------------|
    | None (3)    | (A ^ B) ^ A|     B      |
    |-------------|------------|------------|

Now let's leave this aside, and see what each node stores.

The first node, or the **head**, stores `0 ^ addr (next)` as there is no previous node or address. It looks like [this](http://i.stack.imgur.com/j1UPg.png).

Then the second node stores `addr (prev) ^ addr (next)`. It looks like [this](http://i.stack.imgur.com/IHfqG.png).

The **tail** of the list, does not have any next node, so it stores `addr (prev) ^ 0`. It looks like [this](http://i.stack.imgur.com/qDdQ6.png).


**Moving from Head to the next node**

Let's say you are now at the first node, or at node A. Now you want to move at node B. This is the formula for doing so:

    Address of Next Node = Address of Previous Node ^ pointer in the current Node

So this would be:

    addr (next) = addr (prev) ^ (0 ^ addr (next))

As this is the head, the previous address would simply be 0, so:

    addr (next) = 0 ^ (0 ^ addr (next))

We can remove the parentheses:

    addr (next) = 0 ^ 0 addr (next)

Using the `none (2)` property, we can say that `0 ^ 0` will always be 0:

    addr (next) = 0 ^ addr (next)

Using the `none (1)` property, we can simplify it to:

    addr (next) = addr (next)

You got the address of the next node!

**Moving from a node to the next node**

Now let's say we are in a middle node, which has a previous and next node.

Let's apply the formula:

    Address of Next Node = Address of Previous Node ^ pointer in the current Node

Now substitute the values:

    addr (next) = addr (prev) ^ (addr (prev) ^ addr (next))

Remove Parentheses:

    addr (next) = addr (prev) ^ addr (prev) ^ addr (next)

Using the `none (2)` property, we can simplify:

    addr (next) = 0 ^ addr (next)

Using the `none (1)` property, we can simplify:

    addr (next) = addr (next)

And you get it! 

**Moving from a node to the node you were in earlier**

If you aren't understanding the title, it basically means that if you were at node X, and have now moved to node Y, you want to go back to the node visited earlier, or basically node X.

This isn't a cumbersome task. Remember that I had mentioned above, that you store the address you were at in a temporary variable. So the address of the node you want to visit, is lying in a variable:

    addr (prev) = temp_addr

**Moving from a node to the previous node**

This isn't the same as mentioned above. I mean to say that, you were at node Z, now you are at node Y, and want to go to node X.

This is nearly same as moving from a node to the next node. Just that this is it's vice-versa. When you write a program, you will use the same steps as I had mentioned in the moving from one node to the next node, just that you are finding the earlier element in the list than finding the next element.

---

**<h1>Example Code in C</h1>**

    /* C/C++ Implementation of Memory efficient Doubly Linked List */
    #include <stdio.h>
    #include <stdlib.h>
     
    // Node structure of a memory efficient doubly linked list
    struct node
    {
        int data;
        struct node* npx;  /* XOR of next and previous node */
    };
     
    /* returns XORed value of the node addresses */
    struct node* XOR (struct node *a, struct node *b)
    {
        return (struct node*) ((unsigned int) (a) ^ (unsigned int) (b));
    }
     
    /* Insert a node at the begining of the XORed linked list and makes the
       newly inserted node as head */
    void insert(struct node **head_ref, int data)
    {
        // Allocate memory for new node
        struct node *new_node  = (struct node *) malloc (sizeof (struct node) );
        new_node->data = data;
     
        /* Since new node is being inserted at the begining, npx of new node
           will always be XOR of current head and NULL */
        new_node->npx = XOR(*head_ref, NULL);
     
        /* If linked list is not empty, then npx of current head node will be XOR 
           of new node and node next to current head */
        if (*head_ref != NULL)
        {
            // *(head_ref)->npx is XOR of NULL and next. So if we do XOR of 
            // it with NULL, we get next
            struct node* next = XOR((*head_ref)->npx,  NULL);
            (*head_ref)->npx = XOR(new_node, next);
        }
     
        // Change head
        *head_ref = new_node;
    }
     
    // prints contents of doubly linked list in forward direction
    void printList (struct node *head)
    {
        struct node *curr = head;
        struct node *prev = NULL;
        struct node *next;
     
        printf ("Following are the nodes of Linked List: \n");
     
        while (curr != NULL)
        {
            // print current node
            printf ("%d ", curr->data);
     
            // get address of next node: curr->npx is next^prev, so curr->npx^prev
            // will be next^prev^prev which is next
            next = XOR (prev, curr->npx);
     
            // update prev and curr for next iteration
            prev = curr;
            curr = next;
        }
    }
     
    // Driver program to test above functions
    int main ()
    {
        /* Create following Doubly Linked List
           head-->40<-->30<-->20<-->10   */
        struct node *head = NULL;
        insert(&head, 10);
        insert(&head, 20);
        insert(&head, 30);
        insert(&head, 40);
     
        // print the created list
        printList (head);
     
        return (0);
    }

The above code performs two basic functions: insertion and transversal.

 - **Insertion:** This is performed by the function `insert`. This inserts a new node at the beginning. When this function is called, it puts the new node as the head, and the previous head node as the second node. 

 - **Traversal:** This is performed by the function `printList`. It simply goes through each node and prints out the value.

Note that XOR of pointers is not defined by C/C++ standard. So the above implementation may not work on all platforms.

---

**<h1>References</h1>**

 - https://cybercruddotnet.wordpress.com/2012/07/04/complicating-things-with-xor-linked-lists/

 - http://www.ritambhara.in/memory-efficient-doubly-linked-list/comment-page-1/

 - http://www.geeksforgeeks.org/xor-linked-list-a-memory-efficient-doubly-linked-list-set-2/

Note that I have taken a lot of content from [my own answer](http://stackoverflow.com/questions/35841620/what-is-a-memory-efficient-doubly-linked-list-in-c/35943677#35943677) on main.






  [1]: http://i.stack.imgur.com/t1RDM.gif
  [2]: http://i.stack.imgur.com/wFTQ7.png


## Introduction to Linked Lists
A linked list is a linear collection of data elements, called nodes, which are linked to other node(s) by means of a "pointer." Below is a singly linked list with a head reference.
```                                  
         ┌─────────┬─────────┐   ┌─────────┬─────────┐         
 HEAD ──▶│  data   │"pointer"│──▶│  data   │"pointer"│──▶ null 
         └─────────┴─────────┘   └─────────┴─────────┘         
```
There are many types of linked lists, including [singly](https://www.wikiod.com/data-structures/linked-list#Singly Linked List) and [doubly](https://www.wikiod.com/data-structures/linked-list#Doubly Linked List) linked lists and circular linked lists.

**Advantages**

 - Linked lists are a dynamic data structure, which can grow and shrink, allocating and deallocating memory while the program is running.

 - Node insertion and deletion operations are easily implemented in a linked list.

 - Linear data structures such as stacks and queues are easily implemented with a linked list.

 - Linked lists can reduce access time and may expand in real time without memory overhead.

## Singly Linked List
Singly Linked Lists are a type of [linked list](https://www.wikiod.com/data-structures/linked-list#Introduction to Linked Lists). A singly linked list's nodes have only one "pointer" to another node, usually "next." It is called a singly linked list because each node only has a single "pointer" to another node. A singly linked list may have a head and/or tail reference. The advantage of having a tail reference are the `getFromBack`, `addToBack`, and `removeFromBack` cases, which become O(1).
```
         ┌─────────┬─────────┐   ┌─────────┬─────────┐         
 HEAD ──▶│  data   │"pointer"│──▶│  data   │"pointer"│──▶ null 
         └─────────┴────△────┘   └─────────┴─────────┘         
          SINGLE        │                                      
          REFERENCE ────┘                                                                
```
**[Example Code in C](http://www.thelearningpoint.net/computer-science/data-structures-singly-linked-list-with-c-program-source-code)**

**[Example Code in Java, with unit tests - singly linked list with head reference](https://gist.github.com/etylerflynn/b877dd089ee54eaafdc7c18d4d5495b3)**

## Skip List
Skip lists are linked lists that allow you to skip to the correct node. This is a method which is way more fast than a normal singly linked list.  It is basically a [singly linked list](https://www.wikiod.com/data-structures/linked-list#Singly Linked List) but the pointers not going from one node to the next node, but skipping few nodes. Thus the name "Skip List".

---

**<h1>Is this different from a Singly Linked List?</h1>**

**Yes**, it is. 

A Singly Linked List is a list with each node pointing to the next node. A graphical representation of a singly linked list is like:

[![enter image description here][1]][1]

A Skip List is a list with each node pointing to a node which might or might not be after it. A graphical representation of a skip list is:

[![enter image description here][2]][2]

---

**<h1>How does it work?</h1>**

The Skip List is simple. Let's say we want to access node 3 in the above image. We can't take the route of going from the head to the fourth node, as that is after the third node. So we go from the head to the second node, and then to the third one. 

A graphical representation is like:

[![enter image description here][3]][3]

---

**<h1>References</h1>**

 - http://igoro.com/archive/skip-lists-are-fascinating/

  [1]: http://i.stack.imgur.com/YjCID.png
  [2]: http://i.stack.imgur.com/K4eZ2.png
  [3]: http://i.stack.imgur.com/yuKyB.png

## Doubly Linked List
Doubly Linked Lists are a type of [linked list](https://www.wikiod.com/data-structures/linked-list#Introduction to Linked Lists). A doubly linked list's nodes have two "pointers" to other nodes, "next" and "previous." It is called a double linked list because each node only has two "pointers" to other nodes. A doubly linked list may have a head and/or tail pointer.
```
         ┌─────────┬─────────┬─────────┐   ┌─────────┬─────────┬─────────┐         
 null ◀──┤previous │  data   │  next   │◀──┤previous │  data   │  next   │         
         │"pointer"│         │"pointer"│──▶│"pointer"│         │"pointer"│──▶ null 
         └─────────┴─────────┴─────────┘   └─────────┴─────────┴─────────┘         
                          ▲                     △                   △              
                     HEAD │                     │     DOUBLE        │              
                                                └──── REFERENCE ────┘                 
```
Doubly linked lists are less space efficient than singly linked lists; however, for some operations, they offer significant improvements in time efficiency. A simple example is the `get` function, which for a doubly linked list with a head and tail reference will start from head or tail depending on the index of the element to get. For an `n`-element list, to get the `n/2 + i` indexed element, a singly linked list with head/tail references must traverse `n/2 + i` nodes, because it cannot "go back" from tail. A doubly linked list with head/tail references only has to traverse `n/2 - i` nodes, because it can "go back" from tail, traversing the list in reverse order.

**[Example Code in C](http://www.thelearningpoint.net/computer-science/data-structures-doubly-linked-list-with-c-program-source-code)**

## A basic SinglyLinkedList example in Java
A basic implementation for singly-linked-list in java - that can add integer values to the end of the list, remove the first value encountered value from list, return an array of values at a given instant and determine whether a given value is present in the list.

Node.java

    package com.example.so.ds;
    
    
    /**
     * <p> Basic node implementation </p>
     * @since 20161008
     * @author Ravindra HV
     * @version 0.1
     */
    public class Node {
        
        private Node next;
        private int value;
        
        public Node(int value) {
            this.value=value;
        }
    
        public Node getNext() {
            return next;
        }
    
        public void setNext(Node next) {
            this.next = next;
        }
    
        public int getValue() {
            return value;
        }
        
        
    }



SinglyLinkedList.java

    package com.example.so.ds;
    
    
    /**
     * <p> Basic single-linked-list implementation </p>
     * @since 20161008
     * @author Ravindra HV
     * @version 0.2
     */
    public class SinglyLinkedList {
        
        private Node head;
        private volatile int size;
        
        public int getSize() {
            return size;
        }
        
        public synchronized void append(int value) {
            
            Node entry = new Node(value);
            if(head == null) {
                head=entry;
            }
            else {
                Node temp=head;
                while( temp.getNext() != null) {
                    temp=temp.getNext();
                }
                temp.setNext(entry);
            }
            
            size++;
        }
        
        
        public synchronized boolean removeFirst(int value) {
            boolean result = false;
            
            if( head == null ) { // or size is zero..
                // no action
            }
            else if( head.getValue() == value ) {
                head = head.getNext();
                result = true;
            }
            else {
    
                Node previous = head;
                Node temp = head.getNext();
                while( (temp != null) && (temp.getValue() != value) ) {
                    previous = temp;
                    temp = temp.getNext();
                }
                
                if((temp != null) && (temp.getValue() == value)) { // if temp is null then not found..
                    previous.setNext( temp.getNext() );
                    result = true;
                }
    
            }
            
            if(result) {
                size--;
            }
            
            return result;
            
        }
        
        public synchronized int[] snapshot() {
            Node temp=head;
            int[] result = new int[size];
            for(int i=0;i<size;i++) {
                result[i]=temp.getValue();
                temp = temp.getNext();
            }
            return result;
        }
        
        public synchronized boolean contains(int value) {
            boolean result = false;
            Node temp = head;
            
            while(temp!=null) {
                if(temp.getValue() == value) {
                    result=true;
                    break;
                }
                temp=temp.getNext();
            }
            return result;
        }
        
    }

TestSinglyLinkedList.java 


    package com.example.so.ds;
    
    import java.util.Arrays;
    import java.util.Random;
    
    
    /**
     * 
     * <p> Test-case for single-linked list</p>
     * @since 20161008
     * @author Ravindra HV
     * @version 0.2
     *
     */
    public class TestSinglyLinkedList {
    
        /**
         * @param args
         */
        public static void main(String[] args) {
            
            SinglyLinkedList singleLinkedList = new SinglyLinkedList();
            
            int loop = 11;
            Random random = new Random();
            
            for(int i=0;i<loop;i++) {
                
                int value = random.nextInt(loop);
                singleLinkedList.append(value);
    
                System.out.println();
                System.out.println("Append :"+value);
                System.out.println(Arrays.toString(singleLinkedList.snapshot()));
                System.out.println(singleLinkedList.getSize());
                System.out.println();
            }
            
            for(int i=0;i<loop;i++) {
                int value = random.nextInt(loop);
                boolean contains = singleLinkedList.contains(value);
                singleLinkedList.removeFirst(value);
                
                System.out.println();
                System.out.println("Contains :"+contains);
                System.out.println("RemoveFirst :"+value);
                System.out.println(Arrays.toString(singleLinkedList.snapshot()));
                System.out.println(singleLinkedList.getSize());
                System.out.println();
            }
            
    
        }
    
    }




