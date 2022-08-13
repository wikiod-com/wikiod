---
title: "Queue"
slug: "queue"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Intro to Queue
The queue is a FIFO (first-in, first-out) data-structure, i.e. the first element added to the queue will be the first element removed ("first out").

Let us consider the example of customers waiting to be helped. Alice, Bob, and Dan are all at the supermarket. Alice is ready to pay, so she approaches the cashier. Alice is now in the queue. She is the only person in the queue, so she is both at the front and at the back.

Now, the queue looks like this:
<pre>              |-------|
              | Alice | cashier
              |-------|
                ▲   ▲
back of queue ──┘   └── front of queue
</pre>
Now Bob and then Dan approach the cashier. They are added to the queue. Alice is still at the front, and Dan is at the back:
<pre>              |-------||-------||-------|
              |  Dan  ||  Bob  || Alice | cashier
              |-------||-------||-------|
                ▲                     ▲
back of queue ──┘                     └── front of queue
</pre>
Adding a person to the queue is the enqueue operation. Alice, Bob, and Dan have been enqueued. As the cashier helps each customer, they will be removed from the queue. This is the dequeue operation. Customers, which represent the data elements in our queue, are dequeued from the front of the queue. This means that the first customer that approached up to the cashier was the first customer to be helped (FIFO).

## Queue Implementation by using array.
Queue follow FIFO as it is mentioned in the introduction.
Five major operations:

1. Enqueue(x): pushes x to the back of the queue.
2. Dequeue(): pops an element from the front of the queue.
3. isEmpty(): Finds whether the queue is empty or not.
4. isFull(): Finds whether the queue is full or not.
5. frontValue(): Returns the front value of the Queue.

All the operations are constant O(1) time.

**Code**:

    #include<stdio.h>

    #define MAX 4

    int front = -1;
    int rear = -1;
    int a[MAX];

    bool isFull() {
      if(rear == MAX-1)
        return true;
      else
        return false;    
    }

    bool isEmpty() {
      if(rear == -1 && front==-1)
        return true;
      else
        return false;    
    }

    void enqueue(int data) {
      if (isFull()){
        printf("Queue is full\n");
        return;
      } else if(isEmpty()) {
        front = rear =0;
      } else {
        rear = rear + 1;
        a[rear] = data;      
      }
    }
        
    void deque(){
      if(isEmpty()){
        printf("Queue is empty\n");
        return;
      } else if(front == rear) {
        front =-1;
        rear =-1;
      } else {
        front = front + 1;
      }
    }
        
    void print(){
      printf("Elements in Queue are\n");  
      for(int i = front;i<=rear;i++){
        printf("%d ",a[i]);
      }
      printf("\n");
    }

    int frontValue(){  
      printf("The front element after set of enqueue and dequeue is %d\n", a[front]);
    }

    int main(){
      deque();     // Queue is empty message will be thrown
      enqueue(10);
      print();
      enqueue(20);
      print();
      enqueue(30);
      print();
      enqueue(40);
      frontValue();
      print();
      enqueue(50);
      frontValue();
      deque();
      deque();
      enqueue(50);
      frontValue();
      print();                    
      return 0;
    }

## Implementation of a circular Queue
Memory is efficiently organized in a circular queue as compared to linear queue.

**In Linear Queue:**

[![enter image description here][1]][1]

**In Circular Queue:**

[![enter image description here][2]][2]

**Remaining spaces can be used:**

**Code for it to do the same:**

    #include<stdio.h>
    #define MAX 10000
    int front = -1;
    int rear = -1;
    int a[MAX]; 


    bool isFull() {
      if((rear+1) % MAX == front)
        return true;
      else
        return false;    
    }

    bool isEmpty() {
      if(rear == -1 && front==-1)
        return true;
      else
        return false;    
    }

    void enqueue(int data) {
      if (isFull()){
        printf("Queue is full\n");
        return;
      } else if(isEmpty()) {
        front = rear = 0;
      } else {
        rear = (rear+1) % MAX;
        a[rear] = data;
      }
    }

    void deque() {
      if(isEmpty()){
        printf("Queue is empty\n");
        return;
      } else if(front == rear) {
        front =-1;
        rear =-1;
      } else {
        front = (front+1) % MAX;
      }
    }

    int frontValue() {
      return(a[front]);
    }

    int main() {   
      deque(); 
      enqueue(10);
      enqueue(20);
      enqueue(30);       
      enqueue(40);
      enqueue(50);
      frontValue();
      return 0;
    }

All operations have O(1) time complexity.

  [1]: https://i.stack.imgur.com/mXvnq.png
  [2]: https://i.stack.imgur.com/IQGEe.png

## Linked List representation of Queue
Linked list representation is more efficient in terms of memory managerment.

**Code to show enqueue and deque in a Queue using Linklist in O(1) time.**

    #include<stdio.h>
    #include<malloc.h>
    
    struct node{
        int data;
        node* next;
    };
    
    node* front = NULL;
    node* rear = NULL;
    
    void enqueue(int data){      //adds element to end
        
        struct node* temp = (struct node*)malloc(sizeof(struct node*));
        temp->data = data;
        temp->next =NULL;
        
        if(front== NULL && rear == NULL){
            front = rear = temp;
            return;
        }
        rear->next = temp;
        rear= temp;
    }
    
    void deque(){     //removes element from front
        node* temp = front;
        
        if(front== NULL && rear == NULL){
            return;
        }
        else if (front==rear){
            front =rear = NULL;
        }
        else
        front= front ->next;
        
        free(temp);
    }
    
    void print(){
        node* temp = front;
        
        for(; temp != rear; temp=temp->next){
            printf("%d ",temp->data);
        }
        //for printing the rear element
        
            printf("%d ",temp->data);
        printf("\n");
    }
    
    
    
    int main(){
        
        enqueue(20);
        enqueue(50);
            enqueue(70);
            printf("After set of enques\n");
            print();
            
            deque();
            printf("After 1 deque\n");    
            print();
            
            return 0;    
        
        
    }

