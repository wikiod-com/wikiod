---
title: "Stack"
slug: "stack"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Intro to Stack
The stack is a LIFO (last-in, first-out) data-structure, i.e. the most recent (or "last in") element added to the stack will be the first element removed ("first out").

Let us consider the example of books in a box. Only one book can be added or removed from from the box at a time, and it can only be added and removed from the top. 

Now, the box with the first two books looks like this:
<pre>  |--------|
  | book 2 | ◀──── top of box
  |--------|
  | book 1 | ◀──── bottom of box
  |--------|</pre>
If we add book 3, it will be at the top. The rest of the books, which were added before book 3, will remain below it:
<pre>  |--------|
  | book 3 | ◀──── top of box
  |--------|
  | book 2 |
  |--------|
  | book 1 | ◀──── bottom of box
  |--------|</pre>
The box is like a stack and the books are data elements. Adding a book to the box is the push operation while removing/getting a book from the top of the box is the pop operation. If we perform the pop operation, we will get book 3, and the box will go back to the way it was before we pushed book 3. This means that the last (or most recent) element that we put in was the first element we got out (LIFO). In order to get book 1, we have to perform two more pops: one to remove book 2, and the second to get book 1.

Implementation of the stack using an array. For this, we need an index pointing to the top location (tos). Every time an element is pushed into the stack the tos increments by one and whenever a pop operation is performed the index pointing the top of the stack (tos) is decremented by one.

**PUSH:**

**Before the PUSH operation**
<pre>
                                  tos
                                   |
                                   |
        |-----|-----|-----|-----|-----|
        | i1  | i2  | i3  |  i4 |     |
        |-----|-----|-----|-----|-----|
</pre>
    void push(dataElment item){
        stack[top]=item; //item = i4
        top++;
        
    }
**After the PUSH operation**
The stack has a pointer to the top of the stack. Whenever the push operation is called it places the value at the top and updates  it the value.    
<pre>
        tos -- Top of the stack
                            tos
                             |
                             |
        |-----|-----|-----|-----|
        | i1  | i2  | i3  |     |
        |-----|-----|-----|-----|
</pre>
**POP :**
The pop operation removes the content from the top of the stack and updates the value of tos by decrementing by 1

**Before the pop operation:**
<pre>
                                  tos
                                   |
                                   |
        |-----|-----|-----|-----|-----|
        | i1  | i2  | i3  |  i4 |     |
        |-----|-----|-----|-----|-----|
</pre>

    dataElment pop(){
        dataElment value = stack[tos--];
        return value;
    }

**After the pop operation:**
<pre>
                            tos
                             |
                             |
        |-----|-----|-----|-----|
        | i1  | i2  | i3  |  i4 |
        |-----|-----|-----|-----|
        When a push operation is performed it overwrites i4.
    
</pre>

## Using stacks to find palindromes
A palindrome is a word that can be read both ways, like 'kayak'.

This example will show how to find if a given word is a palindrome using Python3.

First, we need to turn a string into a stack (we will use arrays as stacks).

<!-- language: lang-python -->
    str = "string"
    stk = [c for c in str] #this makes an array: ["s", "t", "r", "i", "n", "g"]
    stk.append("s") #adds a letter to the array
    stk.pop() #pops the last element

Now, we have to invert the word.

<!-- language: lang-python -->
    def invert(str):
        stk = [c for c in str]
        stk2 = []
        while len(stk) > 0:
            stk2.append(stk.pop())
        #now, let's turn stk2 into a string
        str2 = ""
        for c in stk2:
            str2 += c
        return str2

Now we can compare the word and the inverted form.
<!-- language: lang-python -->

    def palindrome(str):
        return str == invert(str)

## Stack Implementation by using array and Linked List
Array Implementation

    #include<stdio.h>
    
    #define MAX 100000                    //Global variables
    int top = -1;
    int a[MAX];
    
    void push(int x){
        
        if(top == MAX-1){                         // Check whether stack is full
            printf("Stack Overflow\n");
            return;
        }
        
        a[++top] = x;                            // Otherwise increment top and insert x
    }
    
    void pop(){
        if(top == -1){                                     // if top = -1, empty stack
            printf("Empty stack\n");
            return;
        }
        top--;        
    }
    
    void print(){                         //printing stack values
        
        for(int i=0;i <= top;i++){
            printf("%d ",a[i]);        
        }        
        printf("\n");        
    }
    
    void topValue(){                        // Method can print the top value of a stack
        printf("%d",a[top]);
    }
    int main(){
            
        push(5);
        push(20);
        push(15);
        print();
        pop();
        print();
        push(35);
        print();
        topValue();    
        
    }


Linked List implementation

    #include<stdio.h>
    #include<malloc.h>
    
     struct node{
        int data;
        node* link;
    };
    node* top = NULL;
    
    void push(int data){
        
        node* temp = (struct node*)malloc(sizeof(struct node*));
        temp->data = data;
        temp->link = top;
        top = temp;
    }
    
    void pop(){
        
        if(top == NULL) return;
        node* temp = top;
        top = top->link;
        free(temp);
    }
    void print(){
        
        node* t = top;
        while(t != NULL){
            printf("%d ",t->data);
            t=t->link;
        }
        printf("\n");
    }
    
    void topValue(){
        printf("%d ",top->data);
    }
    int main(){
            
        push(5);
        push(20);
        push(15);
        print();
        pop();
        print();
        push(35);
        print();            
        topValue();    
    }




## Checking Balanced Parentheses




A bracket is considered to be any one of the following characters: `(`, `)`, `{`, `}`, `[`, or `]`.

Two brackets are considered to be a matched pair if the an opening bracket (i.e., `(`, `[`, or `{`) occurs to the left of a closing bracket (i.e., `)`, `]`, or `}`) of the exact same type. There are three types of matched pairs of brackets: `[]`, `{}`, and `()`.

A matching pair of brackets is not balanced if the set of brackets it encloses are not matched. For example, `{[(])}` is not balanced because the contents in between `{` and `}` are not balanced. The pair of square brackets encloses a single, unbalanced opening bracket, `(`, and the pair of parentheses encloses a single, unbalanced closing square bracket, `]`.

By this logic, we say a sequence of brackets is considered to be balanced if the following conditions are met:

> It contains no unmatched brackets.
> 
> The subset of brackets enclosed within the confines of a matched pair
> of brackets is also a matched pair of brackets.

**Algorithm:**
1) Declare a stack (say `stack`).
2) Now traverse the string input. 
 - a) If the current character is a starting bracket ( i.e. `(` or `{` or `[` ) then push it to stack.
 - b) If the current character is a closing bracket ( i.e. `)` or `}` or `]`)
   then pop from stack. If the popped character is the matching
   starting bracket then fine else parenthesis are `not balanced`.
 - c) If the current character is a closing bracket ( i.e. `)` or `}` or `]`) and the stack is empty, then parenthesis are `not balanced`.
3) After complete traversal, if there is some starting bracket left in stack then the string is `not balanced` else we have a `balanced` string.


