---
title: "Continue and Break!"
slug: "continue-and-break"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Continue and Break Statement
The continue statement in Objective-C programming language works somewhat like the break statement. Instead of forcing termination, however, continue forces the next iteration of the loop to take place, skipping any code in between.

For the for loop, continue statement causes the conditional test and increment portions of the loop to execute. For the while and do...while loops, continue statement causes the program control pass to the conditional tests.


    #import <Foundation/Foundation.h>
     
    int main ()
    {
       /* local variable definition */
       int a = 10;
    
       /* do loop execution */
       do
       {
          if( a == 15)
          {
             /* skip the iteration */
             a = a + 1;
             continue;
          }
          NSLog(@"value of a: %d\n", a);
          a++;
         
       }while( a < 20 );
     
       return 0;
    }

**Output:**

    2013-09-07 22:20:35.647 demo[29998] value of a: 10
    2013-09-07 22:20:35.647 demo[29998] value of a: 11
    2013-09-07 22:20:35.647 demo[29998] value of a: 12
    2013-09-07 22:20:35.647 demo[29998] value of a: 13
    2013-09-07 22:20:35.647 demo[29998] value of a: 14
    2013-09-07 22:20:35.647 demo[29998] value of a: 16
    2013-09-07 22:20:35.647 demo[29998] value of a: 17
    2013-09-07 22:20:35.647 demo[29998] value of a: 18
    2013-09-07 22:20:35.647 demo[29998] value of a: 19

Refer to this [link][1] for more information.


  [1]: https://www.tutorialspoint.com/objective_c/objective_c_continue_statement.htm

