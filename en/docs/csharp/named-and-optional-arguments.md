---
title: "Named and Optional Arguments"
slug: "named-and-optional-arguments"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

**Named Arguments**

*Ref: MSDN* Named arguments enable you to specify an argument for a particular parameter by associating the argument with the parameter’s name rather than with the parameter’s position in the parameter list.

As said by MSDN, A named argument ,

 - Enables you to pass the argument to the function by associating the
   parameter’s name.
 - No needs for remembering the parameters position that we are not
   aware of always.
 - No need to look the order of the parameters in the parameters list of
   called function.
 - We can specify parameter for each arguments by its name.

**Optional Arguments**

*Ref: MSDN* The definition of a method, constructor, indexer, or delegate can specify that its parameters are required or that they are optional. Any call must provide arguments for all required parameters, but can omit arguments for optional parameters.

As said by MSDN, a Optional Argument,

 - We can omit the argument in the call if that argument is an Optional
   Argument
 - Every Optional Argument has its own default value
 - It will take default value if we do not supply the value
 - A default value of a Optional Argument must be a
   - Constant expression.
   - Must be a value type such as enum or struct.
   - Must be an expression of the form default(valueType)
 - It must be set at the end of parameter list

## Optional Arguments
Consider preceding is our function definition with optional arguments.

    private static double FindAreaWithOptional(int length, int width=56)
           {
               try
               {
                   return (length * width);
               }
               catch (Exception)
               {
                   throw new NotImplementedException();
               }
           }

Here we have set the value for width as optional and gave value as 56. If you note, the IntelliSense itself shows you the optional argument as shown in the below image.

[![enter image description here][1]][1]

    Console.WriteLine("Area with Optional Argument : ");
    area = FindAreaWithOptional(120);
    Console.WriteLine(area);
    Console.Read();

Note that we did not get any error while compiling and it will give you an output as follows.

[![enter image description here][2]][2]



**Using Optional Attribute.**

Another way of implementing the optional argument is by using the `[Optional]` keyword. If you do not pass the value for the optional argument, the default value of that datatype is assigned to that argument. The `Optional` keyword is present in “Runtime.InteropServices” namespace.

    using System.Runtime.InteropServices;  
    private static double FindAreaWithOptional(int length, [Optional]int width)
       {
           try
           {
               return (length * width);
           }
           catch (Exception)
           {
               throw new NotImplementedException();
           }
       } 

    area = FindAreaWithOptional(120);  //area=0
And when we call the function, we get 0 because the second argument is not passed and the default value of int is 0 and so the product is 0.
    


  [1]: http://i.stack.imgur.com/Uaszw.png
  [2]: http://i.stack.imgur.com/3BWQA.png

## Named Arguments
Consider following is our function call.

    FindArea(120, 56);
In this our first argument is length (ie 120) and second argument is width (ie 56). And we are calculating the area by that function. And following is the function definition.

    private static double FindArea(int length, int width)
           {
               try
               {
                   return (length* width);
               }
               catch (Exception)
               {
                   throw new NotImplementedException();
               }
           }

So in the first function call, we just passed the arguments by its position. Right?

    double area;
    Console.WriteLine("Area with positioned argument is: ");
    area = FindArea(120, 56);
    Console.WriteLine(area);
    Console.Read();
If you run this, you will get an output as follows.

[![enter image description here][1]][1]

Now here it comes the features of a named arguments. Please see the preceding function call.


    Console.WriteLine("Area with Named argument is: ");
    area = FindArea(length: 120, width: 56);
    Console.WriteLine(area);
    Console.Read();

Here we are giving the named arguments in the method call.

    area = FindArea(length: 120, width: 56);
Now if you run this program, you will get the same result. We can give the names vice versa in the method call if we are using the named arguments. 

    Console.WriteLine("Area with Named argument vice versa is: ");
    area = FindArea(width: 120, length: 56);
    Console.WriteLine(area);
    Console.Read();

One of the important use of a named argument is, when you use this in your program it improves the readability of your code. It simply says what your argument is meant to be, or what it is?.

You can give the positional arguments too. That means, a combination of both positional argument and named argument.

    Console.WriteLine("Area with Named argument Positional Argument : ");
                area = FindArea(120, width: 56);
                Console.WriteLine(area);
                Console.Read();

In the above example we passed 120 as the length and 56 as a named argument for the parameter width.

There are some limitations too. We will discuss the limitation of a named arguments now.

**Limitation of using a Named Argument**

Named argument specification must appear after all fixed arguments have been specified.

If you use a named argument before a fixed argument you will get a compile time error as follows.

[![enter image description here][2]][2]

Named argument specification must appear after all fixed arguments have been specified


  [1]: http://i.stack.imgur.com/aCYyR.png
  [2]: http://i.stack.imgur.com/n8z4Y.png

