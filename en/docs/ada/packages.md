---
title: "Packages"
slug: "packages"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
- with Package_Name_To_Include;
- package New_Package_Name renames Package_To_Rename;
- use Package_Name;
- package Parent_Name.Child_Name is

Package provides:

 - Code encapsulation 
 - Separate compilation
 - Hide procedures, functions, operators on private types

Similarities or analogous in other languages:

 - [C++ namespace][1]
 - [Java packages][2]


  [1]: https://www.wikiod.com/docs/c%2B%2B/495/namespaces#t=201610081130053983861
  [2]: https://www.wikiod.com/java

## More on Packages
In the [Hello World][1], you were introduced to the package `Ada.Text_IO`, and how to use it in order to perform I/O operations within your program. Packages can be further manipulated to do many different things. 

**Renaming**: To rename a package, you use the keyword `renames` in a package declaration, as such:

    package IO renames Ada.Text_IO;

Now, with the new name, you can use the same dotted notation for functions like `Put_Line` (i.e. `IO.Put_Line`), or you can just `use` it with `use IO`. Of course, saying `use IO` or `IO.Put_Line` will use the functions from the package `Ada.Text_IO`.

-----

**Visibility & Isolation**: In the *Hello World* example we included the Ada.Text_IO package using a `with` clause. But we also declared that we wanted to `use Ada.Text_IO` on the same line. The `use Ada.Text_IO` declaration could have been moved into the declarative part of the procedure:

    with Ada.Text_IO;
     
    procedure hello_world is
       use Ada.Text_IO;
    begin
       Put_Line ("Hello, world!");
    end hello_world;

In this version, the procedures, functions, and types of `Ada.Text_IO` are directly available inside the procedure. Outside the block in which use `Ada.Text_IO` is declared, we would have to use the dotted notation to invoke, for example:

    with Ada.Text_IO;
     
    procedure hello_world is
    begin
       Ada.Text_IO.Put ("Hello, ");       --  The Put function is not directly visible here
       declare
          use Ada.Text_IO;
       begin
          Put_Line ("world!");            --  But here Put_Line is, so no Ada.Text_IO. is needed
       end;
    end hello_world;

This enables us to isolate the use … declarations to where they are necessary.

 


  [1]: https://www.wikiod.com/ada/getting-started-with-ada#Hello World

## Parent-Child Relationship
As a way of subdividing Ada programs, packages may have so-called children. These can be packages, too.  A child package has a special privilege: it can see the declarations in the parent package's private part. One typical use of this special visibility is when forming a hierarchy of derived types in object oriented programming.

    package Orders is
       type Fruit is (Banana, Orange, Pear);
       type Money is delta 0.01 digits 6;
       
       type Bill is tagged private;
    
       procedure Add
         (Slip   : in out Bill;
          Kind   : in     Fruit;
          Amount : in     Natural);
       
       function How_Much (Slip : Bill) return Money;
       
       procedure Pay
         (Ordered : in out Bill;
          Giving  : in     Money);
       
    private
       type Bill is tagged record
          --  ...
          Sum : Money := 0.0;
       end record;
    end Orders;

Any Ada unit that is headed by `with Orders;` can declare objects of type `Bill` and then call operations `Add`, `How_Much`, and `Pay`. It does not, however, see the components of `Bill`, nor even of `Orders.Bill`, since the full type definition is hidden in the **private** part of `Orders`. The full definition is not hidden form child packages, though. This visibility facilitates type extension if needed. If a type is declared in the child package as derived from `Bill`, then this inheriting type can manipulate `Bill`'s components directly.

    package Orders.From_Home is
       type Address is new String (1 .. 120);
       
       type Ordered_By_Phone is new Bill with private;
       
       procedure Deliver
         (Ordered : in out Ordered_By_Phone;
          Place   : in     Address);
    
    private
       type Ordered_By_Phone is new Bill with
          record
             Delivered : Boolean := False;
             To        : Address;
          end record;
    end Orders.From_Home;

`Orders.From_Home` is a child package of `Orders`. Type `Ordered_By_Phone` is derived from `Bill` and includes its record component `Sum`.

