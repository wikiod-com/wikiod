---
title: "Task"
slug: "task"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
- task Task_Name;
- task Task_Name is Entries end;
- task body Task_Name is Declarations begin Code end;

## One simple task
    with Ada.Text_IO; use Ada.Text_IO;
    
    procedure Main is
       task My_Task;
       task body My_Task is
       begin
          Put_Line ("Hello from My_Task");
       end;
    begin
       Put_Line ("Hello from Main");
    end;

# Result
The order of `Put_Line` can vary.

    Hello from My_Task
    Hello from Main

## One simple task and one loop
    with Ada.Text_IO; use Ada.Text_IO;
    
    procedure Main is
       task My_Task;
       task body My_Task is
       begin
          for I in 1 .. 4 loop
             Put_Line ("Hello from My_Task");
          end loop;
       end;
    begin
       Put_Line ("Hello from Main");
    end;

# Result
The order of `Put_Line` can vary.

    Hello from My_Task
    Hello from Main
    Hello from My_Task
    Hello from My_Task
    Hello from My_Task

## One simple task and two loops
    with Ada.Text_IO; use Ada.Text_IO;
    
    procedure Main is
       task My_Task;
       task body My_Task is
       begin
          for I in 1 .. 4 loop
             Put_Line ("Hello from My_Task");
          end loop;
       end;
    begin
       for I in 1 .. 4 loop
          Put_Line ("Hello from Main");
       end loop;
    end;

# Result
The order of `Put_Line` can vary.

    Hello from My_Task
    Hello from My_Task
    Hello from Main
    Hello from My_Task
    Hello from Main
    Hello from My_Task
    Hello from Main
    Hello from Main

## Two simple task and two loops
    with Ada.Text_IO; use Ada.Text_IO;
    
    procedure Main is
       task My_Task_1;
       task My_Task_2;
       
       task body My_Task_1 is
       begin
          for I in 1 .. 4 loop
             Put_Line ("Hello from My_Task_1");
          end loop;
       end;
       
       task body My_Task_2 is
       begin
          for I in 1 .. 4 loop
             Put_Line ("Hello from My_Task_2");
          end loop;
       end;
    begin
       null;
    end;

# Result
The order of `Put_Line` can vary.

    Hello from My_Task_1
    Hello from My_Task_1
    Hello from My_Task_2
    Hello from My_Task_1
    Hello from My_Task_2
    Hello from My_Task_1
    Hello from My_Task_2
    Hello from My_Task_2

## A task that increment a number after entry
The user can call `Incrementor.Increment` `K` number of times by pressing a key within `'0' ..  '9'` and it's possible to call `Incrementor.Increment` faster than the `task Incrementor` can increment `I`.

    with Ada.Text_IO;
    with Ada.Integer_Text_IO;
    
    procedure Main is
       use Ada.Text_IO;
       task Incrementor is
          entry Increment;
       end;
       task body Incrementor is
          use Ada.Integer_Text_IO;
          I : Integer := 0;
       begin
          loop
             accept Increment;
             I := I + 1;
             Put (I, 0);
             delay 0.1;
          end loop;
       end;
       K : Character;
    begin
       loop
          Get_Immediate (K);
          if K in '0' .. '9' then
             for I in 1 .. Natural'Value (K & "") loop
                Incrementor.Increment;
             end loop;
          end if;
       end loop;
    end;

## Interrupt Handling
Interrupts are handled by a protected procedure with no parameters.

    ------------------------------------------------------------------
    -- Interrupt Counting Package --
    ------------------------------------------------------------------
    with Ada.Interrupts.Names; use Ada.Interrupts.Names;
    
    package Ctl_C_Handling is
    
       protected CTL_C_Handler is
          procedure Handle_Int with
            Interrupt_Handler,
            Attach_Handler => SIGINT;
          entry Wait_For_Int;
       private
          Pending_Int_Count : Natural := 0;
       end Ctl_C_Handler;
    
       task CTL_Reporter is
          entry Stop;
       end CTL_Reporter;
    
    end Ctl_C_Handling;

The package body shows how the protected procedure works. In this case a boolean is not used in the protected object because interrupts arrive faster than they are handled.
The task CTL_Reporter handles the received interrupts.

    with Ada.Text_IO; use Ada.Text_IO;
    with Ctl_C_Handling; use CTL_C_Handling;
    with Ada.Calendar; use Ada.Calendar;
    
    package body Ctl_C_Handling is
    
       -------------------
       -- CTL_C_Handler --
       -------------------
    
       protected body CTL_C_Handler is
    
          ----------------
          -- Handle_Int --
          ----------------
    
          procedure Handle_Int is
          begin
             Pending_Int_Count := Pending_Int_Count + 1;
          end Handle_Int;
    
          ------------------
          -- Wait_For_Int --
          ------------------
    
          entry Wait_For_Int when Pending_Int_Count > 0 is
          begin
             Pending_Int_Count := Pending_Int_Count - 1;
          end Wait_For_Int;
    
       end CTL_C_Handler;
    
       ------------------
       -- CTL_Reporter --
       ------------------
    
       task body CTL_Reporter is
          type Second_Bin is mod 10;
          type History is array(Second_Bin) of Natural;
    
          ---------------------
          -- Display_History --
          ---------------------
    
          procedure Display_History(Item : History) is
             Sum : Natural := 0;
          begin
             for I in Item'Range loop
                Put_Line("Second: " & Second_Bin'Image(I) & " :" & Natural'Image(Item(I)));
                Sum := Sum + Item(I);
             end loop;
             Put_Line("Total count: " & Natural'Image(Sum));
             New_Line(2);
          end Display_History;
    
          One_Second_Count : Natural := 0;
          Next_Slot : Second_Bin := 0;
          Next_Second : Time := Clock + 1.0;
          Ten_Second_History : History := (Others => 0);
    
       begin
          loop
             Select
                Accept Stop;
                exit;
             else
                select
                   CTL_C_Handler.Wait_For_Int;
                   One_Second_Count := One_Second_Count + 1;
                or
                   delay until Next_Second;
                   Next_Second := Next_Second + 1.0;
                   Ten_Second_History(Next_Slot) := One_Second_Count;
                   Display_History(Ten_Second_History);
                   Next_Slot := Next_Slot + 1;
                   One_Second_Count := 0;
                end Select;
             end Select;
          end loop;
       end CTL_Reporter;
    end Ctl_C_Handling;

An example main program to exercise this package is:

    ------------------------------------------------------------------
    -- Ada2012 Interrupt Handler Example --
    ------------------------------------------------------------------
    with Ada.Text_IO; use Ada.Text_IO;
    with Ctl_C_Handling; use CTL_C_Handling;
    
    procedure Interrupt01 is
    begin
       Delay 40.0;
       CTL_Reporter.Stop;
       Put_Line("Program ended.");
    end Interrupt01;



