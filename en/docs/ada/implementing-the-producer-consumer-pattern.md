---
title: "Implementing the producer-consumer pattern"
slug: "implementing-the-producer-consumer-pattern"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

A demonstration of how the producer-consumer pattern is implemented in Ada.

## Syntax
- function Scalar'[Image][1] (Argument : Scalar'Base) return String;
- [task][2] Task_Name;
- [task][2] Task_Name is Entries end;
- [task][2] body Task_Name is Declarations begin Code end;
- entry Entry_Name;
- accept Entry_Name;
- exit;


  [1]: https://www.wikiod.com/ada/attribute-image
  [2]: https://www.wikiod.com/ada/task

The examples *should* all ensure proper task termination.

## Producer-Consumer pattern using the Ada Rendezvous mechanism
A synchronous producer-consumer solution ensures that the consumer reads every data item written by the producer exactly one time. Asynchronous solutions allow the consumer to sample the output of the producer. Either the consumer consumes the data faster than it is produced, or the consumer consumes the data slower than it is produced. Sampling allows the consumer to handle the currently available data. That data may be only a sampling of the data produced, or it may be already consumed data.

<!-- language: lang-ada -->

    ------------------------------------------------------------------
    -- synchronous PC using Rendezvous --
    ------------------------------------------------------------------
    with Ada.Text_IO; use Ada.Text_IO;
    
    procedure PC_Rendezvous is
       task Producer;
       task Consumer is
          entry Buf(Item : in Integer);
       end Consumer;
       task body Producer is
       begin
          for I in 1..10 loop
             Put_Line("Producer writing" & Integer'Image(I));
             Consumer.Buf(I);
          end loop;
       end Producer;
       task body Consumer is
          Temp : Integer;
       begin
          loop
             select
                accept Buf(Item : in Integer) do
                   temp := Item;
                end;
                Put_Line("Consumer read" & Integer'Image(Temp));
             or
                terminate;
             end select;
          end loop;
       end Consumer;
    
    begin
       null;
    end PC_Rendezvous;




## Producer-Consumer with a sampling consumer
This example uses the main procedure as the producer task. In Ada the main procedure always runs in a task separate from all other tasks in the program, [see minimal example][1].
<!-- language: lang-ada -->
    ------------------------------------------------------------------
    -- Sampling Consumer --
    ------------------------------------------------------------------
    with Ada.Text_IO; use Ada.Text_IO;
    
    procedure Sampling_PC is
       protected Buf is
          procedure Write(Item : in Integer);
          function Read return Integer;
          procedure Set_Done;
          function Get_Done return Boolean;
       private
          Value : Integer := Integer'First;
          Is_Done : Boolean := False;
       end Buf;
       protected body Buf is
          procedure Write(Item : in Integer) is
          begin
             Value := Item;
          end Write;
          function Read return Integer is
          begin
             return Value;
          end Read;
          procedure Set_Done is
          begin
             Is_Done := True;
          end Set_Done;
          function Get_Done return Boolean is
          begin
             return Is_Done;
          end Get_Done;
       end Buf;
    
       task Consumer;
       task body Consumer is
       begin
          while not Buf.Get_Done loop
              Put_Line("Consumer read" & Integer'Image(Buf.Read));
          end loop;
       end Consumer;
    
    begin
       for I in 1..10 loop
         Put_Line("Producer writing" & Integer'Image(I));
         Buf.Write(I);
       end loop;
       Buf.Set_Done;
    end Sampling_PC;


  [1]: https://www.wikiod.com/ada/task#One simple task and two loops

## Using a synchronized buffer
<!-- language: lang-ada -->

    with Ada.Containers.Synchronized_Queue_Interfaces;
    with Ada.Containers.Unbounded_Synchronized_Queues;
    with Ada.Text_IO;
    
    procedure Producer_Consumer_V1 is
       type Work_Item is range 1 .. 100;
    
       package Work_Item_Queue_Interfaces is
         new Ada.Containers.Synchronized_Queue_Interfaces
               (Element_Type => Work_Item);
    
       package Work_Item_Queues is
         new Ada.Containers.Unbounded_Synchronized_Queues
               (Queue_Interfaces => Work_Item_Queue_Interfaces);
    
       Queue : Work_Item_Queues.Queue;
    
       task type Producer;
       task type Consumer;
    
       Producers : array (1 .. 1)  of Producer;
       Consumers : array (1 .. 10) of Consumer;
    
       task body Producer is
       begin
          for Item in Work_Item loop
             Queue.Enqueue (New_Item => Item);
          end loop;
       end Producer;
    
       task body Consumer is
          Item : Work_Item;
       begin
          loop
             Queue.Dequeue (Element => Item);
             Ada.Text_IO.Put_Line (Work_Item'Image (Item));
          end loop;
       end Consumer;
    
    begin
       null;
    end Producer_Consumer_V1;

Notice that I've been lazy here: There is no proper termination of the consumer tasks, once all work items are consumed.

## Multiple Producers and Consumers Sharing the same buffer
This example shows multiple producers and consumers sharing the same buffer. Protected entries in Ada implement a queue to handle waiting tasks. The default queuing policy is First In First Out. 

<!-- language: lang-ada -->
    ------------------------------------------------------------------
    -- Multiple producers and consumers sharing the same buffer --
    ------------------------------------------------------------------
    with Ada.Text_IO; use Ada.Text_Io;
    
    procedure N_Prod_Con is
       protected Buffer is
          Entry Write(Item : in Integer);
          Entry Read(Item : Out Integer);
       private
          Value  : Integer := Integer'Last;
          Is_New : Boolean := False;
       end Buffer;
       protected body Buffer is
          Entry Write(Item : in Integer) when not Is_New is
          begin
             Value := Item;
             Is_New := True;
          end Write;
          Entry Read(Item : out Integer) when Is_New is
          begin
             Item := Value;
             Is_New := False;
          end Read;
       end Buffer;
    
       task type Producers(Id : Positive) is
          Entry Stop;
       end Producers;
       task body Producers is
          Num : Positive := 1;
       begin
          loop
             select
                accept Stop;
                exit;
             or
                delay 0.0001;
             end select;
             Put_Line("Producer" & Integer'Image(Id) & " writing" & Integer'Image(Num));
             Buffer.Write(Num);
             Num := Num + 1;
          end loop;
       end Producers;
    
       task type Consumers(Id : Positive) is
          Entry Stop;
       end Consumers;
    
       task body Consumers is
          Num : Integer;
       begin
          loop
             select
                accept stop;
                exit;
             or
                delay 0.0001;
             end select;
             Buffer.Read(Num);
             Put_Line("Consumer" & Integer'Image(ID) & " read" & Integer'Image(Num));
          end loop;
       end Consumers;
       P1 : Producers(1);
       P2 : Producers(2);
       P3 : Producers(3);
       C1 : Consumers(1);
       C2 : Consumers(2);
       C3 : Consumers(3);
    begin
       delay 0.2;
       P1.Stop;
       P2.Stop;
       P3.Stop;
       C1.Stop;
       C2.Stop;
       C3.Stop;
    end N_Prod_Con;


