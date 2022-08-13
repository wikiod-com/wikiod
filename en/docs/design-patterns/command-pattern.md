---
title: "Command pattern"
slug: "command-pattern"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Command pattern example in Java
[wikipedia][1] definition:

> Command pattern is a behavioral design pattern in which an object is used to encapsulate all information needed to perform an action or trigger an event at a later time


UML diagram from [dofactory][2]:

[![enter image description here][3]][3]

Basic components and workflow:

1. `Command` declares an interface for abstract commands like `execute()`
2. `Receiver` knows how to execute a particular command
3. `Invoker` holds `ConcreteCommand`, which has to be executed
4. `Client` creates `ConcreteCommand` and assign `Receiver`
5. `ConcreteCommand` defines binding between `Command` and `Receiver`

In this way, Command pattern decouples **Sender** (Client)  from **Receiver** through **Invoker**. **Invoker** has complete knowledge of which **Command** to be executed and **Command** knows which **Receiver** to be invoked to execute a particular operation. 

Code snippet:
<!-- language: java -->

    interface Command {
        void execute();
    }
    class Receiver {
        public void switchOn(){
            System.out.println("Switch on from:"+this.getClass().getSimpleName());
        }
    }
    class OnCommand implements Command{
        private Receiver receiver;
    
        public OnCommand(Receiver receiver){
            this.receiver = receiver;
        }
        public void execute(){
            receiver.switchOn();
        }
    }
    class Invoker {
        private Command command;
      
        public Invoker(Command command){
            this.command = command;
        }
        public void execute(){
            this.command.execute();
        }
    }
    
    class TV extends Receiver{
        
        public String toString(){
            return this.getClass().getSimpleName();
        }
    }
    class DVDPlayer extends Receiver{
        
        public String toString(){
            return this.getClass().getSimpleName();
        }
    }
    
    public class CommandDemoEx{
        public static void main(String args[]){
            // On command for TV with same invoker 
            Receiver receiver = new TV();
            Command onCommand = new OnCommand(receiver);
            Invoker invoker = new Invoker(onCommand);
            invoker.execute();
            
            // On command for DVDPlayer with same invoker 
            receiver = new DVDPlayer();
            onCommand = new OnCommand(receiver);
            invoker = new Invoker(onCommand);
            invoker.execute();            
        }
    }

output:

    Switch on from:TV
    Switch on from:DVDPlayer

Explanation:

In this example,

1. **Command** interface defines `execute()` method. 
2. **OnCommand** is **ConcreteCommand**, which implements `execute()` method.
3. **Receiver** is the base class.
4. **TV** and **DVDPlayer** are two types of **Receivers**, which are passed to ConcreteCommand like OnCommand.
5. **Invoker** contains **Command**. It's the key to de-couple Sender from **Receiver**.
6. **Invoker** receives **OnCommand** -> which calls **Receiver** (TV) to execute this command.

By using Invoker, you can switch on TV and DVDPlayer. If you extend this program, you switch off both TV and DVDPlayer too.

**Key use cases:**

1. To implement callback mechanism
2. To implement undo and redo functionality
3. To Maintain a history of commands


  [1]: https://en.wikipedia.org/wiki/Command_pattern
  [2]: http://www.dofactory.com/net/command-design-pattern
  [3]: http://i.stack.imgur.com/0JiTY.gif

