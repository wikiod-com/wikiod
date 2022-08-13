---
title: "Interface Segregation Principle (ISP)"
slug: "interface-segregation-principle-isp"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

The principle states that no client should be forced to depend on methods that it doesn't use. A client should never be forced to implement an interface that it doesn't use or client shouldn't be forced to depend on methods that they don't use.

## Interface Segregation Principle C#
Here we give an example of **ISP** violation and then refactor that violation. Without talking unnecessary things let's jump into the code.

ISP violation :

    public interface IMessage{
     IList<string> ToAddress {get; set;}
     IList<string> BccAddresses {get; set;}
     string MessageBody {get; set;}
     string Subject {get; set;}
     bool Send();
    }
     
    public class SmtpMessage : IMessage{
     public IList<string> ToAddress {get; set;}
     public IList<string> BccAddresses {get; set;}
     public string MessageBody {get; set;}
     public string Subject {get; set;}
     public bool Send(){
      // Code for sending E-mail.
     }
    }
     
    public class SmsMessage : IMessage{
     public IList<string> ToAddress {get; set;}
     public IList<string> BccAddresses {
      get { throw new NonImplementedException(); }
      set { throw new NonImplementedException(); } 
     }
     public string MessageBody {get; set;}
     public string Subject {
      get { throw new NonImplementedException(); }
      set { throw new NonImplementedException(); } 
     }
     public bool Send(){
      // Code for sending SMS.
     }
    }
In the **SmsMessage** we don't need **BccAddresses and Subject**, but we forced to implement it because of **IMessage** interface . So it's violate the ISP principle.

Remove violation according to *ISP*:

    public interface IMessage{
     bool Send(IList<string> toAddress, string messageBody);
    }
     
    public interface IEmailMessage : IMessage{
     string Subject {get; set;}
     IList<string> BccAddresses {get; set;}
    }
     
    public class SmtpMessage : IEmailMessage{
     public IList<string> BccAddresses {get; set;}
     public string Subject {get; set;}
     public bool Send (IList<string> toAddress, string messageBody){
      // Code for sending E-mail.
     }
    }
     
    public class SmsMessage : IMessage{
     public bool Send (IList<string> toAddress, string messageBody){
      // Code for sending SMS.
     }
    }
**SmsMessage** need only *toAddress* and *messageBody*, so now we can use **IMessage** interface to avoid unnecessary implementations.

