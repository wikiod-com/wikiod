---
title: "Handling exceptions"
slug: "handling-exceptions"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

**Further reading**

More about FaultException: [MSDN FaultException][1]


  [1]: https://msdn.microsoft.com/en-us/library/system.servicemodel.faultexception(v=vs.110).aspx

## Using a custom error logging framework
It is sometimes useful to integrate a custom error logging framework to ensure all exceptions are logged.
  

    [ServiceContract]
    [ErrorHandler]
    public interface IMyService
    {
           
    }

    [AttributeUsage(AttributeTargets.Interface)]
    public class CustomErrorHandler : Attribute, IContractBehavior, IErrorHandler
    {
        public bool HandleError(Exception error)
        {
            return false;
        }

        public void ProvideFault(Exception error, MessageVersion version, ref Message fault)
        {
            if (error == null)
            {
                return;
            }

            //my custom logging framework
        }

        public void ApplyDispatchBehavior(ContractDescription contractDescription, ServiceEndpoint endpoint, DispatchRuntime dispatchRuntime)
        {
            dispatchRuntime.ChannelDispatcher.ErrorHandlers.Add(this);
        }

        public void ApplyClientBehavior(ContractDescription contractDescription, ServiceEndpoint endpoint,
            ClientRuntime clientRuntime)
        {

        }

        public void AddBindingParameters(ContractDescription contractDescription, ServiceEndpoint endpoint,
            BindingParameterCollection bindingParameters)
        {

        }

        public void Validate(ContractDescription contractDescription, ServiceEndpoint endpoint)
        {

        }
    }

## Showing additional information when an exception occurs
It's important to handle exceptions in your service.
When developing the service, you can set the WCF to provide more detailed information, adding this tag to configuration file, usually Web.config:

     <serviceDebug includeExceptionDetailInFaults="true"/>

This tag must be placed within the serviceBehavior tag, usually like this:

    <system.serviceModel>
        <behaviors>
          <serviceBehaviors>
            <behavior>
              <serviceDebug includeExceptionDetailInFaults="true"/>
            </behavior>
          </serviceBehaviors>
        </behaviors>
    </system.serviceModel> 

Example of detailed infomation:

> Server stack trace:     em
> System.ServiceModel.Channels.ServiceChannel.HandleReply(ProxyOperationRuntime
> operation, ProxyRpc& rpc)    em
> System.ServiceModel.Channels.ServiceChannel.Call(String action,
> Boolean oneway, ProxyOperationRuntime operation, Object[] ins,
> Object[] outs, TimeSpan timeout)    em
> System.ServiceModel.Channels.ServiceChannelProxy.InvokeService(IMethodCallMessage
> methodCall, ProxyOperationRuntime operation)    em
> System.ServiceModel.Channels.ServiceChannelProxy.Invoke(IMessage
> message)
> 
> Exception rethrown at [0]:     em
> System.Runtime.Remoting.Proxies.RealProxy.HandleReturnMessage(IMessage
> reqMsg, IMessage retMsg)    em
> System.Runtime.Remoting.Proxies.RealProxy.PrivateInvoke(MessageData&
> msgData, Int32 type)    em
> IMyService.GetDataOperation(RequestObterBeneficiario
> request)    em
> MyServiceClient.GetDataOpration(RequestData
> request)



This will return to the client detailed information. **During development** this can help out, but when your service **goes to production**, you will no longer keep this because your service can send sensitive data, like your database name or configuration.


## Throwing a user-friendly message with FaultException
You can handle exceptions and throw a most friendly message like 'Unavailable service' throwing a FaultException:


    try
    {
        // your service logic here
    }
    catch (Exception ex)
    {
        // You can do something here, like log the original exception
        throw new FaultException("There was a problem processing your request");
    }


In your client, you can easily get the message:

    try
    {
        // call the service
    }
    catch (FaultException faultEx)
    {
       var errorMessage = faultEx.Message;
       // do something with error message
    }

You can distinguish the handled exception from other exceptions, like a network error, adding other catches to your code:

    try
    {
        // call the service
    }
    catch (FaultException faultEx)
    {
       var errorMessage = faultEx.Message;
       // do something with error message
    }
    catch (CommunicationException commProblem)
    {
        // Handle the communication error, like trying another endpoint service or logging
    }

## Throwing  a FaultException with a Fault Code
The FaultException can also includes a **FaultCode**, that is a string data you can use to pass additional information, so the client can be able to distinguish different exceptions:


    try
    {
        // your service logic here
    }
    catch (Exception ex)
    {
       throw new FaultException("There was a problem processing your request",
          new FaultCode(("01"));
    }


Getting the FaultCode:

    try
    {
        // call the service
    }
    catch (FaultException faultEx)
    {
       switch (faultEx.Code.Name)
       {
        case "01":
           // do something
           break;
        case "02":
            // do another something
            break
       }
    }

## Decoupling ErrorHandlerAttribute registering from the service implementation
To decouple and reuse the same error logging code for **all** your services you need two boilerplate classes and tuck them away in a library somewhere. 

ErrorhandlerAttribute implementing IServiceBehavior.
FaultErrorhandler implementing IErrorhandler which logs all the exceptions.

    [AttributeUsage(AttributeTargets.Class)]
    public class ErrorHandlerAttribute : Attribute, IServiceBehavior
    {
        Type mErrorType;
        public ErrorHandlerAttribute(Type t)
        {
            if (!typeof(IErrorHandler).IsAssignableFrom(t))
                throw new ArgumentException("Type passed to ErrorHandlerAttribute constructor must inherit from IErrorHandler");
            mErrorType = t;
        }
    
        public void AddBindingParameters(ServiceDescription serviceDescription, ServiceHostBase serviceHostBase, Collection<ServiceEndpoint> endpoints, BindingParameterCollection bindingParameters)
        {
        }
    
        public void ApplyDispatchBehavior(ServiceDescription serviceDescription, ServiceHostBase serviceHostBase)
        {
            foreach (ChannelDispatcher dispatcher in serviceHostBase.ChannelDispatchers)
            {
                dispatcher.ErrorHandlers.Add((IErrorHandler)Activator.CreateInstance(mErrorType));
            }
        }
    
        public void Validate(ServiceDescription serviceDescription, ServiceHostBase serviceHostBase)
        {
        }
    }
    

    class FaultErrorHandler : IErrorHandler
    {
        public bool HandleError(Exception error)
        {
            // LOG ERROR
            return false; // false so the session gets aborted
        }
    
        public void ProvideFault(Exception error, MessageVersion version, ref Message fault)
        {
        }
    }

Then in your service implementation add the ErrorHandler Attribute passing in the Type instance of FaultErrorHandler. ErrorHandler will construct an instance from that type on which HandleError is called.

    [ServiceBehavior]
    [ErrorHandler(typeof(FaultErrorHandler))]
    public class MyService : IMyService
    {
    }

